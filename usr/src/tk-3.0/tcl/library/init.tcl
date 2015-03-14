# init.tcl --
#
# Default system startup file for Tcl-based applications.  Defines
# "unknown" procedure and auto-load facilities.
#
# $Header: /user6/ouster/tcl/scripts/RCS/init.tcl,v 1.12 92/10/22 12:04:50 ouster Exp $ SPRITE (Berkeley)
#
# Copyright 1991-1992 Regents of the University of California
# Permission to use, copy, modify, and distribute this
# software and its documentation for any purpose and without
# fee is hereby granted, provided that this copyright
# notice appears in all copies.  The University of California
# makes no representations about the suitability of this
# software for any purpose.  It is provided "as is" without
# express or implied warranty.
#

# unknown:
# Invoked when a Tcl command is invoked that doesn't exist in the
# interpreter:
#
#	1. See if the autoload facility can locate the command in a
#	   Tcl script file.  If so, load it and execute it.
#	2. See if the command exists as an executable UNIX program.
#	   If so, "exec" the command.
#	3. If the command was invoked at top-level:
#	    (a) see if the command requests csh-like history substitution
#		in one of the common forms !!, !<number>, or ^old^new.  If
#		so, emulate csh's history substitution.
#	    (b) see if the command is a unique abbreviation for another
#		command.  If so, invoke the command.

proc unknown args {
    global auto_noexec auto_noload env unknown_pending;

    set name [lindex $args 0]
    if ![info exists auto_noload] {
	#
	# Make sure we're not trying to load the same proc twice.
	#
	if [info exists unknown_pending($name)] {
	    unset unknown_pending($name)
	    if ![array size unknown_pending] {
		unset unknown_pending
	    }
	    error "self-referential recursion in \"unknown\" for command \"$name\"";
	}
	set unknown_pending($name) pending;
	set ret [auto_load $name];
	unset unknown_pending($name);
	if ![array size unknown_pending] {
	    unset unknown_pending
	}
	if $ret {
	    return [uplevel $args]
	}
    }
    if ![info exists auto_noexec] {
	if [auto_execok $name] {
	    return [uplevel exec $args]
	}
    }
    if {([info level] == 1) && ([info script] == "")} {
	if {$name == "!!"} {
	    return [uplevel {history redo}]
	}
	if [regexp {^!(.+)$} $name dummy event] {
	    return [uplevel history redo $event]
	}
	if [regexp {^\^(.*)\^(.*)^?$} $name dummy old new] {
	    return [uplevel history substitute $old $new]
	}
	set cmds [info commands $name*]
	if {[llength $cmds] == 1} {
	    return [uplevel [lreplace $args 0 0 $cmds]]
	}
	if {[llength $cmds] != 0} {
	    if {$name == ""} {
		error "empty command name \"\""
	    } else {
		error "ambiguous command name \"$name\": [lsort $cmds]"
	    }
	}
    }
    error "invalid command name \"$name\""
}

# auto_load:
# Checks a collection of library directories to see if a procedure
# is defined in one of them.  If so, it sources the appropriate
# library file to create the procedure.  Returns 1 if it successfully
# loaded the procedure, 0 otherwise.

proc auto_load cmd {
    global auto_index auto_oldpath auto_path env

    if [info exists auto_index($cmd)] {
	uplevel #0 source $auto_index($cmd)
	return 1
    }
    if [catch {set path $auto_path}] {
	if [catch {set path $env(TCLLIBPATH)}] {
	    if [catch {set path [info library]}] {
		return 0
	    }
	}
    }
    if [info exists auto_oldpath] {
	if {$auto_oldpath == $path} {
	    return 0
	}
    }
    set auto_oldpath $path
    catch {unset auto_index}
    foreach dir $path {
	set f ""
	catch {
	    set f [open $dir/tclIndex]
	    if {[gets $f] != "# Tcl autoload index file: each line identifies a Tcl"} {
		puts stdout "Bad id line in file $dir/tclIndex"
		error done
	    }
	    while {[gets $f line] >= 0} {
		if {([string index $line 0] == "#") || ([llength $line] != 2)} {
		    continue
		}
		set name [lindex $line 0]
		if {![info exists auto_index($name)]} {
		    set auto_index($name) $dir/[lindex $line 1]
		}
	    }
	}
	if {$f != ""} {
	    close $f
	}
    }
    if [info exists auto_index($cmd)] {
	uplevel #0 source $auto_index($cmd)
	return 1
    }
    return 0
}

# auto_execok:
# Returns 1 if there's an executable in the current path for the
# given name, 0 otherwise.  Builds an associative array auto_execs
# that caches information about previous checks, for speed.

proc auto_execok name {
    global auto_execs env

    if [info exists auto_execs($name)] {
	return $auto_execs($name)
    }
    set auto_execs($name) 0
    foreach dir [split $env(PATH) :] {
	if {[file executable $dir/$name] && ![file isdirectory $dir/$name]} {
	    set auto_execs($name) 1
	    return 1
	}
    }
    return 0
}

# auto_reset:
# Destroy all cached information for auto-loading and auto-execution,
# so that the information gets recomputed the next time it's needed.
# Also delete any procedures that are listed in the auto-load index
# except those related to auto-loading.

proc auto_reset {} {
    global auto_execs auto_index auto_oldpath
    foreach p [info procs] {
	if {[info exists auto_index($p)] && ($p != "unknown")
		&& ![string match auto_* $p]} {
	    rename $p {}
	}
    }
    catch {unset auto_execs}
    catch {unset auto_index}
    catch {unset auto_oldpath}
}

# auto_mkindex:
# Regenerate a tclIndex file from Tcl source files.  Takes two arguments:
# the name of the directory in which the tclIndex file is to be placed,
# and a glob pattern to use in that directory to locate all of the relevant
# files.

proc auto_mkindex {dir files} {
    global errorCode errorInfo
    set oldDir [pwd]
    cd $dir
    set dir [pwd]
    append index "# Tcl autoload index file: each line identifies a Tcl\n"
    append index "# procedure and the file where that procedure is\n"
    append index "# defined.  Generated by the \"auto_mkindex\" command.\n"
    append index "\n"
    foreach file [glob $files] {
	set f ""
	set error [catch {
	    set f [open $file]
	    while {[gets $f line] >= 0} {
		if [regexp {^proc[ 	]+([^ 	]*)} $line match procName] {
		    append index "[list $procName $file]\n"
		}
	    }
	    close $f
	} msg]
	if $error {
	    set code $errorCode
	    set info $errorInfo
	    catch [close $f]
	    cd $oldDir
	    error $msg $info $code
	}
    }
    set f [open tclIndex w]
    puts $f $index nonewline
    close $f
    cd $oldDir
}
