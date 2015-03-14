# @(#)util/mkpath.awk	1.4 7/11/92 11:40:10
#
# mkpath.awk perform awk pre-processing of an input file
#
#    Copyright (C) 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992 Ronald S. Karr
#
# See the file COPYING, distributed with smail, for restriction
# and warranty information.
#
# Some awk's don't process paragraphs in the order that they
# appear in the file, so we are forced to make this program
# non-deterministic.

### initial setup
#
BEGIN {

#   clear line counts
    lineno = 0;
#   not in a block initially
    inblock = 0;
#   no errors so far
    errno = 0;
#   line modes
    mode_file = 1;
    mode_cmd = 2;
    mode_lit = 3;
    mode = 0;

#   preset shell commands
    gleem = "$GLEEM";
}

### per line debug and processing
#
{

#   note another line, echo it if -v
    ++lineno;
    if ( VERBOSE > 0 && NF > 0 ) {
	print PROG": line", lineno":", $0 | "cat 1>&2";
    }
}

### ignore blank lines
#
NF < 1 {
    next;
}

### pre-processing of lines with args
#
NF > 1 {

#   find string beyond the first field
    args = substr($0, index($0,$1)+length($1));	# remove field 1
    if ( $2 ~ /^`/ && $NF ~ /`$/ ) {
	mode = mode_cmd;				# cmd output
	# remove backquotes
	args = substr(args, index(args,$2)+1, length(args)-index(args,$2)-1);
    } else {
	if ( $2 ~ /^'/ && $NF ~ /'$/ ) {
	    mode = mode_lit;				# text literal
	} else {
	    mode = mode_file;				# data file
	}
        args = substr(args, index(args,$2));
    }
}

### map - store map commands into WORK
#
$1 == "map" {

    if ( NF <= 1 ) {
	print "echo", PROG, ": line", lineno":", $1, "no args 1>&2";
	errno = 1;
	if (ERR > 0) {
	    exit;		# END will be processed
	}
    } else {
	if ( inblock == 0 ) {
	    print "(";
	    inblock = 1;
	}
	if (mode == mode_cmd) {
	    print args, "|", gleem, "-p -c -f -";
	}
	if (mode == mode_lit) {
	    print "echo", args "|", gleem, "-p -c -f -";
	}
	if (mode == mode_file) {
	    print gleem, "-p -c -d $CWD -f /dev/null", args;
	}
    }
    next;
}

### safemap - store safe map commands into WORK
#
$1 == "safemap" {

    if ( NF <= 1 ) {
	print "echo", PROG, ": line", lineno":", $1, "no args 1>&2";
	errno = 11;
	if (ERR > 0) {
	    exit;		# END will be processed
	}
    } else {
	if ( inblock == 0 ) {
	    print "(";
	    inblock = 1;
	}
	if (mode == mode_cmd) {
	    print args, "|", gleem, "-s -F -p -c -f -";
	}
	if (mode == mode_lit) {
	    print "echo", args "|", gleem, "-s -F -p -c -f -";
	}
	if (mode == mode_file) {
	    print gleem, "-s -F -p -c -d $CWD -f /dev/null", args;
	}
    }
    next;
}

### delete - store delete commands into WORK
#
$1 == "delete" {

    if ( NF <= 1 ) {
	print "echo", PROG, ": line", lineno":", $1, "no args 1>&2";
	errno = 2;
	if (ERR > 0) {
	    exit;		# END will be processed
	}
    } else {
	if ( inblock == 0 ) {
	    print "(";
	    inblock = 1;
	}
	if (mode == mode_cmd) {
	    print args, "|", gleem, "-n delete -f -";
	}
	if (mode == mode_lit) {
	    print "echo", args "|", gleem, "-n delete -f -";
	}
	if (mode == mode_file) {
	    print gleem, "-p -n delete -d $CWD -f /dev/null", args;
	}
    }
    next;
}

### adjust - store adjust commands into WORK
#
$1 == "adjust" {

    if ( NF <= 1 ) {
	print "echo", PROG, ": line", lineno":", $1, "no args 1>&2";
	errno = 3;
	if (ERR > 0) {
	    exit;		# END will be processed
	}
    } else {
	if ( inblock == 0 ) {
	    print "(";
	    inblock = 1;
	}
	if (mode == mode_cmd) {
	    print args, "|", gleem, "-n adjust -f -";
	}
	if (mode == mode_lit) {
	    print "echo", args "|", gleem, "-n adjust -f -";
	}
	if (mode == mode_file) {
	    print gleem, "-p -n adjust -d $CWD -f /dev/null", args;
	}
    }
    next;
}

### dead - store dead commands into WORK
#
$1 == "dead" {

    if ( NF <= 1 ) {
	print "echo", PROG, ": line", lineno":", $1, "no args 1>&2";
	errno = 4;
	if (ERR > 0) {
	    exit;		# END will be processed
	}
    } else {
	if ( inblock == 0 ) {
	    print "(";
	    inblock = 1;
	}
	if (mode == mode_cmd) {
	    print args, "|", gleem, "-n dead -f -";
	}
	if (mode == mode_lit) {
	    print "echo", args "|", gleem, "-n dead -f -";
	}
	if (mode == mode_file) {
	    print gleem, "-p -n dead -d $CWD -f /dev/null", args;
	}
    }
    next;
}

### text - store commands to inject
#
$1 == "text" {

    if ( NF <= 1 ) {
	print "echo", PROG, ": line", lineno":", $1, "no args 1>&2";
	errno = 5;
	if (ERR > 0) {
	    exit;		# END will be processed
	}
    } else {
	if (mode == mode_cmd) {
	    print args;
	}
	if (mode == mode_lit) {
	    print "echo", args;
	}
	if (mode == mode_file) {
	    print "cat /dev/null", args;
	}
    }
    next;
}

### file - store file changing commands into WORK
#
$1 == "file" {

    if ( NF > 2 ) {
	print "echo", PROG, ": line", lineno":", $0, "too many args 1>&2";
	errno = 6;
	if (ERR > 0) {
	    exit;		# END will be processed
	}
    }
    if ( NF <= 1 ) {
	print "echo", PROG, ": line", lineno":", $1, "no args 1>&2";
	errno = 7;
	if (ERR > 0) {
	    exit;		# END will be processed
	}
    }
    if ( NF == 2 ) {
	if ( inblock == 0 ) {
	    print "(";
	    inblock = 1;
	}
	if ( $2 ~ /^\// ) {
	    print "echo \"file {", $2, "}\"";
	} else {
	    print "echo \"file { `pwd`/"$2, "}\"";
	}
    }
    next;
}

### cd - store cd directory changing commands into WORK
#
$1 == "cd" {

    if ( NF > 2 ) {
	print "echo", PROG, ": line", lineno":", $0, "too many args 1>&2";
	errno = 8;
	if (ERR > 0) {
	    exit;		# END will be processed
	}
    }
#   cd by itself refers to the dir from where mkpath was started
    if ( NF == 1 ) {
	print "cd $PWD";
	print "CWD=$PWD";
    }
    if ( NF == 2 ) {
#	cd with a '-' refers to the dir that the input file is in
	if ( $2 == "-" ) {
	    print "cd $CD";
	    print "CWD=$CD";
#	otherwise cd to the arg
	} else {
	    print "cd", $2;
	    print "CWD="$2;
	}
    }
    next;
}

### sh - store a SHELL command into a file
#
$1 == "sh" {

    if ( NF <= 1 ) {
	print "echo", PROG, ": line", lineno":", $1, "no args 1>&2";
	errno = 9;
	if (ERR > 0) {
	    exit;		# END will be processed
	}
    } else {
	print args;
    }
    next;
}

### pathalias - end a block with pathalias(8) plus optional args
#
$1 == "pathalias" {

    if ( inblock > 0 ) {
	if ( NF > 1 ) {
	    print ") | eval $PATHALIAS", args;
	} else {
	    print ") | eval $PATHALIAS";
	}
    }
    inblock = 0;
    next;
}

### pathsort - end a block with pathalias(8) plus post-processing and args
#
$1 == "pathsort" {

    if ( inblock > 0 ) {
	if ( NF > 1 ) {
	    print ") | eval $PATHSORT", args;
	} else {
	    print ") | eval $PATHSORT";
	}
    }
    inblock = 0;
    next;
}

### ERROR - catch unknown lines, known lines resulted in 'next' skipping ERROR
#
{
    errno = 10;
    print PROG": line", lineno": unknown command:", $0 | "cat 1>&2";
#   if the -e flag was igven to mkpath, insert a exit right now
    if (ERR > 0) {
	exit;		# END will be processed
    }
}

### END - the end of a config file has an implied pathsort if it needs one
#
END {

    if ( inblock > 0 ) {
	print ") | eval $PATHSORT";
    }
    print "exit", errno;
}
