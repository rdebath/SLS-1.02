# mkTextSearch w
#
# Create a top-level window containing a text widget that allows you
# to load a file and highlight all instances of a given string.
#
# Arguments:
#    w -	Name to use for new top-level window.

proc mkTextSearch {{w .search}} {
    catch {destroy $w}
    toplevel $w
    dpos $w
    wm title $w "Text Demonstration - Search and Highlight"
    wm iconname $w "Text Search"

    frame $w.file
    label $w.file.label -text "File name:" -width 13 -anchor w
    entry $w.file.entry -width 40 -relief sunken -bd 2 -textvariable fileName
    button $w.file.button -text "Load File" \
	    -command "TextLoadFile $w.t \$fileName"
    pack append $w.file $w.file.label left $w.file.entry left \
	    $w.file.button {left pady 10 padx 20}
    bind $w.file.entry <Return> "
	TextLoadFile $w.t \$fileName
	focus $w.string.entry
    "

    frame $w.string
    label $w.string.label -text "Search string:" -width 13 -anchor w
    entry $w.string.entry -width 40 -relief sunken -bd 2 \
	    -textvariable searchString
    button $w.string.button -text "Highlight" \
	    -command "TextSearch $w.t \$searchString search"
    pack append $w.string $w.string.label left $w.string.entry left \
	    $w.string.button  {left pady 10 padx 20}
    bind $w.string.entry <Return> "TextSearch $w.t \$searchString search"

    button $w.ok -text OK -command "destroy $w"
    text $w.t -relief raised -bd 2 -yscrollcommand "$w.s set" -setgrid true
    scrollbar $w.s -relief flat -command "$w.t yview"
    pack append $w $w.file {top fill} $w.string {top fill} \
	    $w.ok {bottom fillx} $w.s {right filly} $w.t {expand fill}

    # Set up display styles for text highlighting.

    if {[tk colormodel $w] == "color"} {
	TextToggle "$w.t tag configure search -background \
		SeaGreen4 -foreground white" 800 "$w.t tag configure \
		search -background {} -foreground {}" 200
    } else {
	TextToggle "$w.t tag configure search -background \
		black -foreground white" 800 "$w.t tag configure \
		search -background {} -foreground {}" 200
    }
    $w.t insert 0.0 {\
This window demonstrates how to use the tagging facilities in text
widgets to implement a searching mechanism.  First, type a file name
in the top entry, then type <Return> or click on "Load File".  Then
type a string in the lower entry and type <Return> or click on
"Load File".  This will cause all of the instances of the string to
be tagged with the tag "search", and it will arrange for the tag's
display attributes to change to make all of the strings blink.
}
    $w.t mark set insert 0.0
    bind $w <Any-Enter> "focus $w.file.entry"
}
set fileName ""
set searchString ""

# The utility procedure below loads a file into a text widget,
# discarding the previous contents of the widget. Tags for the
# old widget are not affected, however.
# Arguments:
#
# w -		The window into which to load the file.  Must be a
#		text widget.
# file -	The name of the file to load.  Must be readable.

proc TextLoadFile {w file} {
    set f [open $file]
    $w delete 1.0 end
    while {![eof $f]} {
	$w insert end [read $f 10000]
    }
    close $f
}

# The utility procedure below searches for all instances of a
# given string in a text widget and applies a given tag to each
# instance found.
# Arguments:
#
# w -		The window in which to search.  Must be a text widget.
# string -	The string to search for.  The search is done using
#		exact matching only;  no special characters.
# tag -		Tag to apply to each instance of a matching string.

proc TextSearch {w string tag} {
    $w tag remove search 0.0 end
    scan [$w index end] %d numLines
    set l [string length $string]
    for {set i 1} {$i <= $numLines} {incr i} {
	if {[string first $string [$w get $i.0 $i.1000]] == -1} {
	    continue
	}
	set line [$w get $i.0 $i.1000]
	set offset 0
	while 1 {
	    set index [string first $string $line]
	    if {$index < 0} {
		break
	    }
	    incr offset $index
	    $w tag add $tag $i.[expr $offset] $i.[expr $offset+$l]
	    incr offset $l
	    set line [string range $line $offset 1000]
	}
    }
}

# The procedure below is invoked repeatedly to invoke two commands
# at periodic intervals.  It normally reschedules itself after each
# execution but if an error occurs (e.g. because the window was
# deleted) then it doesn't reschedule itself.
# Arguments:
#
# cmd1 -	Command to execute when procedure is called.
# sleep1 -	Ms to sleep after executing cmd1 before executing cmd2.
# cmd2 -	Command to execute in the *next* invocation of this
#		procedure.
# sleep2 -	Ms to sleep after executing cmd2 before executing cmd1 again.

proc TextToggle {cmd1 sleep1 cmd2 sleep2} {
    catch {
	eval $cmd1
	after $sleep1 [list TextToggle $cmd2 $sleep2 $cmd1 $sleep1]
    }
}
