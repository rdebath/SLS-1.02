.TH XDM 1 "Release 4" "X Version 11"
.SH NAME
xdm \- X Display Manager
.SH SYNOPSIS
.B xdm
[-config \fIconfiguration_file\fP]
[-daemon]
[-debug \fIdebug_level\fP]
[-error \fIerror_log_file\fP]
[-nodaemon]
[-resources \fIresource_file\fP]
[-server \fIserver_entry\fP]
[-session \fIsession_program\fP]
[-xrm \fIresource_specification\fP]
.SH DESCRIPTION
.PP
.I Xdm
manages a collection of X displays, both local and possibly remote \(em the
emergence of X terminals guided the design of several parts of this system,
along with the development of the X Consortium standard XDMCP, the
\fIX Display Manager Control Protocol\fP).
It is designed to provide services similar to that provided by init, getty
and login on character terminals:  prompting for login/password,
authenticating the user and running a ``session''.
.PP
A ``session'' is defined by the lifetime of a particular process; in the
traditional character-based terminal world, it is the user's login shell
process.  In the
.I xdm
context, it is an arbitrary session manager.  This is because in a windowing
environment, a user's login shell process would not necessarily have any
terminal-like interface with which to connect.
.PP
Until real session managers become widely available, the typical
.I xdm
substitute would be either a window manager with an exit option, or a
terminal emulator running a shell - with the condition that
the lifetime of the terminal emulator is the lifetime of the shell process
that it is running - 
thus degenerating the X session to an emulation of the
character-based terminal session.
.PP
When the session is terminated,
.I xdm
resets the X server and (optionally) restarts the whole process.
.PP
Because
.I xdm
provides the first interface that users will see, it is designed to be
simple to use and easy to customize to the needs of a particular site.
.I Xdm
has many options, most of which have reasonable defaults.  Browse through the
various sections, picking and choosing the things you want to change.  Pay
particular attention to the \fBXsession\fP section, which will describe how to
set up the style of session desired.
.PP
.SH OPTIONS
.PP
First, note that all of these options, except \fB-config\fP,
specify values which can also be specified in the configuration file
as resources.
.IP "\fB-config\fP \fIconfiguration_file\fP"
Specifies a resource file which specifies the remaining configuration
parameters.  If no file is specified and the file
\fI/usr/lib/X11/xdm/xdm-config\fP exists,
.I xdm
will use it.
.IP "\fB-daemon\fP"
Specifies ``true'' as the value for the \fBDisplayManager.daemonMode\fP
resource.  This makes
.I xdm
close all file descriptors, disassociate the controlling terminal and put
itself in the background when it first starts up (just like the host
of other daemons).  It is the default behavior.
.IP "\fB-debug\fP \fIdebug_level\fP"
Specifies the numeric value for the \fBDisplayManager.debugLevel\fP
resource.  A non-zero value causes
.I xdm
to print piles of debugging statements to the terminal; it also disables the
\fBDisplayManager.daemonMode\fP resource, forcing
.I xdm
to run synchronously.  To interpret these debugging messages, a copy
of the source code for xdm is almost a necessity.  No attempt has been
made to rationalize or standardize the output.
.IP "\fB-error\fP \fIerror_log_file\fP"
Specifies the value for the \fBDisplayManager.errorLogFile\fP resource.
This file contains errors from
.I xdm
as well as anything written to stderr by the various scripts and programs
run during the progress of the session.
.IP "\fB-nodaemon\fP"
Specifies ``false'' as the value for the \fBDisplayManager.daemonMode\fP
resource.
.IP "\fB-resources\fP \fIresource_file\fP"
Specifies the value for the \fBDisplayManager*resources\fP resource.  This file
is loaded using \fIxrdb (1)\fP to specify configuration parameters for the
authentication widget.
.IP "\fB-server\fP \fIserver_entry\fP"
Specifies the value for the \fBDisplayManager.servers\fP resource.
See the section below which describes this resource in depth.
.IP "\fB-udpPort\fP \fIport_number\fP"
Specifies the value for the \fBDisplayManager.requestPort\fP resource.  This
sets the port-number which XDM will monitor for XDMCP requests.  As XDMCP
uses the registered well-known udp port 177, this resource should probably
not be changed except for debugging.
.IP "\fB-session\fP \fIsession_program\fP"
Specifies the value for the \fBDisplayManager*session\fP resource.  This
indicates the program to run when the user has logged in as the session.
.IP "\fB-xrm\fP \fIresource_specification\fP"
This allows an arbitrary resource to be specified, just as most
toolkit applications.
.SH RESOURCES
At many stages the actions of
.I xdm
can be controlled through the use of the configuration file, which is in the
familiar X resource format.  See Jim Fulton's article on resource files
(\fIdoc/tutorials/resources.txt\fP) for a description of the format.
Some resources modify the behavior of
.I xdm
on all displays,
while others modify its behavior on one single display.  Where actions relate
to a specific display,
the display name is inserted into the resource name between
``DisplayManager'' and the final resource name segment.
For example, \fBDisplayManager.expo_0.startup\fP is the name of the 
resource which defines the startup shell file on the ``expo:0'' display.
Because the resource
manager uses colons to separate the name of the resource from its value and
dots to separate resource name parts,
.I xdm
substitutes underscores for the dots and colons when generating the resource
name.
.IP "\fBDisplayManager.servers\fP"
This resource either specifies a file name full of server entries, one per
line (if the value starts with a slash), or a single server entry.  Each
entry indicates a displays which should constantly be managed and which is
not using XDMCP.  Each entry consists of at least three parts:  a display
name, a display class, a display type, and (for local servers) a command
line to start the server.  A typical entry for local display number 0 would
be:
.nf

  :0 Digital-QV local /usr/bin/X11/X :0

.fi
The display types are:
.ta 1.5i
.nf

local		a local display, i.e. one which has a server program to run
foreign		a remote display, i.e. one which has no server program to run

.fi
.IP
The display name must be something that can be passed in the \fB-display\fP
option to any X program.  This string is used in the display-specific
resources to specify the particular display, so be careful to match the
names (e.g. use ":0 local /usr/bin/X11/X :0" instead of "localhost:0 local
/usr/bin/X11/X :0" if your other resources are specified as
"DisplayManager._0.session").  The display class portion is also used in the
display-specific resources, as the class portion of the resource.  This is
useful if you have a large collection of similar displays (like a corral of
X terminals) and would like to set resources for groups of them.  When using
XDMCP, the display is required to specify the display class, so perhaps your
X terminal documentation describes a reasonably standard display class
string for your device.
.IP "\fBDisplayManager.requestPort\fP"
This indicates the UDP port number which
.I xdm
uses to listen for incoming XDMCP requests.  Unless you need to debug the
system, leave this with it's default value of 177.
.IP "\fBDisplayManager.errorLogFile\fP"
Error output is normally directed at the system console.  To redirect it simply
set this resource to any file name.  A method to send these messages to
syslog should be developed for systems which support it; however the
wide variety of "standard" interfaces precludes any system-independent
implementation.  This file also contains any output directed to stderr
by \fIXstartup, Xsession and Xreset\fP, so it will contain descriptions
of problems in those scripts as well.
.IP "\fBDisplayManager.debugLevel\fP"
A non-zero value specified for this integer resource will enable reams of
debugging information to be printed.  It also disables daemon mode which
would redirect the information into the bit-bucket.  Specifying a non-zero
debug level also allows non-root users to run
.I xdm 
which would normally not be useful.
.IP "\fBDisplayManager.daemonMode\fP"
Normally,
.I xdm
attempts to make itself into an unassociated daemon process.  This is
accomplished by forking and leaving the parent process to exit, then closing
file descriptors and mangling the controlling terminal.  When attempting to
debug
.I xdm ,
this is quite bothersome.  Setting this resource to "false" will disable
this feature.
.IP "\fBDisplayManager.pidFile\fP"
The filename specified will be created to contain an ascii
representation of the process-id of the main \fBxdm\fP process.  This is
quite useful when reinitializing the system.
.I Xdm
also uses file locking to attempt to eliminate multiple daemons running on
the same machine, which would cause quite a bit of havoc.
.IP "\fBDisplayManager.lockPidFile\fP"
This is the resource which controls whether
.I xdm
uses file locking to keep multiple xdms from running amok.  On SYSV, this
uses the lockf library call, while on BSD it uses flock.  The default value
is "true".
.IP "\fBDisplayManager.remoteAuthDir\fP"
This is a directory name which
.I xdm
uses to temporarily store authorization files for displays using XDMCP.  The
default value is /usr/lib/X11/xdm.
.IP \fBDisplayManager.autoRescan\fP"
This boolean controls whether
.I xdm
rescans the configuration file and servers file after a session terminates
and the files have changed.  By default it is "true".  You can force
.I xdm
to reread these files by sending a SIGHUP to the main process.
.IP "\fBDisplayManager.removeDomainname\fP"
When computing the display name for XDMCP clients, the resolver will
typically create a fully qualified host name for the terminal.  As this is
sometimes confusing,
.I xdm
will remove the domain name portion of the host name if it is the same as the
domain name for the local host when this variable is set.  By default the
value is "true".
.IP "\fBDisplayManager.keyFile\fP"
XDM-AUTHENTICATION-1 style XDMCP authentication requires that a private key
be shared between
.I xdm
and the terminal.  This resource specifies the file containing those
values.  Each entry in the file consists of a display name and the shared
key.  By default,
.I xdm
does not include support for XDM-AUTHENTICATION-1 as it requires DES which
is not generally distributable.
.IP "\fBDisplayManager.DISPLAY.resources\fP"
This resource specifies the name of the file to be loaded by \fIxrdb (1)\fP
as the resource data-base onto the root window of screen 0 of the display.
This resource data base is loaded just before the authentication procedure
is started, so it can control the appearance of the "login" window.  See the
section below on the authentication widget which describes the various
resources which are appropriate to place in this file.  There is no
default value for this resource, but the conventional name is
\fB/usr/lib/X11/xdm/Xresources\fP.
.IP "\fBDisplayManager.DISPLAY.xrdb\fP"
Specifies the program used to load the resources.  By default,
.I xdm
uses \fI/usr/bin/X11/xrdb\fP.
.IP "\fBDisplayManager.DISPLAY.cpp\fP"
This specifies the name of the C preprocessor which is used by xrdb.
.IP "\fBDisplayManager.DISPLAY.startup\fP"
This specifies a program which is run (as root) after the authentication
process succeeds.  By default, no program is run.  The conventional name for a
file used here is \fIXstartup\fP.  See the \fBXstartup\fP section below.
.IP "\fBDisplayManager.DISPLAY.session\fP"
This specifies the session to be executed (not running as root).
By default, \fI/usr/bin/X11/xterm\fP is
run.  The conventional name is \fIXsession\fP.  See the \fBXsession\fP
session below.
.IP "\fBDisplayManager.DISPLAY.reset\fP"
This specifies a program which is run (as root) after the session terminates.
Again, by default no program is run.
The conventional name is \fIXreset\fP.  See
the \fBXreset\fP section further on in this document.
.IP "\fBDisplayManager.DISPLAY.openDelay\fP"
.IP "\fBDisplayManager.DISPLAY.openRepeat\fP"
.IP "\fBDisplayManager.DISPLAY.openTimeout\fP"
.IP "\fBDisplayManager.DISPLAY.startAttempts\fP"
These numeric resources control the behavior of
.I xdm
when attempting to open intransigent servers.  \fBopenDelay\fP is
the length of the
pause (in seconds) between successive attempts, \fBopenRepeat\fP is the
number of attempts to make, \fBopenTimeout\fP is the amount of time
to wait while actually
attempting the open (i.e. the maximum time spent in the \fIconnect (2)\fP
syscall) and \fBstartAttempts\fP is the number of times this entire process
is done before giving up on the server.  After \fBopenRepeat\fP attempts have been made,
or if \fBopenTimeout\fP seconds elapse in any particular attempt,
.I xdm
terminates and restarts the server, attempting to connect again, this
process is repeated \fBstartAttempts\fP time, at which point the display is
declared dead and disabled.  Although
this behavior may seem arbitrary, it has been empirically developed and
works quite well on most systems.  The default values are
5 for \fBopenDelay\fP, 5 for \fBopenRepeat\fP, 30 for \fBopenTimeout\fP and
4 for \fBstartAttempts\fP.
.IP "\fBDisplayManager.DISPLAY.pingInterval\fP"
.IP "\fBDisplayManager.DISPLAY.pingTimeout\fP"
To discover when remote displays disappear,
.I xdm
occasionally "pings" them, using an X connection and sending XSync
requests.  \fBpingInterval\fP specifies the time (in minutes) between each
ping attempt, \fBpingTimeout\fP specifies the maximum amount of time (in
minutes) to wait for the terminal to respond to the request.  If the
terminal does not respond, the session is declared dead and terminated.  By
default, both are set to 5 minutes.
.I xdm
will not ping local displays.  Although it would seem harmless, it is
unpleasant when the workstation session is terminated as a result of the
server hanging for NFS service and not responding to the ping.
.IP "\fBDisplayManager.DISPLAY.terminateServer\fP"
This boolean resource specifies whether the X server should be terminated
when a session terminates (instead of resetting it).  This option can be
used when the server tends to grow without bound over time in order to limit
the amount of time the server is run.  The default value is "FALSE".
.IP "\fBDisplayManager.DISPLAY.userPath\fP"
.I Xdm
sets the PATH environment variable for the session to this value.  It should
be a colon separated list of directories, see \fIsh(1)\fP for a full
description.  The default value can be specified in the X system
configuration file with DefUserPath, frequently it is set to
":/bin:/usr/bin:/usr/bin/X11:/usr/ucb".
.IP "\fBDisplayManager.DISPLAY.systemPath\fP"
.I Xdm
sets the PATH environment variable for the startup and reset scripts to the
value of this resource.  The default for this resource is specified
with the DefaultSystemPath entry in the system configuration file, but
it is frequently "/etc:/bin:/usr/bin:/usr/bin/X11:/usr/ucb".  Note the
conspicuous absence of "." from this entry.  This is a good practise to
follow for root; it avoids many common trojan horse system penetration
schemes.
.IP "\fBDisplayManager.DISPLAY.systemShell\fP"
.I Xdm
sets the SHELL environment variable for the startup and reset scripts to the
value of this resource.  By default, it is "/bin/sh".
.IP "\fBDisplayManager.DISPLAY.failsafeClient\fP"
If the default session fails to execute,
.I xdm
will fall back to this program.  This program is executed with no
arguments, but executes using the same environment variables as
the session would have had (see the section "Xsession" below).
By default, \fI/usr/bin/X11/xterm\fP is used.
.IP "\fBDisplayManager.DISPLAY.grabServer\fP"
.IP "\fBDisplayManager.DISPLAY.grabTimeout\fP"
To eliminate obvious security shortcomings in the X protocol,
.I xdm
grabs the server and keyboard while reading the name/password.  The
\fBgrabServer\fP resource specifies if the server should be held for the
duration of the name/password reading, when FALSE, the server is ungrabbed
after the keyboard grab succeeds, otherwise the server is grabbed until just
before the session begins.  The \fBgrabTimeout\fP resource specifies the
maximum time
.I xdm
will wait for the grab to succeed.  The grab may fail if some other
client has the server grabbed, or possibly if the network latencies
are very high.  This resource has a default value of 3 seconds; you
should be cautious when raising it as a user can be spoofed by a
look-alike window on the display.  If the grab fails,
.I xdm
kills and restarts the server (if possible) and session.
.IP "\fBDisplayManager.DISPLAY.authorize\fP"
.IP "\fBDisplayManager.DISPLAY.authName\fP"
\fBauthorize\fP is a boolean resource which controls whether
.I xdm
generates and uses authorization for the server connections.  If
authorization is used, \fBauthName\fP specifies the type to use.  Currently,
xdm supports only MIT-MAGIC-COOKIE-1 authorization, XDM-AUTHORIZATION-1
could be supported as well, but DES is not generally distributable.  XDMCP
connections specify which authorization types are supported dynamically, so
\fBauthName\fP is ignored in this case.  When \fBauthorize\fP is set for a
display and authorization is not available, the user is informed by having a
different message displayed in the login widget.  By default, \fBauthorize\fP
is "true"; \fBauthName\fP is "MIT-MAGIC-COOKIE-1".
.IP \fBDisplayManager.DISPLAY.authFile\fP"
This file is used to communicate the authorization data from \fBxdm\fP to
the server, using the \fI-auth\fP server command line option.  It should be
kept in a directory which is not world-writable as it could easily be
removed, disabling the authorization mechanism in the server.
.IP "\fBDisplayManager.DISPLAY.resetForAuth\fP"
The original implementation of authorization in the sample server reread the
authorization file at server reset time, instead of when checking the
initial connection.  As
.I xdm
generates the authorization information just before connecting to the
display, an old server would not get up-to-date authorization information.
This resource causes
.I xdm
to send SIGHUP to the server after setting up the file, causing an
additional server reset to occur, during which time the new authorization
information will be read
.IP "\fBDisplayManager.DISPLAY.userAuthDir\fP"
When
.I xdm
is unable to write to the usual user authorization file ($HOME/.Xauthority),
it creates a unique file name in this directory and points the environment
variable XAUTHORITY at the created file.  By default it uses "/tmp".
.SH "CONTROLLING THE SERVER"
.I Xdm
controls local servers using POSIX signals.  SIGHUP is expected to reset the
server, closing all client connections and performing other clean up
duties.  SIGTERM is expected to terminate the server.  If these signals do
not perform the expected actions,
.I xdm
will not perform properly.
.PP
To control remote servers not using XDMCP,
.I xdm
searches the window hierarchy on the display and uses the protocol request
KillClient in an attempt to clean up the terminal for the next session.  This
may not actually kill all of the clients, as only those which have created
windows will be noticed.  XDMCP provides a more sure mechanism; when xdm
closes it's initial connection, the session is over and the terminal is
required to close all other connections.
.SH "CONTROLLING XDM"
.PP
.I Xdm
responds to two signals: SIGHUP and SIGTERM.  When sent a SIGHUP,
.I xdm
rereads the configuration file and the 1file specified by the
\fBDisplayManager.servers\fP resource and notices if entries have been added
or removed.  If a new entry has been added,
.I xdm
starts a session on the associated display.  Entries which have been removed
are disabled immediately, meaning that any session in progress will be
terminated without notice, and no new session will be started.
.PP
When sent a SIGTERM,
.I xdm
terminates all sessions in progress and exits.  This can be used when
shutting down the system.
.PP
.I Xdm
attempts to mark the various sub-processes for ps(1) by editing the
command line argument list in place.  Because xdm can't allocate additional
space for this task, it is useful to start xdm with a reasonably long
command line (15 to 20 characters should be enough).  Each process which is
servicing a display is marked "-<Display-Name>".
.SH "AUTHENTICATION WIDGET"
The authentication widget is an application which reads a name/password pair
from the keyboard.  As this is a toolkit client, nearly every imaginable
parameter can be controlled with a resource.  Resources for this widget
should be put into the file named by
\fBDisplayManager.DISPLAY.resources\fP.  All of these have reasonable
default values, so it is not necessary to specify any of them.
.IP "\fBxlogin.Login.width, xlogin.Login.height, xlogin.Login.x, xlogin.Login.y\fP"
The geometry of the login widget is normally computed automatically.  If you
wish to position it elsewhere, specify each of these resources.
.IP "\fBxlogin.Login.foreground\fP"
The color used to display the typed-in user name.
.IP "\fBxlogin.Login.font\fP"
The font used to display the typed-in user name.
.IP "\fBxlogin.Login.greeting\fP"
A string which identifies this window.
The default is "Welcome to the X Window System".
.IP "\fBxlogin.Login.unsecureGreeting\fP"
When X authorization is requested in the configuration file for this
display and none is in use, this greeting replaces the standard
greeting.  It's default value is "This is an unsecure session"
.IP "\fBxlogin.Login.greetFont\fP"
The font used to display the greeting.
.IP "\fBxlogin.Login.greetColor\fP"
The color used to display the greeting.
.IP "\fBxlogin.Login.namePrompt\fP"
The string displayed to prompt for a user name.
.I Xrdb
strips trailing white space from resource values, so to add spaces at
the end of the prompt (usually a nice thing), add spaces escaped with
backslashes.  The default is "Login:  "
.IP "\fBxlogin.Login.passwdPrompt\fP"
The string displayed to prompt for a password.
The default is "Password:  ".
.IP "\fBxlogin.Login.promptFont\fP"
The font used to display both prompts.
.IP "\fBxlogin.Login.promptColor\fP"
The color used to display both prompts.
.IP "\fBxlogin.Login.fail\fP"
A message which is displayed when the authentication fails.
The default is "Login Failed, please try again".
.IP "\fBxlogin.Login.failFont\fP"
The font used to display the failure message.
.IP "\fBxlogin.Login.failColor\fP"
The color used to display the failure message.
.IP "\fBxlogin.Login.failTimeout\fP"
The time (in seconds) that the fail message is displayed.
The default is 30 seconds.
.IP "\fBxlogin.Login.translations\fP"
This specifies the translations used for the login widget.  Refer to the X
Toolkit documentation for a complete discussion on translations.  The default
translation table is:
.nf
.ta .5i 2i

	Ctrl<Key>H:	delete-previous-character() \\n\\
	Ctrl<Key>D:	delete-character() \\n\\
	Ctrl<Key>B:	move-backward-character() \\n\\
	Ctrl<Key>F:	move-forward-character() \\n\\
	Ctrl<Key>A:	move-to-begining() \\n\\
	Ctrl<Key>E:	move-to-end() \\n\\
	Ctrl<Key>K:	erase-to-end-of-line() \\n\\
	Ctrl<Key>U:	erase-line() \\n\\
	Ctrl<Key>X:	erase-line() \\n\\
	Ctrl<Key>C:	restart-session() \\n\\
	Ctrl<Key>\\\\:	abort-session() \\n\\
	<Key>BackSpace:	delete-previous-character() \\n\\
	<Key>Delete:	delete-previous-character() \\n\\
	<Key>Return:	finish-field() \\n\\
	<Key>:	insert-char() \\

.fi
.PP
The actions which are supported by the widget are:
.IP "delete-previous-character"
Erases the character before the cursor.
.IP "delete-character"
Erases the character after the cursor.
.IP "move-backward-character"
Moves the cursor backward.
.IP "move-forward-character"
Moves the cursor forward.
.IP "move-to-begining"
(Apologies about the spelling error.)
Moves the cursor to the beginning of the editable text.
.IP "move-to-end"
Moves the cursor to the end of the editable text.
.IP "erase-to-end-of-line"
Erases all text after the cursor.
.IP "erase-line"
Erases the entire text.
.IP "finish-field"
If the cursor is in the name field, proceeds to the password field; if the
cursor is in the password field, check the current name/password pair.  If
the name/password pair are valid,
.I xdm
starts the session.  Otherwise the failure message is displayed and
the user is prompted to try again.
.IP "abort-session"
Terminates and restarts the server.
.IP "abort-display"
Terminates the server, disabling it.  This is a rash action and
is not accessible in the default configuration.  It can be used to
stop
.I xdm
when shutting the system down, or when using xdmshell.
.IP "restart-session"
Resets the X server and starts a new session.  This can be used when
the resources have been changed and you want to test them, or when
the screen has been overwritten with system messages.
.IP "insert-char"
Inserts the character typed.
.IP "set-session-argument"
Specifies a single word argument which is passed to the session at startup.
See the sections on \fBXsession\fP and \fBTypical usage\fP.
.IP "allow-all-access"
Disables access control in the server, this can be used when
the .Xauthority file cannot be created by xdm.  Be very careful using
this, it might be better to disconnect the machine from the network
before doing this.
.SH "The Xstartup file"
.PP
This file is typically a shell script.  It is run as "root" and should be
very careful about security.  This is the place to put commands which make
fake entries in /etc/utmp, mount users' home directories from file servers,
display the message of the day, or abort the session if logins are not
allowed.  Various environment variables are set for the use of this script:
.nf
.ta .5i 2i

	DISPLAY	is set to the associated display name
	HOME	is set to the home directory of the user
	USER	is set to the user name
	PATH	is set to the value of \fBDisplayManager.DISPLAY.systemPath\fP
	SHELL	is set to the value of \fBDisplayManager.DISPLAY.systemShell\fP
	XAUTHORITY	may be set to an authority file

.fi
.PP
No arguments of any kind are passed to the script.
.I Xdm
waits until this script exits before starting the user session.  If the
exit value of this script is non-zero,
.I xdm
discontinues the session immediately and starts another authentication
cycle.
.SH "The Xsession program"
.PP
This is the command which is run as the user's session.  It is run with
the permissions of the authorized user, and has several environment variables
specified:
.nf
.ta .5i 2i

	DISPLAY	is set to the associated display name
	HOME	is set to the home directory of the user
	USER	is set to the user name
	PATH	is set to the value of \fBDisplayManager.DISPLAY.userPath\fP
	SHELL	is set to the user's default shell (from /etc/passwd)
	XAUTHORITY	may be set to a non-standard authority file

.fi
.PP
At most installations, \fIXsession\fP should look in $HOME for
a file \fI\.xsession\fP
which would contain commands that each user would like to use as a session.
This would replace the system default session.  \fIXsession\fP should also
implement the system default session if no user-specified session exists.
See the section \fBTypical Usage\fP below.
.PP
An argument may be passed to this program from the authentication widget
using the `set-session-argument' action.  This can be used to select
different styles of session.  One very good use of this feature is to allow
the user to escape from the ordinary session when it fails.  This would
allow users to repair their own \fI.xsession\fP if it fails,
without requiring administrative intervention.  The section on typical usage
demonstrates this feature.
.SH "The Xreset file"
.PP
Symmetrical with \fIXstartup\fP, this script is run after the user session has
terminated.  Run as root, it should probably contain commands that undo
the effects of commands in \fIXstartup\fP, removing fake entries
from \fI/etc/utmp\fP
or unmounting directories from file servers.  The collection of environment
variables that were passed to \fIXstartup\fP are also
given to \fIXreset\fP.
.SH "Typical Usage"
.PP
Actually,
.I xdm
is designed to operate in such a wide variety of environments that "typical"
is probably a misnomer.  However, this section will focus on making
.I xdm
a superior solution to traditional means of starting X from /etc/ttys or
manually.
.PP
First off, the
.I xdm
configuration file should be set up.  A good thing to do is to
make a directory (\fI/usr/lib/X11/xdm\fP comes immediately to mind)
which will contain all of the relevant
files.  Here is a reasonable configuration file, which could be
named \fIxdm-config\fP :
.nf

.ta .5i 4i

	DisplayManager.servers:	/usr/lib/X11/xdm/Xservers
	DisplayManager.errorLogFile:	/usr/lib/X11/xdm/xdm-errors
	DisplayManager.pidFile:	/usr/lib/X11/xdm/xdm-pid
	DisplayManager*resources:	/usr/lib/X11/xdm/Xresources
	DisplayManager*session:	/usr/lib/X11/xdm/Xsession
	DisplayManager._0.authorize:	true
	DisplayManager*authorize:	false

.fi
.PP
As you can see, this file simply contains references to other files.  Note
that some of the resources are specified with ``*'' separating the
components.  These resources can be made unique for each different display,
by replacing the ``*'' with the display-name, but normally this is not very
useful.  See the \fBResources\fP section for a complete discussion.
.PP
The first file \fI/usr/lib/X11/xdm/Xservers\fP contains the list of displays to
manage.  Most workstations have only one display, numbered 0, so the file
will look like this:
.nf
.ta .5i

	:0 Local local /usr/bin/X11/X :0

.fi
.PP
This will keep \fI/usr/bin/X11/X\fP running on this display and
manage a continuous cycle of sessions.
.PP
The file \fI/usr/lib/X11/xdm/xdm-errors\fP will contain error messages from
.I xdm
and anything output to stderr by \fIXstartup, Xsession or Xreset\fP.  When
you have trouble getting
.I xdm
working, check this file to see if
.I xdm
has any clues to the trouble.
.PP
The next configuration entry, \fI/usr/lib/X11/xdm/Xresources\fP, is loaded onto
the display as a resource database using \fIxrdb (1)\fP.  As the authentication
widget reads this database before starting up, it usually contains
parameters for that widget:
.nf
.ta .5i 1i

	xlogin*login.translations: #override\\
		<Key>F1: set-session-argument(failsafe) finish-field()\\n\\
		<Key>Return: set-session-argument() finish-field()
	xlogin*borderWidth: 3
	#ifdef COLOR
	xlogin*greetColor: #f63
	xlogin*failColor: red
	xlogin*Foreground: black
	xlogin*Background: #fdc
	#else
	xlogin*Foreground: black
	xlogin*Background: white
	#endif

.fi
.PP
The various colors specified here look reasonable on several of the displays
we have, but may look awful on other monitors.  As X does not currently have
any standard color naming scheme, you might need to tune these entries to
avoid disgusting results.  Please note the translations entry; it specifies
a few new translations for the widget which allow users to escape from the
default session (and avoid troubles that may occur in it).  Note that if
#override is not specified, the default translations are removed and replaced
by the new value, not a very useful result as some of the default translations
are quite useful (like "<Key>: insert-char ()" which responds to normal
typing).
.PP
The \fIXstartup\fP file used here simply prevents login while the
file \fI/etc/nologin\fP
exists.  As there is no provision for displaying any messages here
(there isn't any core X client which displays files),
the user will probably be baffled by this behavior.
I don't offer this as a complete example, but
simply a demonstration of the available functionality.
.PP
Here is a sample \fIXstartup\fP script:
.nf
.ta .5i 1i

	#!/bin/sh
	#
	# Xstartup
	#
	# This program is run as root after the user is verified
	#
	if [ -f /etc/nologin ]; then
		exit 1
	fi
	exit 0
.fi
.PP
.PP
The most interesting script is \fIXsession\fP.  This version recognizes
the special
"failsafe" mode, specified in the translations
in the \fIXresources\fP file above, to provide an escape
from the ordinary session:
.nf
.ta .5i 1i 1.5i

	#!/bin/sh
	#
	# Xsession
	#
	# This is the program that is run as the client
	# for the display manager.  This example is
	# quite friendly as it attempts to run a per-user
	# .xsession file instead of forcing a particular
	# session layout
	#
	
	case $# in
	1)
		case $1 in
		failsafe)
			exec xterm -geometry 80x24-0-0 -ls
			;;
		esac
	esac
	
	startup=$HOME/.xsession
	resources=$HOME/.Xresources
	
	if [ -f $startup ]; then
		exec $startup
		exec /bin/sh $startup
	else
		if [ -f $resources ]; then
			xrdb -load $resources
		fi
		twm &
		exec xterm -geometry 80x24+10+10 -ls
	fi

.fi
.PP
No \fIXreset\fP script is necessary, so none is provided.

.SH "SOME OTHER POSSIBILITIES"
.PP
You can also use
.I xdm
to run a single session at a time, using the 4.3 \fIinit\fP
options or other suitable daemon by specifying the server on the command
line:
.nf
.ta .5i

	xdm -server ":0 SUN-3/60CG4 local /usr/bin/X :0"

.fi
.PP
Or, you might have a file server and a collection of X terminals.  The
configuration for this could look identical to the sample above,
except the \fIXservers\fP file might look like:
.nf
.ta .5i

	extol:0 VISUAL-19 foreign
	exalt:0 NCD-19 foreign
	explode:0 NCR-TOWERVIEW3000 foreign

.fi
.PP
This would direct
.I xdm
to manage sessions on all three of these terminals.  See the section
"Controlling Xdm" above for a description of using signals to enable
and disable these terminals in a manner reminiscent of init(8).
.PP
One thing that
.I xdm
isn't very good at doing is coexisting with other window systems.  To use
multiple window systems on the same hardware, you'll probably be more
interested in
.I xinit .
.SH "SEE ALSO"
X(1), xinit(1) and XDMCP
.SH BUGS
.br
.SH COPYRIGHT
Copyright 1988, Massachusetts Institute of Technology.
.br
See \fIX(1)\fP for a full statement of rights and permissions.
.SH AUTHOR
Keith Packard, MIT X Consortium
