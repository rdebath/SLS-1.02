#ident	"@(#)uucp:Permissions	2.2"
# This entry for public login.
# It provides the default permissions.
# See the Basic Networking Utilities Guide for more information.
#
#
#MACHINE=ima COMMANDS=ALL REQUEST=yes SENDFILES=yes READ=/ WRITE=/
#LOGNAME=ima COMMANDS=ALL REQUEST=yes SENDFILES=yes READ=/ WRITE=/
#
#
# the following entry can be explained as follows:
#	MACHINE = system name
#	CALLBACK = call them back when they call in
#	REQUEST = can remote system request files from this system
#	READ/WRITE = paths uucico can access for requesting/depositing files
#	COMMANDS = specific commands that uux can execute
#
MACHINE=SYSTEM LOGNAME=nuucp \
READ=/usr/spool/uucppublic \
WRITE=/usr/spool/uucppublic \
SENDFILES=yes REQUEST=yes \
COMMANDS=/bin/rmail

# I'm paranoid, so I have one of these for each uucp neighbor
# with the proper paths edited in...note the long path to rnews
#
MACHINE=my_uucp_neighbor LOGNAME=my_uucp_neighbor \
READ=/usr/spool/uucp/my_uucp_neighbor \
WRITE=/usr/spool/uucp/my_uucp_neighbor \
SENDFILES=yes REQUEST=yes \
COMMANDS=/bin/rmail:/usr/bin/smail:/usr/local/lib/news/bin/rnews


