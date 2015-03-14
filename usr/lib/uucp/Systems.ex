
# this is a vanilla system using the 'scout' dialer (semi-normal Hayes-like)
remote Any;1 ACU 19200 scout5555555 "" \r ogin:--ogin: account  word: my_password

#ident	"@(#)uucp:Systems	2.4"
#
# Entries have this format:
#
#	Machine-Name Time Type Class Phone Login
#
# Machine-Name		node name of the remote machine
# Time			day-of-week and time-of-day when you may call
#			(e.g., MoTuTh0800-1700). Use "Any" for any day.
#			Use "Never" for machines that poll you, but that
#			you never call directly.
# Type			device type
# Class			transfer speed
# Phone			phone number (for autodialers) or token (for
#			data switches)
# Login			login sequence is composed of fields and subfields
#			in the format "[expect send] ...".  The expect field
#			may have subfields in the format "expect[-send-expect]".
#
# Example:
#	cuuxb Any ACU 1200 chicago8101242 in:--in: nuucp word: panzer
#
# See Basic Networking Utilities - release 1 # 307-165 for details
