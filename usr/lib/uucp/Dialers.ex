#
# Dialers - note the setup strings to set the TrailBlazer registers
#
scout	=W-,	"" ATM0DT\T CONNECT
tbfast	=W-,	"" A\pA\pA\pT OK ATS50=255DT\T CONNECT\sFAST
tbslow	=W-,	"" A\pA\pA\pT OK ATS50=2DT\T CONNECT\s1200
tbmed	=W-,	"" A\pA\pA\pT OK ATS50=3DT\T CONNECT\s2400

#ident	"@(#)uucp:Dialers	2.8"
#
# Each caller type that appears in the Devices file (5th field)
# should appear in this file except for the built in callers.
# Each line consists of three parts:
# - the name of the caller
# - the translation table for the phone number to translate from
#   the 801 codes (=-) to the code for the particular device
# - a chat script (same format and meaning as the login scripts
#   that appear in the Systems file.
#
# Meaning of some of the escape characters:
# \p - pause (approximately 1/4-1/2 second delay)
# \d - delay (2 seconds)
# \D - phone number/token
# \T - phone number with Dialcodes and character translation
# \N - null byte
# \K - insert a BREAK
# \E - turn on echo checking (for slow devices)
# \e - turn off echo checking
# \r - carriage return
# \c - no new-line
# \n - send new-line
# \nnn - send octal number
#
# See the Administration Documentation for more details.
#
# NOTE: blank lines and lines that begin with a <space>, <tab>, or # are ignored

penril	=W-P	"" \d > Q\c : \d- > s\p9\c )-W\p\r\ds\p9\c-) y\c : \E\TP > 9\c OK
ventel	=&-%	"" \r\p\r\c $ <K\T%%\r>\c ONLINE!
vadic	=K-K	"" \005\p *-\005\p-*\005\p-* D\p BER? \E\T\e \r\c LINE
develcon ""	"" \pr\ps\c est:\007 \E\D\e \007
micom	""	"" \s\c NAME? \D\r\c GO
direct

#  Rixon Intelligent Modem -- modem should be set up in the Rixon 
#  mode and not the Hayes mode.
#
rixon	=&-%	"" \r\r\d $ s9\c )-W\r\ds9\c-) s\c : \T\r\c $ 9\c LINE

#   Hayes Smartmodem -- modem should be set with the configuration
#   switches as follows:
#
#       S1 - UP		S2 - UP		S3 - DOWN	S4 - UP
#       S5 - UP		S6 - DOWN	S7 - ?		S8 - DOWN
#
hayes	=,-,	"" \dAT\r\c OK\r \EATDT\T\r\c CONNECT

##########
#   AT&T Programmable 300/1200 Modem Model 4000
#
#	Commands:	Explanation:
#	------------	---------------------------------------------------
#	=,-,		Use comma for secondary dial tone & for pause
#	""		expect nothing
#	\033\r\r\c	Escape sequences!! This one should work right after
#			power-up and later. If first use after power up,
#			the modem will ignore the escape and see the
#			two carriage returns--and become active and
#			spits out the MODEM: prompt. Otherwise, the modem
#			is already active and the escape followed by the
#			first carriage return tells the modem to go into
#			menu mode, and the second carriage return simply
#			causes the MODEM: prompt to come out.
#	DEM:		expect "DEM:" (actually "MODEM:")
#	\033s0401\c	Turn the speaker off.
#	\006		expct ACK (octal 6)
#	\033s0901\c	DTR should control connection (i.e. dropping DTR
#			drops connection and disallows auto answer)
#	\006		expct ACK (octal 6)
#	\033s1001\c	Ignore the cancel character.
#	\006		expct ACK (octal 6)
#	\033s1102\c	Turn off ability for remote modem to start test
#			in this modem.
#	\006		expct ACK (octal 6)
#	\033dT\T\r\c	Tell the modem to dial the given number, using
#			touch-tone signaling (change the first T to P
#			if you want to use pulse dialing).
#	\006		expct ACK (octal 6)
##########
att4000	=,-,	"" \033\r\r\c DEM: \033s0401\c \006 \033s0901\c \006 \033s1001\c \006 \033s1102\c \006 \033dT\T\r\c \006

##########
#  AT&T DATAPHONE II 2212C Modem
#
#	Commands:	Explanation:
#	------------	---------------------------------------------------
#	=+-,		'+' for secondary dial tone, ',' for pause
#	""		expect nothing
#	\r\c		send carriage return to enter interactive mode
#	:--:		expect colon.  if don't see it, send newline
#			and expect colon again.
#	ato12=y,T\T\r\c	set option 12 (transparent data mode) to y,
#			pause for 2 seconds, dial the given number using
#			touch-tone signaling (change the first T to P if
#			you want to use pulse dialing).
#	red		expect "red" (actually "Answered")
#
#			Once transparent data mode is enabled, you
#			cannot enter the interactive mode from the data mode.
##########
att2212c	=+-,	"" \r\c :--: ato12=y,T\T\r\c red

##########
#  AT&T DATAPHONE II 2224 Modem
#
#	This entry is for 2224A/2224B with the optional automatic caller.
#	If using a 2224A with a AT&T 801CR ACU, use dialer type "801"
#	instead.
#
#	Commands:	Explanation:
#	------------	---------------------------------------------------
#	=+-,		'+' for secondary dial tone, ',' for pause
#	""		expect nothing
#	\r\c		send carriage return to enter interactive mode
#	:--:		expect colon.  if don't see it, send newline
#			and expect colon again.
#	T\T\r\c		dial the given number, using touch-tone signaling
#			(change the first T to P if you want to use pulse
#			dialing).
#	red		expect "red" (actually "Answered")
##########
att2224	=+-,	"" \r\c :--: T\T\r\c red

##########
#  Network Listener Service
#  The format of the request to the listener is
#		NLPS:000:001:<service_code>\N\c
#  where <service_code> determines what server the listener invokes.
#  The entry below asks for service code 1.
#
#  If cu & uucico use different service codes, you will have to use 
#  separate Dialers files (e.g., Dialers.cico and Dialers.cu).
#  See comments in Sysfiles for instructions.
##########
nls	""	"" NLPS:000:001:1\N\c

