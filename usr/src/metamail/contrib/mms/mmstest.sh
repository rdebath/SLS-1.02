#!/bin/sh
echo "*** TESTING THE MMS: MetaMail-Server ***"


if [ "$MMSERVER" != "" ]; then
	MMS="mms -host $MMSERVER"
#	PASSWORD=`cat $HOME/.mmsprofile`
#	PASSFILE=".mmspasswd"
	echo "### connecting to internet metamail server (mms@$MMSERVER) ###"
else
	MMS="mms -server"
	echo "(setenv MMSERVER to check the internet installation)"
	echo "### local mms ###"
fi

if [ "$MMSERVER" = "" -o "$MMSERVER" = `hostname` ]; then
	MIMEFILE="/tmp/mms.mime.$USER"
fi


REQMARK="<REQUEST>"
SEND(){
	echo "ECHO $REQMARK $M"
	echo "$M"
}
SENDBODY(){
	echo "$B"
}
ENDOFBODY(){
	echo "."
}

AUDIOFILE="/usr/demo/SOUND/sounds/cowbell.au"
XWDTMP="/tmp/mms.xwd.$$"
#
#
#
(
#	if [ "$MMSERVER" != "" ]; then
#		M="USER $USER $PASSFILE $PASSWORD";	SEND;
#	fi
	M="ENV TERM $TERM";				SEND;
	M="ADDHEAD To $USER";				SEND;
	M="SUBJECT MMS TEST";				SEND;
	M="ADDTEXT bold";				SEND;
	B="This text is composed with MMS on ";		SENDBODY;
	B=`hostname`;					SENDBODY;
							ENDOFBODY;
	M="PRINT";					SEND;
	if [ "$DISPLAY" != "" ]; then
		M="ENV DISPLAY $DISPLAY";		SEND;
		M="ENCLOSE audio/basic";		SEND;
		mmencode $AUDIOFILE;			ENDOFBODY;
		M="SH xwd -id $WINDOWID -out $XWDTMP";	SEND;
		M="ENCLOSE image/xwd $XWDTMP";	 	SEND;
		M="SH rm $XWDTMP";			SEND;
	fi
	M="SKELETON";					SEND;
	M="VIEW";					SEND;
	if [ "$MIMEFILE" != "" ]; then
		M="WRITE $MIMEFILE";			SEND;
	fi
	M="QUIT";					SEND;

) | $MMS | awk \
	'{	if($3 == "'$REQMARK'"){\
			printf "%s %s %s %s\n",$4,$5,$6,$7 }\
		else{	printf "\t%s\n",$0 }\
	}'

if [ "$MIMEFILE" != "" ]; then
	echo "*******************************************************"
	echo "A sample MIME message was written to $MIMEFILE, Try"
	echo '% '"metamail $MIMEFILE"
fi

