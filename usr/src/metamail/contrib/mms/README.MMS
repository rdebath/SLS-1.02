Subject: mms: metamail server
Date: Thu Jun 3 8:00:00 JST 1992
From: Yutaka Sato <ysato@etl.go.jp>
Organization: Electrotechnical Laboratory, Tsukuba Science City, Japan

=====================================================================

                         MMS-0.5

---------------------------------------------------------------------
1. WHAT IS MMS ?
---------------------------------------------------------------------

    MMS is the metamail server which serves Metamail functions via
inter-process communication.  MMS make it easy to integrate Metamail
functions into a "user agent" which is implemented in an arbitrary
language.

   Current MMS is an experimental prototype. External specification
of MMS will be changed frequently as long as the version number stays
zero.

---------------------------------------------------------------------
2. MMS PROTOCOL
---------------------------------------------------------------------

2.1 COMMANDS

   Commands consist of a command word, which in some cases may be
   followed by a argument string.

       command [argment_string]

   Command ward is not case sensitive.

2.2 RESPONSES

   Responses consist of a status response, which in some case may be
   followed by response body.

2.2.1 STATUS RESPONSE

   A status message is constracted as follows:

       ABC D message

   where

    A class {1:Informative, 2:Ok, 3:Send-the-rest, 4:Error, 5:Serious}
    B category {9:MMS status}
    C identification
    D response body {0:Non, N:Non encoded response body,
                            B:Base64 coded response body follows}

2.2.2 RESPONSE BODY

   Response body terminates with "." i.e. a line including only a dot
   character.  A line ".." means "."

---------------------------------------------------------------------
3. HOW TO MAKE AND TEST MMS
---------------------------------------------------------------------

You need Metamail version 2.2 which is at the directory $METAMAIL.
To make mms, do as follows:

  (1) mkdir $METAMAIL/contrib/mms
  (2) chdir $METAMAIL/contrib/mms
  (3) tar xf mms.tar
  (4) make

When mms is made, it can be tested with mmstest.sh:

  (5) mmstest.sh
      metamail /tmp/mms.mime.$USER
  (6) mms -server < test.mms; metamail mms.out

The content of this file may be helpful to understand how to use mms.

  (7) mms -viewer MIME_FILE                 ## simple viewer
      mms -viewer /tmp/mms.mime.$USER
  (8) mms -transl -sh < MIME_FILE > x.sh    ## simple translator
      sh x.sh

---------------------------------------------------------------------
4. USING MMS VIA INTERNET PROTOCOL
---------------------------------------------------------------------

4.1 SERVER INSTALLATION
    (1) service port
         /etc/services
           mms   2200/tcp      # metamail server
         % cd /etc/yp; make services
    (2) inetd
         /etc/inetd.conf
           mms   stream  tcp  nowait  root /usr/local/bin/mms  mms -server
         % kill -HUP PID_OF(inetd)
 
4.2 AUTHENTICATION
  Client must know followings:
    (1) USER name on the server HOST
    (2) PASSWD_FILE name on the HOST which records the encrypted password
                                and is readable only for the USER
    (3) PASSWORD
  Client must send "user" command to be executable all MMS commands.

4.3 ADDING NEW USER
  The procedure to add a new user is as follows:
    (1) On the HOST, create a PASSWD_FILE which is readable only for the USER.
    (2) get crypted PASSWORD as follows:
          % telnet HOST mms
          user USER PASSWD_FILE PASSWORD
          591 0 incorrect passwd PASSWORD (CRYPTED-PASSWORD)
          QUIT
    (3) write the CRYPTED-PASSWORD into PASSWD_FILE.

4.4 AUTOMATIC AUTHENTICATION
  To do automatic authentication in mms client library,
     store CRYPTED-PASSWORD in ServerHost:$HOME/".mmspasswd", and
     store PASSWORD in ClientHost:$HOME/".mmsprofile".

4.5 TESTING
  Do as follows:
    % telnet ServerHost mms
    HELP
    QUIT

   (You need setting up described in 4.4 to proceed)
    % mms -host AudioHost "show audio/basic" AudioFile
    % setenv MMSERVER ServerHost
    % mmstest.sh

---------------------------------------------------------------------
5. MANIFEST
---------------------------------------------------------------------

   README.MMS        this file
   Makefile          makefile for MMS

   mms.c             MMS main
   mmsserver.c       MMS server
   mmsclient.c       Client library
   mmsparser.c       MIME parser
   mmssynthe.c       MIME synthesizer
   mmsviewer.c       MIME viewer based on mmsparser
   mmstransl.c       MIME translator
   mmsauth.c         Authentication

   mmstest.sh        MMS test written in shell script
   test.mms          MMS test in MMS commands
   mms.el            MMS interface written in Emacs/Lisp

   news/             patches to make nntpd translate MIME header
   lib/              library

---------------------------------------------------------------------
6. LATEST VERSION OF MMS
---------------------------------------------------------------------

The latest VERSION of mms is available via FTP from:

   HOST: etlport.etl.go.jp [192.31.197.99]
   FILE: pub/mime/mms.VERSION.tar.Z

--
Yutaka Sato <ysato@etl.go.jp>
Information Base Section
ELECTROTECHNICAL LABORATORY
1-1-4 Umezono, Tsukuba, Ibaraki, 305 Japan
