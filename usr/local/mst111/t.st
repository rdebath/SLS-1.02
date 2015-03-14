Smalltalk at: #DebugPrinting put: true!

!Object methodsFor: 'debugging'!

bugOut: aString
    DebugPrinting 
	ifTrue: [ aString print. self printNl ]
!

hexBug: aString
    DebugPrinting 
	ifTrue: [ aString print. self printOn: stdout base: 16. stdout nl ]
!!


!BlockContext methodsFor: 'fun hacking'!

ifDebug
    DebugPrinting ifTrue: [ self value ]
!!


!String methodsFor: 'converting'!

asByteArray
    | bytes i |
    bytes _ ByteArray new: self size.
    i _ 1.
    self do: [ :char | bytes at: i put: (char asciiValue).
		       i _ i + 1 ].
    ^bytes
!!



!Display methodsFor: 'anonymous protocol requests'!

listFonts: aPattern maxNames: anInteger
    | packet numStrs strs n str |
    packet _ XPacket command: 49.
    socket bytes: (packet word: anInteger; word: aPattern size;
			       bytes: aPattern asByteArray; done).
    socket getReply
	ifFalse: [ ^self ].
    
    numStrs _ socket word.
    socket skipBytes: 22.	"there should be a better way"
    strs _ Array new: numStrs.
    1 to: numStrs do:
	[ :i | n _ socket byte.
	       str _ (socket getUnpaddedString: n).
	       strs at: i put: str ].
    ^strs
!!



| d s result id title win gc event button tracking pen |

    id _ 0.

    "Create the initial display connection"
    d _ Display host: '' display: 0.
    d isNil ifTrue: [ ^self error: 'Failed to create display' ].

    " Create the root window from the initial display connection"
    RootWindow _ Window new: d id: RootWindowID.

    "An example of how to work listFonts:"
"    (d listFonts: '*adobe*normal*' maxNames: 9999) printNl."

    s _ d socket.


    "Create a window that is n bits deep, where n is the default screen depth"
    win _ RootWindow createWindow: 0 
		     x: 100 y: 100
		     width: 400 height: 200
		     borderWidth: 1 class: #CopyFromParent
		     visual: #CopyFromParent
		     attrs: [ :pack | pack backgroundPixel: WhitePixel;
					  borderPixel: BlackPixel;
					   eventMask: #(Exposure ButtonPress ButtonRelease)
					  ].


    gc _ win createGC: [ :pack | pack foreground: BlackPixel;
				      background: WhitePixel;
				      "lineWidth: 10;"
				      lineStyle: #Solid ].

    " Set the name of the window "
    win changeProperty: #WmName
	type: #String mode: #Replace format: 8 data: Version asByteArray.

    " Display the window "
    win mapWindow.

    " Get the exposure event "
    s getPacket.

    " Draw a nice picture "
    pen _ Pen new: gc.
    pen goto: 150@100.
    pen down.

    "This will draw a spiral-like image"
"    1 to: 40 do: [ :i | pen turn: -50.
			pen go: i]."

    " Draw a dragon curve "
    pen dragon: 6.

    button _ 230 @ 130 extent: 130 @ 40.

    " create the rest of the image, including the button "
    gc drawExampleImage: button.

    " Wait until a button press event "
    [ event _ s getPacket.
      event class == ButtonPressEvent
	  ifTrue: [ (button containsPoint: event eventX @ event eventY) ]
	  ] whileFalse.

    " Tidy up "
    win destroyWindow.

!

Smalltalk quitPrimitive!

