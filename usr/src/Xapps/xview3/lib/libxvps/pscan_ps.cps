%
% @(#)pscan_ps.cps	1.29 4/25/91 Copyright 1990 Sun Microsystems, Inc.
%

% CPS definitions for XView/NeWS library
%
% This file contains definitions needed for using XView and NeWS
% together.  Much of the code concerns NeWS canvas handling, which
% in this library are stored as NeWS "usertokens" CPS routines here
% create and manipulate these canvas usertokens.
%
% Henry Krempel, May 19 1990;  Based on Robin Schaufler's original work
% Pravin Kumar,	 30-Nov-1990:
%	Got scrolling to work
%	Simplified error handling
%	Ensured that canvas is *ALWAYS* in NeWS rather than X coordinates
%	Removed public access to the CPS routines in this file
%

#define	BUFSIZ	 4096
#define VALUETAG 2006
#define ERRORTAG 2007

%
% Download a couple of functions into the server
% All PostScript routines downloaded to the server begin with "_xvps_"
%
cdef pscanvas_init_canvas()
%
% Flip the coordinate system
%
/_xvps_flip_coords {	% - => -
    clippath pathbbox
    0 exch translate pop pop pop
    1 -1 scale
} def

%
% Re-define initgraphics. If an initgraphics is allowed to go through,
% the canvas ends up in the XView coordinate system
%
/_xvps_initgraphics { initgraphics } def
/initgraphics {		    		% - => -
    _xvps_initgraphics
    currentcanvas dup /XID known {
	/XID get 0 ne {
	    _xvps_flip_coords
	} if
    }{
	pop
    } ifelse
} def

%
% Scroll the canvas and return the current clipping path to the client
%
#ifdef	DEBUG
/_xvps_scroll {			% dx dy => x y w h
    console (_xvps_scroll: entered\n) writestring
    clippath 2 copy copyarea		% dx dy
    currentcanvas false getbbox		% dx dy 0 0 w h
    2 copy 6 2 roll			% dx dy w h 0 0 w h
    rectpath				% dx dy w h
    rectpath				% -
    eoclip
    %
    % Get the current clipping path, transform it into device coordinates, and
    % leave them on the stack so that the client can pick them up
    %
    clippath pathbbox	    	    	% x0 y0 x1 y1
    points2rect				% x y w h
    %
    % Print out the pathbbox
    %
    4 copy 4 array astore console exch
    (_xvps_scroll: pathbbox: \(x=% y=% w=% h=%\)\n)
    exch fprintf
    %
    % Transform the do-hickeys into device space, and cvi them
    %
    dtransform	    	    	    	% x y w' h'
    cvi 4 1 roll			% h' x y w'
    cvi 4 1 roll			% w' h' x y
    transform				% w' h' x' y'
    cvi 4 1 roll			% y' w' h' x'
    cvi 4 1 roll			% x' y' w' h'
    %
    % Print it out again
    %
    4 copy 4 array astore console exch
    (_xvps_scroll: pathbbox = \(x'=% y'=% w'=% h'=%\)\n)
    exch fprintf
} def
#else
/_xvps_scroll {			% dx dy => x y w h
    clippath 2 copy copyarea		% dx dy
    currentcanvas false getbbox		% dx dy 0 0 w h
    2 copy 6 2 roll			% dx dy w h 0 0 w h
    rectpath				% dx dy w h
    rectpath				% -
    eoclip
    %
    % Get the current clipping path, transform it into device coordinates, and
    % leave them on the stack so that the client can pick them up
    %
    clippath pathbbox	    	    	% x0 y0 x1 y1
    points2rect				% x y w h
    %
    % Transform the do-hickeys into device space, cvi them, and leave
    % them on the stack
    %
    dtransform	    	    	    	% x y w' h'
    cvi 4 1 roll			% h' x y w'
    cvi 4 1 roll			% w' h' x y
    transform				% w' h' x' y'
    cvi 4 1 roll			% y' w' h' x'
    cvi 4 1 roll			% x' y' w' h'
} def
#endif


%
% Print warning message on the canvas
%
/_xvps_PrintErrorMsg {	    	% errString => -
    %
    % Print errString on the console
    %
    console exch writestring		% -
    %
    % Warn the user about the error
    %
    gsave
    newpath currentcanvas false getbbox rectpath
    1.0 setgray fill
    %
    % Print Warning on the canvas ...
    %
    .75 setgray				% set a nice gray
    /Courier-Bold findfont 32 scalefont setfont
    currentcanvas false getbbox	    	% 0 0 w h
    4 1 roll pop pop pop .5 mul		% .5h
    currentfont fontheight 1.2 mul exch	% 1.2fontheight .5h
    translate 0 0 moveto		% -
    (Warning: NeWS/PostScript Error!) show
    %
    % Print error message asking the user to look at the console
    %
    0 currentfont fontheight -1.2 mul  % -1.2fontheight
    translate 0 0 moveto	       % -
    (See console for stack trace) show
    grestore
} def

%
% End of initialization
%

%
% Define a NeWS canvas, given an XID.  Return a NeWS token
%
cdef pscanvas_token_from_xid(int xid, tokvalue) => VALUETAG(tokvalue) 
    % find the NeWS canvas given the xid; pass back a token or -1 (failed)
    %
    % XXX - This assumes OWV3!!
    %
    xid
    /X11 3 0 findpackage beginpackage
	xlookupid
    endpackage
    { dup type /canvastype ne  { pop -1 } if } { -1 } ifelse

    currentfile countfileinputtoken dup     % canvas token token
    3 -2 roll				    % token canvas token
    setfileinputtoken			    % token
    VALUETAG tagprint
    typedprint
%
%
% Return a token, given some PostScript code.  If the code given is
% not an object, return -1
%
% - => -
cdef pscanvas_token_from_code(postscript Code,tokvalue) => VALUETAG(tokvalue)
    currentfile countfileinputtoken	    	    % token
    dup mark					    % token token mark
    Code counttomark 1 ne
    { cleartomark pop pop -1 }	    	    	    % code is not an object
    { exch pop exch setfileinputtoken }   % token
    ifelse
    VALUETAG tagprint
    typedprint

%
% Useful PostScript Definitions 
%

% - => -
cdef pscanvas_flip()
_xvps_flip_coords

cdef ps_newpath()
    newpath

% - => -
cdef ps_clipcanvas()
    clipcanvas

% - => -
cdef ps_do_fillcanvas()
%    fillcanvas
    pop


%
% Before doing anything else, establish our package
% stack.
%
cdef ps_init_connection()
/owV3? systemdict /findpackage known def
owV3? {
    /NeWS	3 0 findpackage beginpackage
    /X11	3 0 findpackage beginpackage
} if



%
% Initialize some stuff, and start the server loop
%
cdef pscanvas_start_server_loop()
%
% Make sure we get the correct type of error messages
%
currentprocess /ErrorDetailLevel 1 put
%
% Make NeWS ignore timeouts
%
%statusdict begin
%    0 setjobtimeout
%end
%
% Load up some packages if we are under OW3.0
%

owV3? {
    /fillcanvas {		% grayvalue => -
	gsave
	    setgray
	    clipcanvaspath setgray fill
	    fill
	grestore
    } def
} if
%
% token 0 is a Boolean		- true, if an error has occurred
% token 1 is a string		- points to the error message string
% token 2 is a save object	- points to the initial VM state
%
false	0 setfileinputtoken
()	1 setfileinputtoken
save	2 setfileinputtoken
%
% Start the server loop
%
{
    {
	currentfile cvx exec
    }
    stopped {
	clear				% clear the stack
	(PScanvas NeWS Error:\n***ERROR***\n%*****)
	[
	    $error /message get dup null eq
	    {
		pop ( )
	    } if
	] sprintf   	    	    	% newErrStr
	dup _xvps_PrintErrorMsg    	% newErrStr
#ifdef undef
	dup length			% newErrStr nlen
	1 getfileinputtoken dup length	% newErrStr nlen oldErrStr olen
	%
	% If the old error string is big enough, copy the new error
	% string into it
	%
	3 -1 roll lt			% newErrStr oldErrStr
	{
	    pop	    	    	    	% newErrStr
	}
	{
	    copy    	    	    	% copy newErrStr in oldErrStr
	} ifelse
#endif
	1 setfileinputtoken 	    	% save a pointer to ErrStr
	true 0 setfileinputtoken	% an error has occurred
    }
    {   % gets here if the C side goes away...
	2 getfileinputtoken restore     % restore the VM state
	owV3?
	{
	    endpackage
	    endpackage
	} if
	exit				% exit the loop
    } ifelse
} loop
%
% Return the error string to client
%    
cdef pscanvas_return_error(string error, int flag) => VALUETAG(error, flag)
    VALUETAG tagprint
    0 getfileinputtoken
    {
	1 getfileinputtoken typedprint 1 typedprint
    }
    {
	( ) typedprint 0 typedprint
    }
    ifelse
    false 0 setfileinputtoken
    () 1 setfileinputtoken
	
%
% NeWS Canvas Operations
% - => -
cdef ps_canvastobottom(token Can)
    Can canvastobottom

% - => -
cdef ps_canvastotop(token Can)
    Can canvastotop

% - => -
cdef ps_createoverlay(token Can,tokvalue) => VALUETAG(tokvalue)
    currentfile countfileinputtoken	    	    % token
    dup Can createoverlay exch setfileinputtoken	    % token
    VALUETAG tagprint
    typedprint

% - => -
cdef ps_eoreshapecanvas(token Can)
    Can eoreshapecanvas

% - => -
cdef ps_getcanvaslocation(token Can,x,y) => VALUETAG(x,y)
    Can getcanvaslocation
    VALUETAG tagprint
    typedprint typedprint

% - => -
cdef ps_imagecanvas(token Can)
    Can imagecanvas

% - => -
cdef ps_imagemaskcanvas(int Bool, token Can)
    % C program uses TRUE and FALSE ints,  convert to boolean
    Bool 1 eq Can imagemaskcanvas

% - => -
cdef ps_insertcanvasabove(token Can,int x,int y)
    Can x y insertcanvasabove

% - => -
cdef ps_insertcanvasbelow(token Can,int x,int y)
    Can x y insertcanvasbelow

% - => -
cdef ps_movecanvas(int x,int y,token Can)
    x y Can movecanvas

% - => -
cdef ps_newcanvas(token Parent, tokvalue) => VALUETAG(tokvalue)
    currentfile countfileinputtoken	    	    % token
    dup Parent newcanvas exch setfileinputtoken	    % token
    VALUETAG tagprint
    typedprint

% - => -
cdef ps_reshapecanvas(token Can)
    Can reshapecanvas

% - => -
cdef ps_setcanvas(token Can)
    Can setcanvas

% - => -
cdef ps_clear_cliprects()
newpath clipcanvas

%
% Definitions needed specifically for XView/NeWS Library
%

% canvas => -
%cdef pscanvas_init_transform()
%  % Then reshape the canvas to its original shape in order to
%  % reset the canvas transform.

%    initclip clippath pathbbox
%    2 index add				% x y w <d to origin>
%    [1.0 0.0 0.0 -1.0 0.0 0.0] dup 5	% x y w d m m 5
%    4 -1 roll put setmatrix pop pop pop
%    pathbbox points2rect rectpath currentcanvas reshapecanvas

% - => -
%cdef pscanvas_fix_translation(yt)
%console (fix_trans\n) fprintf
%  0 yt idtransform translate

% dx, dy => x y w h
cdef pscanvas_scroll_by(dx, dy)
dx dy _xvps_scroll

cdef pscanvas_getint(tag, anint) => tag(anint)
  tag tagprint typedprint

% - => -
cdef pscanvas_rectpath(x, y, w, h)
  % set a rectangular path given x, y, w, h in pixel coordinates:
  % useful for setting a clippath on WIN_REPAINT.
  x y itransform w h idtransform rectpath
