/* Resource and command-line junk
 *
 * xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.  No representations are made about the suitability of this
 * software for any purpose.  It is provided "as is" without express or 
 * implied warranty.
 */

static char *xkeycapsDefaults[] = {
  /*
   *  The default colors are specified rather completely here to avoid
   *  losing badly when the user specifies conflicting values for
   *  "*Foreground" and "*Background" in their personal resources.
   *  The downside of this is that it's more work for the user to change
   *  the colors of XKeyCaps.  If you want white-on-black, instead of
   *  the black-on-light-shades-of-gray default configuration, do
   *  something like
   *			XKeyCaps*Background: black
   *			XKeyCaps*Foreground: white
   *			XKeyCaps*BorderColor: black
   *
   *  "xkeycaps -fg white -bg black -bd white" will do the same thing.
   *
   */
  "XKeyCaps*Foreground:				black",
  "XKeyCaps*borderColor:			black",
  "XKeyCaps*background: 			gray88",
  "XKeyCaps*Command.background:			gray93",
  "XKeyCaps*MenuButton.background:		gray93",
  "XKeyCaps*Toggle.background:			gray93",
  "XKeyCaps*Key.background:			gray93",
  "XKeyCaps*Key.highlight:			white",
  "XKeyCaps*editKey*label.foreground:		gray93",
  "XKeyCaps*editKey*label.background:		black",
  "XKeyCaps*editKey*Toggle.background:		gray88",
  "XKeyCaps*editKey*Viewport*background:	gray93",
  "XKeyCaps*editKey*autoRepeatValue.background:	gray88",

  "*Paned*showGrip:		false",
  "*Paned.borderWidth:		0",
  "*Paned.internalBorderWidth:	0",
  "*buttons.borderWidth:	0",
  "*info.borderWidth:		0",
  "*Keyboard.borderWidth:	0",
  "*Key.borderWidth:		1",
  /* Why is this one necessary? */
  "*horizontal.internalBorderWidth:	0",

  "*info.defaultDistance:	 0",
  "*info.labels.defaultDistance: 1",
  "*info.labels.borderWidth:	 0",
  "*info.line.vSpace:		 0",
  "*info.line.borderWidth:	 0",
  "*Label*internalWidth:	 3",
  "*Label*internalHeight:	 0",
  "*Label.borderWidth:		 0",

  "*editKey*Viewport.borderWidth:	1",
  "*editKey*Viewport.forceBars:		true",
  "*editKey*Viewport.allowVert:		true",
  "*editKey*List.defaultColumns:	1",
  "*editKey*List.forceColumns:		true",
  "*editKey*Toggle*internalWidth:	3",
  "*editKey*Toggle*internalHeight:	3",
  "*editKey*Toggle*vertDistance:	0",
  "*editKey*Toggle.borderWidth:		0",
  "*editKey*keysymLine.borderWidth:	0",
  "*editKey*label.internalHeight:	5",

  "*editKey*keysymBox*defaultDistance:		0",
  "*editKey*keysymLine.vSpace:			0",
  "*editKey*keysymBox*Label.internalHeight:	3",
  "*editKey*keysymBox*symsOfCode.internalWidth: 15",
  "*editKey*keysymBox*symsOfCode.internalHeight:5",
  "*editKey*keysymBox*Label.internalHeight:	0",
  "*editKey*Toggle*internalHeight:		1",
  "*editKey*spacer.height:			5",

  "*editKey*autoRepeatValue.internalHeight:	0",
  "*editKey*autoRepeatValue.shapeStyle:	rectangle",
  "*editKey*autoRepeatValue.borderWidth:	0",

  "*editKey*modifierBox*internalHeight:	 1",
  "*editKey*List.internalWidth:		 5",
  "*editKey*List.columnSpacing:		 10",
  "*editKey*List.rowSpacing:		 0",

  "*buttons.defaultDistance:		1",
  "*Command.shapeStyle:			oval",
  "*buttons.MenuButton.shapeStyle:	oval",
  "*Command.internalWidth:		5",
  "*buttons.MenuButton.internalWidth:	5",
  "*buttons.MenuButton.internalHeight:	1",
  "*Command.internalHeight:		1",
  "*Command.borderWidth:		1",
  "*buttons.MenuButton.borderWidth:	1",
  "*keyboardMenu*leftMargin:		15",
  "*keyboardMenu*rightMargin:		15",
  
  "*labels.Label.font:		*-helvetica-bold-r-*-*-*-100-*-*-*-*-*-*",
  "*Label.font: 		*-courier-medium-r-*-*-*-120-*-*-*-*-*-*",
  "*info*message.font:		*-helvetica-medium-r-*-*-*-100-*-*-*-*-*-*",
  "*info*message2.font:		*-helvetica-medium-r-*-*-*-100-*-*-*-*-*-*",
  "*Command.font:		*-helvetica-bold-r-*-*-*-100-*-*-*-*-*-*",
  "*keyMenu*font:		*-helvetica-bold-r-*-*-*-100-*-*-*-*-*-*",
  "*keyMenu.menuLabel.font:	*-helvetica-bold-o-*-*-*-120-*-*-*-*-*-*",
  "*modifiers*label.font:	*-helvetica-bold-r-*-*-*-140-*-*-*-*-*-*",
  "*modifiers*Toggle.font:	*-helvetica-bold-r-*-*-*-120-*-*-*-*-*-*",
  "*modifiers*Command.font:	*-helvetica-bold-r-*-*-*-140-*-*-*-*-*-*",
  "*buttons.MenuButton.font:	*-helvetica-bold-r-*-*-*-100-*-*-*-*-*-*",
  "*buttons.Command.font:	*-helvetica-bold-r-*-*-*-100-*-*-*-*-*-*",
  "*keyboardMenu*font:		*-helvetica-bold-r-*-*-*-100-*-*-*-*-*-*",

  "*editKey*buttons*font:	*-helvetica-bold-r-*-*-*-140-*-*-*-*-*-*",
  "*editKey*label.font:		*-helvetica-bold-r-*-*-*-140-*-*-*-*-*-*",
  "*editKey*Label.font:		*-helvetica-bold-r-*-*-*-100-*-*-*-*-*-*",
  "*editKey*Toggle.font:	*-helvetica-medium-r-*-*-*-100-*-*-*-*-*-*",
  "*editKey*List*font:		*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*",
  "*editKey*autoRepeatValue.font:*-helvetica-medium-r-*-*-*-100-*-*-*-*-*-*",

  "*buttons.quit.label:			Quit",
  "*buttons.focus.label:		Type At Window",
  "*buttons.keyboard.label:		Select Keyboard",
  "*buttons.restore.label:		Restore Default Map",
  "*buttons.write.label:		Write Output",

  "*keyMenu.editKeysyms.label:		Edit KeySyms of Key",
  "*keyMenu.swapKey.label:		Exchange Keys",
  "*keyMenu.cloneKey.label:		Duplicate Key",
  "*keyMenu.disableKey.label:		Disable Key",
  "*keyMenu.restoreKey.label:		Restore Key to Default",

  "*editKey*keysym1.label:		KeySym 1:",
  "*editKey*keysym2.label:		KeySym 2:",
  "*editKey*keysym3.label:		KeySym 3:",
  "*editKey*keysym4.label:		KeySym 4:",
  "*editKey*keysym5.label:		KeySym 5:",
  "*editKey*keysym6.label:		KeySym 6:",
  "*editKey*keysym7.label:		KeySym 7:",
  "*editKey*keysym8.label:		KeySym 8:",
  "*editKey*autoRepeat.label:		AutoRepeat:",
  "*editKey*symsOfCode.label:		KeySyms of KeyCode:",
  "*editKey*modifiers.label:		Modifiers:",
  "*editKey*allKeySets.label:		Character Set:",
  "*editKey*keySymsOfSet.label:		KeySym:",
  "*editKey*modifierBox.modShift.label:	Shift",
  "*editKey*modifierBox.modControl.label:Control",
  "*editKey*modifierBox.modLock.label:	Lock",

  /* Why is this necessary? */
  "*TransientShell*Paned.borderWidth:	0",

  "*writeQuery*label.font:	*-helvetica-bold-r-*-*-*-140-*-*-*-*-*-*",
  "*restoreQuery*label.font:	*-helvetica-bold-r-*-*-*-140-*-*-*-*-*-*",
  "*writeQuery*Command.font:	*-helvetica-bold-r-*-*-*-140-*-*-*-*-*-*",
  "*restoreQuery*Command.font:	*-helvetica-bold-r-*-*-*-140-*-*-*-*-*-*",

  "*writeQuery*label.label:		\\n\
Write an xmodmap file for all keys, or only\\n\
for keys which differ from the default?",

  "*writeQuery*full.label:		All Keys",
  "*writeQuery*partial.label:		Changed Keys",
  "*writeQuery*abort.label:		Cancel",

  "*restoreQuery*label.label:		\\n\
Restore Default Keymap?\\n\\n\
If you are not really using the kind of keyboard\\n\
that is displayed, you will lose big.",

  "*restoreQuery*yes.label:		Restore",
  "*restoreQuery*no.label:		Cancel",

  "*restoreQuery*label.internalWidth:	20",
  "*restoreQuery*label.internalHeight:	0",
  "*restoreQuery*buttons.hSpace:	20",
  "*restoreQuery*buttons.vSpace:	20",

  "*writeQuery*label.internalWidth:	20",
  "*writeQuery*label.internalHeight:	0",
  "*writeQuery*buttons.hSpace:		20",
  "*writeQuery*buttons.vSpace:		20",

  NULL
};



static XrmOptionDescRec options [] = {
  { "-foreground",	"*Foreground",			XrmoptionSepArg, 0 },
  { "-background",	"*Background",			XrmoptionSepArg, 0 },
  { "-fg",		"*Foreground",			XrmoptionSepArg, 0 },
  { "-bg",		"*Background",			XrmoptionSepArg, 0 },
  { "-gutterwidth",	"*Keyboard.Key.gutterWidth",	XrmoptionSepArg, 0 },
  { "-gw",		"*Keyboard.Key.gutterWidth",	XrmoptionSepArg, 0 },
  { "-font",		"*Keyboard.Key.keycapFont",	XrmoptionSepArg, 0 },
  { "-fn",		"*Keyboard.Key.keycapFont",	XrmoptionSepArg, 0 },
  { "-keyboard",	"*Keyboard.keyboard",		XrmoptionSepArg, 0 },
#ifdef HAVE_XTRAP
  { "-xtrap",		"*useXTrap",			XrmoptionNoArg, "on" },
  { "-use-xtrap",	"*useXTrap",			XrmoptionNoArg, "on" },
  { "-use_xtrap",	"*useXTrap",			XrmoptionNoArg, "on" },
  { "-no-xtrap",	"*useXTrap",			XrmoptionNoArg,"off" },
  { "-no_xtrap",	"*useXTrap",			XrmoptionNoArg,"off" },
#endif
  { "-kbd",		"*Keyboard.keyboard",		XrmoptionSepArg, 0 }
};
