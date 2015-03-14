"======================================================================
|
|   SystemDictionary Method Definitions
|
 ======================================================================"


"======================================================================
|
| Copyright (C) 1990, 1991 Free Software Foundation, Inc.
| Written by Steve Byrne.
|
| This file is part of GNU Smalltalk.
|
| GNU Smalltalk is free software; you can redistribute it and/or modify it
| under the terms of the GNU General Public License as published by the Free
| Software Foundation; either version 1, or (at your option) any later version.
| 
| GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
| FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
| details.
| 
| You should have received a copy of the GNU General Public License along with
| GNU Smalltalk; see the file LICENSE.  If not, write to the Free Software
| Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
|
 ======================================================================"


"
|     Change Log
| ============================================================================
| Author       Date       Change 
| sbyrne      8 Jul 89	  Created.
|
"

Behavior defineCFunc: 'system'
         withSelectorArgs: 'system: aString'
	 forClass: SystemDictionary
	 returning: #int
	 args: #(string)!

Behavior defineCFunc: 'getenv'
         withSelectorArgs: 'getenv: aString'
	 forClass: SystemDictionary
	 returning: #string
	 args: #(string)!

"Below are object and method definitions used to create a simple interface
to SunView windws from GNU Smalltalk."

Smalltalk at: #SuntoolObjectTypes put: Dictionary new!

Behavior defineCFunc: 'lookupCAddr'
    	 withSelectorArgs: 'lookupCAddr: aString'
	 forClass: SystemDictionary
	 returning: #cObject
	 args: #(string)!

!Dictionary methodsFor: 'testing'!

findDefsFor: anArray
    anArray do:
	[ :elt | self at: (elt at: 1)
		      put: (Smalltalk lookupCAddr: (elt at: 2)) ]
!!

Smalltalk addInit:
    [ SuntoolObjectTypes findDefsFor: #(
		(FrameType	    	'FRAME')
		(PanelType	    	'PANEL')
		(PanelMessageType	'PANEL_MESSAGE')
		(PanelButtonType	'PANEL_BUTTON')
		(PanelChoiceType	'PANEL_CHOICE')
		(PanelSliderType	'PANEL_SLIDER')
		(PanelTextType		'PANEL_TEXT')
		(PanelToggleType	'PANEL_TOGGLE')
		(PanelLineType		'PANEL_LINE')
		)
	] !


Smalltalk at: #PanelAttrs put: Dictionary new!

PanelAttrs findDefsFor: #(
    (panelItemX                     'PANEL_ITEM_X')
    (panelItemXGap                  'PANEL_ITEM_X_GAP')
    (panelLabelX                    'PANEL_LABEL_X')
    (panelValueX                    'PANEL_VALUE_X')
    (panelSliderWidth               'PANEL_SLIDER_WIDTH')
    (panelWidth                     'PANEL_WIDTH')
    (panelChoiceX                   'PANEL_CHOICE_X')
    (panelMarkX                     'PANEL_MARK_X')
    (panelChoiceXs                  'PANEL_CHOICE_XS')
    (panelMarkXs                    'PANEL_MARK_XS')
    (panelItemY                     'PANEL_ITEM_Y')
    (panelItemYGap                  'PANEL_ITEM_Y_GAP')
    (panelLabelY                    'PANEL_LABEL_Y')
    (panelValueY                    'PANEL_VALUE_Y')
    (panelHeight                    'PANEL_HEIGHT')
    (panelChoiceY                   'PANEL_CHOICE_Y')
    (panelMarkY                     'PANEL_MARK_Y')
    (panelChoiceYs                  'PANEL_CHOICE_YS')
    (panelMarkYs                    'PANEL_MARK_YS')
    (panelValue                     'PANEL_VALUE')
    (panelLabelDisplayLength        'PANEL_LABEL_DISPLAY_LENGTH')
    (panelValueDisplayLength        'PANEL_VALUE_DISPLAY_LENGTH')
    (panelValueStoredLength         'PANEL_VALUE_STORED_LENGTH')
    (panelMinValue                  'PANEL_MIN_VALUE')
    (panelMaxValue                  'PANEL_MAX_VALUE')
    (panelChoiceOffset              'PANEL_CHOICE_OFFSET')
    (panelTimerSecs                 'PANEL_TIMER_SECS')
    (panelTimerUsecs                'PANEL_TIMER_USECS')
    (panelMenuChoiceValues          'PANEL_MENU_CHOICE_VALUES')
    (panelValueUnderlined           'PANEL_VALUE_UNDERLINED')
    (panelItemBoxed                 'PANEL_ITEM_BOXED')
    (panelLabelBold                 'PANEL_LABEL_BOLD')
    (panelChoicesBold               'PANEL_CHOICES_BOLD')
    (panelShowItem                  'PANEL_SHOW_ITEM')
    (panelShowValue                 'PANEL_SHOW_VALUE')
    (panelShowRange                 'PANEL_SHOW_RANGE')
    (panelShowMenu                  'PANEL_SHOW_MENU')
    (panelShowMenuMark              'PANEL_SHOW_MENU_MARK')
    (panelChooseOne                 'PANEL_CHOOSE_ONE')
    (panelAdjustable                'PANEL_ADJUSTABLE')
    (panelBlinkCaret                'PANEL_BLINK_CARET')
    (panelAcceptKeystroke           'PANEL_ACCEPT_KEYSTROKE')
    (panelLabelShaded               'PANEL_LABEL_SHADED')
    (panelToggleValue               'PANEL_TOGGLE_VALUE')
    (panelNotifyLevel               'PANEL_NOTIFY_LEVEL')
    (panelDisplayLevel              'PANEL_DISPLAY_LEVEL')
    (panelLayout                    'PANEL_LAYOUT')
    (panelFeedback                  'PANEL_FEEDBACK')
    (panelPaint                     'PANEL_PAINT')
    (panelMouseState                'PANEL_MOUSE_STATE')
    (panelMaskChar                  'PANEL_MASK_CHAR')
    (panelLabelString               'PANEL_LABEL_STRING')
    (panelValueString               'PANEL_VALUE_STRING')
    (panelNotifyString              'PANEL_NOTIFY_STRING')
    (panelMenuTitleString           'PANEL_MENU_TITLE_STRING')
    (panelName                      'PANEL_NAME')
    (panelChoiceStrings             'PANEL_CHOICE_STRINGS')
    (panelMenuChoiceStrings         'PANEL_MENU_CHOICE_STRINGS')
    (panelChoiceString              'PANEL_CHOICE_STRING')
    (panelLabelImage                'PANEL_LABEL_IMAGE')
    (panelMenuTitleImage            'PANEL_MENU_TITLE_IMAGE')
    (panelMenuMarkImage             'PANEL_MENU_MARK_IMAGE')
    (panelMenuNomarkImage           'PANEL_MENU_NOMARK_IMAGE')
    (panelTypeImage                 'PANEL_TYPE_IMAGE')
    (panelChoiceImages              'PANEL_CHOICE_IMAGES')
    (panelMenuChoiceImages          'PANEL_MENU_CHOICE_IMAGES')
    (panelMarkImages                'PANEL_MARK_IMAGES')
    (panelNomarkImages              'PANEL_NOMARK_IMAGES')
    (panelChoiceImage               'PANEL_CHOICE_IMAGE')
    (panelMarkImage                 'PANEL_MARK_IMAGE')
    (panelNomarkImage               'PANEL_NOMARK_IMAGE')
    (panelLabelFont                 'PANEL_LABEL_FONT')
    (panelValueFont                 'PANEL_VALUE_FONT')
    (panelMenuTitleFont             'PANEL_MENU_TITLE_FONT')
    (panelFont                      'PANEL_FONT')
    (panelChoiceFonts               'PANEL_CHOICE_FONTS')
    (panelMenuChoiceFonts           'PANEL_MENU_CHOICE_FONTS')
    (panelNotifyProc                'PANEL_NOTIFY_PROC')
    (panelTimerProc                 'PANEL_TIMER_PROC')
    (panelEventProc                 'PANEL_EVENT_PROC')
    (panelBackgroundProc            'PANEL_BACKGROUND_PROC')
    (panelItemRect                  'PANEL_ITEM_RECT')
    (panelPixwin                    'PANEL_PIXWIN')
    (panelClientData                'PANEL_CLIENT_DATA')
    (panelVerticalScrollbar         'PANEL_VERTICAL_SCROLLBAR')
    (panelHorizontalScrollbar       'PANEL_HORIZONTAL_SCROLLBAR')
    (panelCursor                    'PANEL_CURSOR')
    (panelCaretItem                 'PANEL_CARET_ITEM')
    (panelFirstItem                 'PANEL_FIRST_ITEM')
    (panelNextItem                  'PANEL_NEXT_ITEM')
    (panelParentPanel               'PANEL_PARENT_PANEL')
)
!

Smalltalk at: #FrameAttrs put: Dictionary new!

FrameAttrs findDefsFor: #(
    (frameClosed                    'FRAME_CLOSED')
    (frameInheritColors             'FRAME_INHERIT_COLORS')
    (frameShowLabel                 'FRAME_SHOW_LABEL')
    (frameSubwindowsAdjustable      'FRAME_SUBWINDOWS_ADJUSTABLE')
    (frameLabel                     'FRAME_LABEL')
    (frameForegroundColor           'FRAME_FOREGROUND_COLOR')
    (frameBackgroundColor           'FRAME_BACKGROUND_COLOR')
    (frameIcon                      'FRAME_ICON')
    (frameClosedX                   'FRAME_CLOSED_X')
    (frameClosedY                   'FRAME_CLOSED_Y')
    (frameClosedRect                'FRAME_CLOSED_RECT')
    (frameCurrentRect               'FRAME_CURRENT_RECT')
    (frameOpenRect                  'FRAME_OPEN_RECT')
    (frameArgs                      'FRAME_ARGS')
    (frameArgcPtrArgv               'FRAME_ARGC_PTR_ARGV')
    (frameCmdlineHelpProc           'FRAME_CMDLINE_HELP_PROC')
    (frameBorderStyle               'FRAME_BORDER_STYLE')
    (frameEmboldenLabel             'FRAME_EMBOLDEN_LABEL')
    (frameNthWindow                 'FRAME_NTH_WINDOW')
    (frameNthSubwindow              'FRAME_NTH_SUBWINDOW')
    (frameNthSubframe               'FRAME_NTH_SUBFRAME')
    (frameDoneProc                  'FRAME_DONE_PROC')
    (frameDefaultDoneProc           'FRAME_DEFAULT_DONE_PROC')
    (frameNoConfirm                 'FRAME_NO_CONFIRM')
    (frameNullAttr                  'FRAME_NULL_ATTR')
)
!

Smalltalk at: #CanvasAttrs put: Dictionary new!

CanvasAttrs findDefsFor: #(
    (canvasPixwin                   'CANVAS_PIXWIN')
    (canvasLeft                     'CANVAS_LEFT')
    (canvasTop                      'CANVAS_TOP')
    (canvasWidth                    'CANVAS_WIDTH')
    (canvasHeight                   'CANVAS_HEIGHT')
    (canvasDepth                    'CANVAS_DEPTH')
    (canvasMargin                   'CANVAS_MARGIN')
    (canvasRetained                 'CANVAS_RETAINED')
    (canvasFixedImage               'CANVAS_FIXED_IMAGE')
    (canvasRepaintProc              'CANVAS_REPAINT_PROC')
    (canvasResizeProc               'CANVAS_RESIZE_PROC')
    (canvasAutoClear                'CANVAS_AUTO_CLEAR')
    (canvasAutoExpand               'CANVAS_AUTO_EXPAND')
    (canvasAutoShrink               'CANVAS_AUTO_SHRINK')
    (canvasFastMono                 'CANVAS_FAST_MONO')
)
!

Smalltalk at: #WindowAttrs put: Dictionary new!

WindowAttrs findDefsFor: #(
    (winErrorMsg                    'WIN_ERROR_MSG')
    (winX                           'WIN_X')
    (winY                           'WIN_Y')
    (winRightOf                     'WIN_RIGHT_OF')
    (winBelow                       'WIN_BELOW')
    (winWidth                       'WIN_WIDTH')
    (winHeight                      'WIN_HEIGHT')
    (winPercentWidth                'WIN_PERCENT_WIDTH')
    (winPercentHeight               'WIN_PERCENT_HEIGHT')
    (winColumns                     'WIN_COLUMNS')
    (winRows                        'WIN_ROWS')
    (winRect                        'WIN_RECT')
    (winScreenRect                  'WIN_SCREEN_RECT')
    (winFitHeight                   'WIN_FIT_HEIGHT')
    (winFitWidth                    'WIN_FIT_WIDTH')
    (winShow                        'WIN_SHOW')
    (winMenu                        'WIN_MENU')
    (winCursor                      'WIN_CURSOR')
    (winHorizontalScrollbar         'WIN_HORIZONTAL_SCROLLBAR')
    (winVerticalScrollbar           'WIN_VERTICAL_SCROLLBAR')
    (winEventProc                   'WIN_EVENT_PROC')
    (winInputDesignee               'WIN_INPUT_DESIGNEE')
    (winGrabAllInput                'WIN_GRAB_ALL_INPUT')
    (winKbdInputMask                'WIN_KBD_INPUT_MASK')
    (winConsumeKbdEvent             'WIN_CONSUME_KBD_EVENT')
    (winIgnoreKbdEvent              'WIN_IGNORE_KBD_EVENT')
    (winConsumeKbdEvents            'WIN_CONSUME_KBD_EVENTS')
    (winIgnoreKbdEvents             'WIN_IGNORE_KBD_EVENTS')
    (winPickInputMask               'WIN_PICK_INPUT_MASK')
    (winConsumePickEvent            'WIN_CONSUME_PICK_EVENT')
    (winIgnorePickEvent             'WIN_IGNORE_PICK_EVENT')
    (winConsumePickEvents           'WIN_CONSUME_PICK_EVENTS')
    (winIgnorePickEvents            'WIN_IGNORE_PICK_EVENTS')
    (winFont                        'WIN_FONT')
    (winTopMargin                   'WIN_TOP_MARGIN')
    (winBottomMargin                'WIN_BOTTOM_MARGIN')
    (winLeftMargin                  'WIN_LEFT_MARGIN')
    (winRightMargin                 'WIN_RIGHT_MARGIN')
    (winRowHeight                   'WIN_ROW_HEIGHT')
    (winColumnWidth                 'WIN_COLUMN_WIDTH')
    (winRowGap                      'WIN_ROW_GAP')
    (winColumnGap                   'WIN_COLUMN_GAP')
    (winType                        'WIN_TYPE')
    (winOwner                       'WIN_OWNER')
    (winName                        'WIN_NAME')
    (winClientData                  'WIN_CLIENT_DATA')
    (winImplData                    'WIN_IMPL_DATA')
    (winFd                          'WIN_FD')
    (winDeviceName                  'WIN_DEVICE_NAME')
    (winDeviceNumber                'WIN_DEVICE_NUMBER')
    (winPixwin                      'WIN_PIXWIN')
    (winMouseXy                     'WIN_MOUSE_XY')
    (winShowUpdates                 'WIN_SHOW_UPDATES')
    (winObject                      'WIN_OBJECT')
    (winGetProc                     'WIN_GET_PROC')
    (winSetProc                     'WIN_SET_PROC')
    (winPresetProc                  'WIN_PRESET_PROC')
    (winPostsetProc                 'WIN_POSTSET_PROC')
    (winLayoutProc                  'WIN_LAYOUT_PROC')
    (winNotifyDestroyProc           'WIN_NOTIFY_DESTROY_PROC')
    (winNotifyEventProc             'WIN_NOTIFY_EVENT_PROC')
    (winDefaultEventProc            'WIN_DEFAULT_EVENT_PROC')
    (winCreated                     'WIN_CREATED')
    (winCompatibility               'WIN_COMPATIBILITY')
    (winCompatibilityInfo           'WIN_COMPATIBILITY_INFO')
    (winRegister                    'WIN_REGISTER')
    (winNotifyInfo                  'WIN_NOTIFY_INFO')
    (winNullAttr                    'WIN_NULL_ATTR')
    (winKbdFocus                    'WIN_KBD_FOCUS')
    (winEventState                  'WIN_EVENT_STATE')
)
!

Smalltalk at: #TextswAttrs put: Dictionary new!

TextswAttrs findDefsFor: #(
    (textswAdjustIsPendingDelete    'TEXTSW_ADJUST_IS_PENDING_DELETE')
    (textswAgainLimit               'TEXTSW_AGAIN_LIMIT')
    (textswAgainRecording           'TEXTSW_AGAIN_RECORDING')
    (textswAutoIndent               'TEXTSW_AUTO_INDENT')
    (textswAutoScrollBy             'TEXTSW_AUTO_SCROLL_BY')
    (textswBlinkCaret               'TEXTSW_BLINK_CARET')
    (textswBrowsing                 'TEXTSW_BROWSING')
    (textswCheckpointFrequency      'TEXTSW_CHECKPOINT_FREQUENCY')
    (textswClientData               'TEXTSW_CLIENT_DATA')
    (textswConfirmOverwrite         'TEXTSW_CONFIRM_OVERWRITE')
    (textswContents                 'TEXTSW_CONTENTS')
    (textswControlCharsUseFont      'TEXTSW_CONTROL_CHARS_USE_FONT')
    (textswDestroyAllViews          'TEXTSW_DESTROY_ALL_VIEWS')
    (textswDisableCd                'TEXTSW_DISABLE_CD')
    (textswDisableLoad              'TEXTSW_DISABLE_LOAD')
    (textswEditBackChar             'TEXTSW_EDIT_BACK_CHAR')
    (textswEditBackLine             'TEXTSW_EDIT_BACK_LINE')
    (textswEditBackWord             'TEXTSW_EDIT_BACK_WORD')
    (textswEditCount                'TEXTSW_EDIT_COUNT')
    (textswErrorMsg                 'TEXTSW_ERROR_MSG')
    (textswEsCreateProc             'TEXTSW_ES_CREATE_PROC')
    (textswFile                     'TEXTSW_FILE')
    (textswFont                     'TEXTSW_FONT')
    (textswHeight                   'TEXTSW_HEIGHT')
    (textswHistoryLimit             'TEXTSW_HISTORY_LIMIT')
    (textswIgnoreLimit              'TEXTSW_IGNORE_LIMIT')
    (textswInsertMakesVisible       'TEXTSW_INSERT_MAKES_VISIBLE')
    (textswInsertOnly               'TEXTSW_INSERT_ONLY')
    (textswInsertionPoint           'TEXTSW_INSERTION_POINT')
    (textswLength                   'TEXTSW_LENGTH')
    (textswLoadDirIsCd              'TEXTSW_LOAD_DIR_IS_CD')
    (textswLowerContext             'TEXTSW_LOWER_CONTEXT')
    (textswMemoryMaximum            'TEXTSW_MEMORY_MAXIMUM')
    (textswMenu                     'TEXTSW_MENU')
    (textswModified                 'TEXTSW_MODIFIED')
    (textswMultiClickSpace          'TEXTSW_MULTI_CLICK_SPACE')
    (textswMultiClickTimeout        'TEXTSW_MULTI_CLICK_TIMEOUT')
    (textswMustShowCaret            'TEXTSW_MUST_SHOW_CARET')
    (textswName                     'TEXTSW_NAME')
    (textswNameToUse                'TEXTSW_NAME_TO_USE')
    (textswNoResetToScratch         'TEXTSW_NO_RESET_TO_SCRATCH')
    (textswNoSelectionService       'TEXTSW_NO_SELECTION_SERVICE')
    (textswNotifyLevel              'TEXTSW_NOTIFY_LEVEL')
    (textswNotifyProc               'TEXTSW_NOTIFY_PROC')
    (textswPixwin                   'TEXTSW_PIXWIN')
    (textswReadOnly                 'TEXTSW_READ_ONLY')
    (textswResetMode                'TEXTSW_RESET_MODE')
    (textswStatus                   'TEXTSW_STATUS')
    (textswStoreChangesFile         'TEXTSW_STORE_CHANGES_FILE')
    (textswStoreSelfIsSave          'TEXTSW_STORE_SELF_IS_SAVE')
    (textswTabWidth                 'TEXTSW_TAB_WIDTH')
    (textswTool                     'TEXTSW_TOOL')
    (textswUpperContext             'TEXTSW_UPPER_CONTEXT')
    (textswWidth                    'TEXTSW_WIDTH')
    (textswCoalesceWith             'TEXTSW_COALESCE_WITH')
    (textswEndAllViews              'TEXTSW_END_ALL_VIEWS')
    (textswForAllViews              'TEXTSW_FOR_ALL_VIEWS')
    (textswFirst                    'TEXTSW_FIRST')
    (textswFirstLine                'TEXTSW_FIRST_LINE')
    (textswLeftMargin               'TEXTSW_LEFT_MARGIN')
    (textswLineBreakAction          'TEXTSW_LINE_BREAK_ACTION')
    (textswRightMargin              'TEXTSW_RIGHT_MARGIN')
    (textswScrollbar                'TEXTSW_SCROLLBAR')
    (textswUpdateScrollbar          'TEXTSW_UPDATE_SCROLLBAR')
    (textswSpare1                   'TEXTSW_SPARE_1')
    (textswSpare2                   'TEXTSW_SPARE_2')
)
!

Smalltalk at: #MenuAttrs put: Dictionary new!

MenuAttrs findDefsFor: #(
    (menuActionProc                 'MENU_ACTION_PROC')
    (menuAction                     'MENU_ACTION')
    (menuActionImage                'MENU_ACTION_IMAGE')
    (menuActionItem                 'MENU_ACTION_ITEM')
    (menuAppendItem                 'MENU_APPEND_ITEM')
    (menuAppend                     'MENU_APPEND')
    (menuBoxed                      'MENU_BOXED')
    (menuCenter                     'MENU_CENTER')
    (menuClientData                 'MENU_CLIENT_DATA')
    (menuColumnMajor                'MENU_COLUMN_MAJOR')
    (menuDefault                    'MENU_DEFAULT')
    (menuDefaultItem                'MENU_DEFAULT_ITEM')
    (menuDefaultImage               'MENU_DEFAULT_IMAGE')
    (menuDisableItem                'MENU_DISABLE_ITEM')
    (menuDefaultSelection           'MENU_DEFAULT_SELECTION')
    (menuDescendFirst               'MENU_DESCEND_FIRST')
    (menuFeedback                   'MENU_FEEDBACK')
    (menuFirstEvent                 'MENU_FIRST_EVENT')
    (menuFont                       'MENU_FONT')
    (menuGenPullright               'MENU_GEN_PULLRIGHT')
    (menuGenPullrightImage          'MENU_GEN_PULLRIGHT_IMAGE')
    (menuGenPullrightItem           'MENU_GEN_PULLRIGHT_ITEM')
    (menuGenProc                    'MENU_GEN_PROC')
    (menuGenProcImage               'MENU_GEN_PROC_IMAGE')
    (menuGenProcItem                'MENU_GEN_PROC_ITEM')
    (menuHeight                     'MENU_HEIGHT')
    (menuImage                      'MENU_IMAGE')
    (menuImageItem                  'MENU_IMAGE_ITEM')
    (menuImages                     'MENU_IMAGES')
    (menuInitialSelection           'MENU_INITIAL_SELECTION')
    (menuInitialSelectionSelected   'MENU_INITIAL_SELECTION_SELECTED')
    (menuInitialSelectionExpanded   'MENU_INITIAL_SELECTION_EXPANDED')
    (menuInactive                   'MENU_INACTIVE')
    (menuInsert                     'MENU_INSERT')
    (menuInsertItem                 'MENU_INSERT_ITEM')
    (menuItem                       'MENU_ITEM')
    (menuInvert                     'MENU_INVERT')
    (menuJumpAfterSelection         'MENU_JUMP_AFTER_SELECTION')
    (menuJumpAfterNoSelection       'MENU_JUMP_AFTER_NO_SELECTION')
    (menuLastEvent                  'MENU_LAST_EVENT')
    (menuLeftMargin                 'MENU_LEFT_MARGIN')
    (menuLike                       'MENU_LIKE')
    (menuMargin                     'MENU_MARGIN')
    (menuMenu                       'MENU_MENU')
    (menuNitems                     'MENU_NITEMS')
    (menuNotifyProc                 'MENU_NOTIFY_PROC')
    (menuNthItem                    'MENU_NTH_ITEM')
    (menuNcols                      'MENU_NCOLS')
    (menuNrows                      'MENU_NROWS')
    (menuParent                     'MENU_PARENT')
    (menuPullright                  'MENU_PULLRIGHT')
    (menuPullrightDelta             'MENU_PULLRIGHT_DELTA')
    (menuPullrightImage             'MENU_PULLRIGHT_IMAGE')
    (menuPullrightItem              'MENU_PULLRIGHT_ITEM')
    (menuRelease                    'MENU_RELEASE')
    (menuReleaseImage               'MENU_RELEASE_IMAGE')
    (menuRemove                     'MENU_REMOVE')
    (menuRemoveItem                 'MENU_REMOVE_ITEM')
    (menuReplace                    'MENU_REPLACE')
    (menuReplaceItem                'MENU_REPLACE_ITEM')
    (menuRightMargin                'MENU_RIGHT_MARGIN')
    (menuSelected                   'MENU_SELECTED')
    (menuSelectedItem               'MENU_SELECTED_ITEM')
    (menuShadow                     'MENU_SHADOW')
    (menuStayUp                     'MENU_STAY_UP')
    (menuString                     'MENU_STRING')
    (menuStringItem                 'MENU_STRING_ITEM')
    (menuStrings                    'MENU_STRINGS')
    (menuTitleItem                  'MENU_TITLE_ITEM')
    (menuTitleImage                 'MENU_TITLE_IMAGE')
    (menuType                       'MENU_TYPE')
    (menuValidResult                'MENU_VALID_RESULT')
    (menuValue                      'MENU_VALUE')
    (menuWidth                      'MENU_WIDTH')
    (menuNop                        'MENU_NOP')
    (menuButton                     'MENU_BUTTON')
    (menuFd                         'MENU_FD')
    (menuIe                         'MENU_IE')
    (menuPos                        'MENU_POS')
)
!

Smalltalk at: #IconAttrs put: Dictionary new!

IconAttrs findDefsFor: #(
    (iconX                          'ICON_X')
    (iconY                          'ICON_Y')
    (iconWidth                      'ICON_WIDTH')
    (iconHeight                     'ICON_HEIGHT')
    (iconImageRect                  'ICON_IMAGE_RECT')
    (iconLabelRect                  'ICON_LABEL_RECT')
    (iconImage                      'ICON_IMAGE')
    (iconLabel                      'ICON_LABEL')
    (iconFont                       'ICON_FONT')
)
!

