/*
 * $XConsortium: actions.h,v 1.9 91/07/17 21:20:49 converse Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

	/* from compfuncs.c */

extern void	XmhResetCompose();
extern void	XmhSend();
extern void	XmhSave();

	/* from folder.c */

extern void	XmhClose();
extern void	XmhComposeMessage();
extern void 	XmhOpenFolder();
extern void	XmhOpenFolderInNewWindow();
extern void	XmhCreateFolder();
extern void	XmhDeleteFolder();
extern void	XmhPopupFolderMenu();
extern void	XmhSetCurrentFolder();
extern void	XmhLeaveFolderButton();
extern void 	XmhPushFolder();
extern void	XmhPopFolder();
extern void	XmhWMProtocols();

	/* from msg.c */

extern void	XmhInsert();

	/* from popup.c */

extern void	XmhPromptOkayAction();

	/* from toc.c */

extern void	XmhPushSequence();
extern void	XmhPopSequence();
extern void	XmhReloadSeqLists();

	/* from tocfuncs.c */

extern void	XmhCheckForNewMail();
extern void	XmhIncorporateNewMail();
extern void	XmhCommitChanges();
extern void	XmhPackFolder();
extern void	XmhSortFolder();
extern void	XmhForceRescan();
extern void	XmhViewNextMessage();
extern void	XmhViewPreviousMessage();
extern void	XmhMarkDelete();
extern void	XmhMarkMove();
extern void	XmhMarkCopy();
extern void	XmhUnmark();
extern void	XmhViewInNewWindow();
extern void	XmhReply();
extern void	XmhForward();
extern void	XmhUseAsComposition();
extern void	XmhPrint();
extern void	XmhShellCommand();
extern void	XmhPickMessages();
extern void	XmhOpenSequence();
extern void	XmhAddToSequence();
extern void	XmhRemoveFromSequence();
extern void	XmhDeleteSequence();

	/* from viewfuncs.c */

extern void	XmhCloseView();
extern void	XmhViewReply();
extern void	XmhViewForward();
extern void	XmhViewUseAsComposition();
extern void	XmhEditView();
extern void	XmhSaveView();
extern void	XmhPrintView();
extern void	XmhViewMarkDelete();
