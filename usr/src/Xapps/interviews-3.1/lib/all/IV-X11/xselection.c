/*
 * Copyright (c) 1992 Stanford University
 * Copyright (c) 1992 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

#include "wtable.h"
#include <InterViews/display.h>
#include <InterViews/patch.h>
#include <InterViews/window.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/xselection.h>
#include <IV-X11/xwindow.h>
#include <OS/string.h>
#include <OS/table.h>
#include <X11/Xatom.h>

/*
 * SelectionManager -- inter client communications mechanism
 */

SelectionManager::SelectionManager(Display* d, const char* name) {
    rep_ = new SelectionManagerRep(d, String(name));
}

SelectionManager::SelectionManager(Display* d, const String& name) {
    rep_ = new SelectionManagerRep(d, name);
}

SelectionManager::~SelectionManager() {
    delete rep_;
}

void SelectionManager::own(
    SelectionHandler* convert, SelectionHandler* lose, SelectionHandler* done
) {
    SelectionManagerRep& s = *rep();
    Atom a = XInternAtom(s.xdisplay_, s.name_->string(), true);
    XSetSelectionOwner(s.xdisplay_, a, s.owner_->rep()->xwindow_, CurrentTime);
    Resource::ref(convert);
    Resource::unref(s.convert_);
    s.convert_ = convert;
    Resource::ref(lose);
    Resource::unref(s.lose_);
    s.lose_ = lose;
    Resource::ref(done);
    Resource::unref(s.done_);
    s.done_ = done;
}

void SelectionManager::put_value(const void* data, int length, int format) {
    SelectionManagerRep& s = *rep();
    XChangeProperty(
        s.xdisplay_, s.x_req_.requestor, s.x_req_.property,
	/* type */ XA_STRING, format, PropModeReplace,
	(const unsigned char*)data, length
    );
    XEvent xe;
    XSelectionEvent& xs = xe.xselection;
    xs.type = SelectionNotify;
    xs.requestor = s.x_req_.requestor;
    xs.selection = s.x_req_.selection;
    xs.target = s.x_req_.target;
    xs.property = s.x_req_.property;
    xs.time = s.x_req_.time;
    XSendEvent(s.xdisplay_, xs.requestor, False, 0, &xe);
}

void SelectionManager::retrieve(
    const String&, SelectionHandler*, SelectionHandler*
) {
    /* unimplemented */
}

void SelectionManager::get_value(
    String*&, void*&, int&, int&
) {
    /* unimplemented */
}

SelectionManagerRep* SelectionManager::rep() const { return rep_; }

/* class SelectionManagerRep */

SelectionManagerRep::SelectionManagerRep(Display* d, const String& name) {
    DisplayRep& dr = *d->rep();
    xdisplay_ = dr.display_;
    name_ = new CopyString(name);
    owner_ = new PopupWindow(new Patch(nil));
    WindowRep& wr = *owner_->rep();
    wr.display_ = d;
    wr.xwindow_ = XCreateSimpleWindow(
        xdisplay_, dr.root_, 0, 0, 1, 1, 0, 0, 0
    );
    dr.wtable_->insert(wr.xwindow_, owner_);
    wr.xtoplevel_ = wr.xwindow_;
    convert_ = nil;
    lose_ = nil;
    done_ = nil;
    ok_ = nil;
    fail_ = nil;
}

SelectionManagerRep::~SelectionManagerRep() {
    delete name_;
    delete owner_;
    Resource::unref(convert_);
    Resource::unref(lose_);
    Resource::unref(done_);
    Resource::unref(ok_);
    Resource::unref(fail_);
}

void SelectionManagerRep::request(
    SelectionManager* s, const XSelectionRequestEvent& x
) {
    if (convert_ != nil) {
	x_req_ = x;
	convert_->handle(s);
    }
}

void SelectionManagerRep::notify(SelectionManager*, const XSelectionEvent&) {
    /* unimplemented */
}

/* class SelectionHandler */

SelectionHandler::SelectionHandler(){}

SelectionHandler::~SelectionHandler(){}
