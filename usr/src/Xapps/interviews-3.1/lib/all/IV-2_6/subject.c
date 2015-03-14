/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
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

/*
 * Implementation of subject base class.
 */

#include <IV-2_6/InterViews/interactor.h>
#include <IV-2_6/InterViews/subject.h>
#include <OS/list.h>

declarePtrList(ViewList,Interactor)
implementPtrList(ViewList,Interactor)

class SubjectRep {
private:
    friend class Subject;

    ViewList views_;
};

Subject::Subject() {
    rep_ = new SubjectRep;
}

Subject::~Subject() {
    delete rep_;
    rep_ = nil;
}

void Subject::Attach(Interactor* v) {
    rep_->views_.append(v);
    ref();
}

void Subject::Detach(Interactor* v) {
    ViewList& vl = rep_->views_;
    for (unsigned int i = 0; i < vl.count(); i++) {
	if (vl.item(i) == v) {
	    vl.remove(i);
	    unref();
	    break;
	}
    }
}

void Subject::Notify() {
    ViewList& vl = rep_->views_;
    for (unsigned int i = 0; i < vl.count(); i++) {
	vl.item(i)->Update();
    }
}


boolean Subject::IsView(Interactor* v) {
    ViewList& vl = rep_->views_;
    for (unsigned int i = 0; i < vl.count(); i++) {
	if (vl.item(i) == v) {
	    return true;
	}
    }
    return false;
}
