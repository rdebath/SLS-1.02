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
 * A button is a view of some value that is normally set when
 * the button is pressed.
 */

#ifndef ivlook2_6_button_h
#define ivlook2_6_button_h

#include <IV-2_6/InterViews/interactor.h>
#include <IV-2_6/InterViews/subject.h>

#include <IV-2_6/_enter.h>

class Button;
class ButtonList;
class Bitmap;

class ButtonState : public Subject {
public:
    ButtonState();
    ButtonState(int);
    ButtonState(void*);
    virtual ~ButtonState();

    void operator=(ButtonState&);
    void SetValue(int);
    void SetValue(void*);
    void GetValue (int& v) { v = (int)value; }
    void GetValue (void*& v) { v = value; }
protected:
    void* value;

    void Modify(void*);
};

class Button : public Interactor {
public:
    void Attach(Button*);
    void Detach(Button*);

    void Enable();
    void Disable();

    void Choose();
    void UnChoose();

    virtual void Press();
    virtual void Refresh();

    virtual void Handle(Event&);

    virtual void Update();
protected:
    Button(ButtonState*, void*);
    Button(const char*, ButtonState*, void*);
    ~Button();

    void* value;		/* value associated with this button */
    ButtonState* subject;	/* set to this->value when pressed */
    ButtonList* associates;	/* enable/disable when chosen/unchosen */
    boolean enabled;		/* can be pressed */
    boolean chosen;		/* currently toggled on */
    boolean hit;		/* currently being pushed */
private:
    void Init(ButtonState*, void*);
};

class TextButton : public Button {
protected:
    char* text;
    Painter* background;
    Painter* grayout;		/* for disabled buttons */

    TextButton(const char*, const char*, ButtonState*, void*);
    TextButton(const char*, ButtonState*, void*);
    virtual ~TextButton();

    virtual void Reconfig();

    void MakeBackground();
    void MakeShape();
private:
    void Init(const char*);
};

class PushButton : public TextButton {
public:
    PushButton(const char*, ButtonState*, int);
    PushButton(const char*, ButtonState*, void*);
    PushButton(const char*, const char*, ButtonState*, int);
    PushButton(const char*, const char*, ButtonState*, void*);
    virtual ~PushButton();

    virtual void Refresh();
protected:
    virtual void Reconfig();
    virtual void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
private:
    void Init();
};

class RadioButton : public TextButton {
public:
    RadioButton(const char*, ButtonState*, int);
    RadioButton(const char*, ButtonState*, void*);
    RadioButton(const char*, const char*, ButtonState*, int);
    RadioButton(const char*, const char*, ButtonState*, void*);
    virtual ~RadioButton();

    virtual void Refresh();
protected:
    virtual void Reconfig();
    virtual void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
private:
    void Init();
};

class CheckBox : public TextButton {
public:
    CheckBox(const char*, ButtonState*, int, int);
    CheckBox(const char*, ButtonState*, void*, void*);
    CheckBox(const char*, const char*, ButtonState*, int, int);
    CheckBox(const char*, const char*, ButtonState*, void*, void*);
    virtual ~CheckBox();

    virtual void Press();
    virtual void Refresh();
    virtual void Update();
protected:
    virtual void Reconfig();
    virtual void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
private:
    void* offvalue;

    void Init(void*);
};

#include <IV-2_6/_leave.h>

#endif
