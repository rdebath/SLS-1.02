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
 * Sensors describe input events of interest.
 */

#ifndef iv2_6_sensor_h
#define iv2_6_sensor_h

#include <InterViews/event.h>
#include <InterViews/resource.h>

#include <InterViews/_enter.h>

class Interactor;
class InteractorWindow;

class Sensor : virtual public Resource {
public:
    Sensor();
    Sensor(const Sensor&);
    Sensor(const Sensor*);
    virtual ~Sensor();

    virtual Sensor& operator =(const Sensor&);

    void Catch(EventType);
    void CatchButton(EventType, int);
    void Ignore(EventType);
    void IgnoreButton(EventType, int);

    boolean Caught(const Event&) const;

    static void init();
protected:
    unsigned long mask;
    unsigned long down[8];
    unsigned long up[8];

    int ButtonIndex(unsigned long b) const { return (b >> 5) & 07; }
    int ButtonFlag(unsigned long b)  const{ return 1 << (b & 0x1f); }
    void SetButton(unsigned long a[], unsigned long b) {
	a[ButtonIndex(b)] |= ButtonFlag(b);
    }
    void ClearButton(unsigned long a[], unsigned long b) {
	a[ButtonIndex(b)] &= ~ButtonFlag(b);
    }
    boolean ButtonIsSet(const unsigned long a[], unsigned long b) const {
	return (a[ButtonIndex(b)] & ButtonFlag(b)) != 0;
    }
    void SetMouseButtons(unsigned long a[]) { a[0] |= 0x7; }
    void ClearMouseButtons(unsigned long a[]) { a[0] &= ~0x7; }
    boolean MouseButtons(const unsigned long a[]) const {
	return (a[0] & 0x7) != 0;
    }
private:
    friend class Interactor;
    friend class InteractorWindow;
};

extern Sensor* allEvents;
extern Sensor* onoffEvents;
extern Sensor* updownEvents;
extern Sensor* noEvents;

#include <InterViews/_leave.h>

#endif
