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
 * Telltale - glyph with a state that may affect appearance
 */

#ifndef ivlook_telltale_h
#define ivlook_telltale_h

#include <InterViews/monoglyph.h>
#include <InterViews/observe.h>

class TelltaleGroup;

typedef unsigned int TelltaleFlags;

class TelltaleState : public Resource, public Observable {
public:
    TelltaleState(const TelltaleFlags = 0);
    virtual ~TelltaleState();

    enum {
	is_enabled = 0x1,
	is_visible = 0x2,
	is_enabled_visible = 0x3,
	is_active = 0x4,
	is_enabled_active = 0x5,
	is_visible_active = 0x6,
	is_enabled_visible_active = 0x7,
	is_chosen = 0x8,
	is_enabled_chosen = 0x9,
	is_visible_chosen = 0xa,
	is_enabled_visible_chosen = 0xb,
	is_active_chosen = 0xc,
	is_enabled_active_chosen = 0xd,
	is_visible_active_chosen = 0xe,
	is_enabled_visible_active_chosen = 0xf,
	is_running = 0x10,
	is_choosable = 0x20,
	is_toggle = 0x40,
	max_flags = 0x80
    };

    TelltaleFlags flags() const;

    virtual void set(const TelltaleFlags, boolean);
    virtual boolean test(const TelltaleFlags) const;
    virtual void join(TelltaleGroup*);
    virtual void leave_group();
private:
    TelltaleFlags flags_;
    TelltaleGroup* group_;
};

inline TelltaleFlags TelltaleState::flags() const { return flags_; }

class Telltale : public MonoGlyph, public Observer {
protected:
    Telltale(Glyph*, TelltaleState* = nil);
public:
    virtual ~Telltale();

    virtual void state(TelltaleState*);
    virtual TelltaleState* state() const;

    virtual void disconnect(Observable*);
private:
    TelltaleState* state_;

    /* backward compatibility */
public:
    void highlight(boolean);
    boolean highlighted() const;

    void choose(boolean);
    boolean chosen() const;

    void enable(boolean);
    boolean enabled() const;
};

#define Telltale_access(writer,reader,flag) \
inline void Telltale::writer(boolean b) { state()->set(flag, b); } \
inline boolean Telltale::reader() const { return state()->test(flag); }

Telltale_access(highlight,highlighted,TelltaleState::is_active)
Telltale_access(choose,chosen,TelltaleState::is_chosen)
Telltale_access(enable,enabled,TelltaleState::is_enabled)

class TelltaleGroup : public Resource {
public:
    TelltaleGroup();
    virtual ~TelltaleGroup();

    virtual void update(TelltaleState*);
    virtual void remove(TelltaleState*);
private:
    TelltaleState* current_;
};

#endif
