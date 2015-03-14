/*
 * Copyright (c) 1990, 1991 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * Unidraw - performs top-level event handling, catalog initialization, and
 * manages multiple editors.
 */

#ifndef unidraw_h
#define unidraw_h

#include <Unidraw/globals.h>

class Catalog;
class Command;
class Component;
class Editor;
class Event;
class Iterator;
class HistoryMap;
class OptionDesc;
class PropertyData;
class UList;
class World;

class Unidraw {
public:
    Unidraw(
        Catalog*, int& argc, char** argv, 
        OptionDesc* = nil, PropertyData* = nil
    );
    Unidraw(Catalog*, World*);
    virtual ~Unidraw();

    virtual void Run();
    virtual void Update(boolean immediate = false);
    virtual void Quit();

    virtual void Open(Editor*);
    virtual void Close(Editor*);                // delete editor
    virtual void CloseDependents(Component*);
    virtual void CloseAll();
    virtual boolean Opened(Editor*);

    void First(Iterator&);
    void Next(Iterator&);
    boolean Done(Iterator);
    Editor* GetEditor(Iterator);

    Editor* Find(Component*);
    Editor* FindAny(Component*);

    Catalog* GetCatalog();
    World* GetWorld();

    void SetHistoryLength(int);
    int GetHistoryLength();

    void Log(Command*);
    void Undo(Component*, int = 1);
    void Redo(Component*, int = 1);
    void ClearHistory(Component* = nil);
    void ClearHistory(Editor*);
protected:
    virtual void Process();

    boolean IsClean(Editor*);
    void Mark(Editor*);
    void Sweep();
    void DoUpdate();

    void GetHistory(Component*, UList*& past, UList*& future);
    void ClearHistory(UList*, int = 1);

    UList* elem(Iterator);
    Command* command(UList*);
    boolean alive();
    boolean updated();

    void alive(boolean);
    void updated(boolean);
private:
    void Init(Catalog*, World*);
    void InitAttributes();
    void DeleteComponent(Component*);

    Editor* editor(UList*);
    Editor* FindAny(Component*, UList*);
    Editor* FindAnyDead(Component*);
private:
    Catalog* _catalog;
    World* _world;
    UList* _editors;
    UList* _deadEditors;
    boolean _alive;
    boolean _updated;

    HistoryMap* _histories;
    int _histlen;
};

inline Catalog* Unidraw::GetCatalog () { return _catalog; }
inline World* Unidraw::GetWorld () { return _world; }
inline boolean Unidraw::alive () { return _alive; }
inline boolean Unidraw::updated () { return _updated; }
inline void Unidraw::alive (boolean a) { _alive = a; }
inline void Unidraw::updated (boolean u) { _updated = u; }

#endif
