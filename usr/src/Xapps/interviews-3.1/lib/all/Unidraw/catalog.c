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
 * Catalog implementation.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/ctrlinfo.h>
#include <Unidraw/creator.h>
#include <Unidraw/editorinfo.h>
#include <Unidraw/globals.h>
#include <Unidraw/statevars.h>
#include <Unidraw/transfn.h>
#include <Unidraw/uarray.h>
#include <Unidraw/uformat.h>
#include <Unidraw/ulist.h>

#include <Unidraw/Commands/command.h>

#include <Unidraw/Components/csolver.h>
#include <Unidraw/Components/connector.h>

#include <Unidraw/Tools/tool.h>

#include <Unidraw/Graphic/graphic.h>
#include <Unidraw/Graphic/pspaint.h>

#include <InterViews/bitmap.h>
#include <InterViews/raster.h>
#include <InterViews/transformer.h>
#include <IV-2_6/InterViews/world.h>

#include <OS/memory.h>

#include <IV-2_6/_enter.h>

#include <ctype.h>
#include <osfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stream.h>
#include <strstream.h>
#ifdef SYSV
#include <OS/types.h>
#include <unistd.h>
#endif
#include <sys/file.h>

#ifdef __DECCXX
extern "C" {
    extern int access(char*, int);
    extern int unlink(char*);
}
#endif

/*****************************************************************************/

static const int nograylevel = -1;
static const int hex_encode = 6;
static const int hex_gray_encode = 2;
static char buf[CHARBUFSIZE];

static const unsigned int color_base = 255;     // 2^color_depth - 1

static char hexcharmap[] = {
     '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
     'a', 'b', 'c', 'd', 'e', 'f'
};

static unsigned int hexintmap[] = {
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  0,  0,  0,  0,  0,
     0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
};

static const char* HexEncode (
    ColorIntensity ir, ColorIntensity ig, ColorIntensity ib
) {
    unsigned int r = round(ir * color_base);
    unsigned int g = round(ig * color_base);
    unsigned int b = round(ib * color_base);

    static char enc[hex_encode+1];
    enc[hex_encode] = '\0';

    enc[0] = hexcharmap[r >> 4 & 0xf];
    enc[1] = hexcharmap[r & 0xf];
    enc[2] = hexcharmap[g >> 4 & 0xf];
    enc[3] = hexcharmap[g & 0xf];
    enc[4] = hexcharmap[b >> 4 & 0xf];
    enc[5] = hexcharmap[b & 0xf];

    return enc;
}

static void HexDecode (
    const char* enc, 
    ColorIntensity& ir, ColorIntensity& ig, ColorIntensity& ib
) {
    unsigned int r, g, b;

    r  = hexintmap[enc[0]] << 4;
    r |= hexintmap[enc[1]];
    g  = hexintmap[enc[2]] << 4;
    g |= hexintmap[enc[3]];
    b  = hexintmap[enc[4]] << 4;
    b |= hexintmap[enc[5]];

    ir = float(r) / float(color_base);
    ig = float(g) / float(color_base);
    ib = float(b) / float(color_base);
}

static const char* HexGrayEncode (
    ColorIntensity ir, ColorIntensity ig, ColorIntensity ib
) {
    ColorIntensity igray = 0.30 * ir + 0.59 * ig + 0.11 * ib;
    unsigned int gray = round(igray * color_base);

    static char enc[hex_gray_encode+1];
    enc[hex_gray_encode] = '\0';

    enc[0] = hexcharmap[gray >> 4 & 0xf];
    enc[1] = hexcharmap[gray & 0xf];

    return enc;
}

static void HexGrayDecode (const char* enc, ColorIntensity& ig) {
    unsigned int g;

    g  = hexintmap[enc[0]] << 4;
    g |= hexintmap[enc[1]];

    ig = float(g) / float(color_base);
}

/*****************************************************************************/

class NameMapElem : public UMapElem {
public:
    NameMapElem(void*, const char*);
    virtual ~NameMapElem();

    virtual void* id();
    virtual void* tag();
private:
    void Init(void*, const char*);
private:
    void* _object;
    char* _string;
};

void NameMapElem::Init (void* object, const char* string) {
    _object = object;
    _string = strnew(string);
}

NameMapElem::NameMapElem (void* object, const char* string) {
    Init(object, string);
}

NameMapElem::~NameMapElem () { delete _string; }
void* NameMapElem::id () { return _object; }
void* NameMapElem::tag () { return (void*) _string; }

/*****************************************************************************/

class NameMap : public UMap {
public:
    NameMap();

    void Register(void* obj, const char* name);
    void Unregister(void* obj);
    void Unregister(const char* name);

    void* GetObject(const char*);
    const char* GetName(void* obj);
protected:
    virtual UMapElem* FindTag(void*);
};

NameMap::NameMap () { }

void NameMap::Register (void* object, const char* name) {
    UMap::Register(new NameMapElem(object, name));
}

void NameMap::Unregister (void* object) {
    UMapElem* elem = FindId(object);

    if (elem != nil) {
        UMap::Unregister(elem);
        delete elem;
    }
}

void NameMap::Unregister (const char* name) {
    UMapElem* elem = FindTag((void*) name);

    if (elem != nil) {
        UMap::Unregister(elem);
        delete elem;
    }
}

void* NameMap::GetObject (const char* name) { 
    UMapElem* elem = FindTag((void*) name);
    return (elem == nil) ? nil : elem->id();
}

const char* NameMap::GetName (void* object) {
    UMapElem* elem = FindId(object);
    return (elem == nil) ? nil : (char*) elem->tag();
}

UMapElem* NameMap::FindTag (void* tag) {
    const char* string = (char*) tag;

    for (int i = 0; i < _elems.Count(); ++i) {
        const char* elemString = (char*) Elem(i)->tag();

        if (strcmp(string, elemString) == 0) {
            return (UMapElem*) _elems[i];
        }
    }
    return nil;
}

/*****************************************************************************/

inline PSBrush* getbr (UList* u) { return (PSBrush*) (*u)(); }
inline PSColor* getcolor (UList* u) { return (PSColor*) (*u)(); }
inline PSFont* getfont (UList* u) { return (PSFont*) (*u)(); }
inline PSPattern* getpat (UList* u) { return (PSPattern*) (*u)(); }

Catalog::Catalog (const char* domainName, Creator* creator, float version) {
    _name = strnew(domainName);
    _creator = creator;
    _version = version;
    _brs = new UList;
    _colors = new UList;
    _fonts = new UList;
    _pats = new UList;

    _curMap = nil;
    _substMap = new ObjectMap(nil, COMPONENT);
    _clipboard = new Clipboard;
#ifdef __GNUG__
    _tmpfile = nil;
#endif

    _edInfoMap = new NameMap;
    _compMap = new NameMap;
    _cmdMap = new NameMap;
    _toolMap = new NameMap;
}

void Catalog::Init (World* w) {
    _world = w;
    const char* fg = GetAttribute("foreground");
    const char* bg = GetAttribute("background");
    const char* font = GetAttribute("font");

    pssingle = FindBrush(0xffff, 0);
    psnonebr = FindNoneBrush();
    psblack = FindColor((fg == nil) ? "Black" : fg);
    pswhite = FindColor((bg == nil) ? "White" : bg);
    psstdfont = FindFont((font == nil) ? "fixed" : font, "fixed", "12");
    pssolid = FindGrayLevel(0.0);
    psclear = FindGrayLevel(1.0);
    psnonepat = FindNonePattern();

    stdgraphic = new FullGraphic;
    stdgraphic->FillBg(true);
    stdgraphic->SetColors(psblack, pswhite);
    stdgraphic->SetPattern(pssolid);
    stdgraphic->SetBrush(pssingle);
    stdgraphic->SetFont(psstdfont);
}

static void deleteBrushes (UList* brs) {
    while (!brs->IsEmpty()) {
        UList* u = brs->First();
        brs->Remove(u);
        PSBrush* br = getbr(u);
        Unref(br);
    }
}

static void deleteColors (UList* colors) {
    while (!colors->IsEmpty()) {
        UList* u = colors->First();
        colors->Remove(u);
        PSColor* color = getcolor(u);
        Unref(color);
    }
}

static void deleteFonts (UList* fonts) {
    while (!fonts->IsEmpty()) {
        UList* u = fonts->First();
        fonts->Remove(u);
        PSFont* font = getfont(u);
        Unref(font);
    }
}

static void deletePatterns (UList* pats) {
    while (!pats->IsEmpty()) {
        UList* u = pats->First();
        pats->Remove(u);
        PSPattern* pat = getpat(u);
        Unref(pat);
    }
}

Catalog::~Catalog () {
    deleteBrushes(_brs);
    deleteColors(_colors);
    deleteFonts(_fonts);
    deletePatterns(_pats);

    delete _name;
    delete _brs;
    delete _colors;
    delete _fonts;
    delete _pats;

    delete _substMap;
    delete _edInfoMap;
    delete _compMap;
    delete _cmdMap;
    delete _toolMap;

    _clipboard->DeleteComps();
    delete _clipboard;

#ifdef __GNUG__
    if (_tmpfile != nil) {
        unlink(_tmpfile);
        delete _tmpfile;
    }
#endif
}

boolean Catalog::Save (EditorInfo* edInfo, const char* name) {
    filebuf fbuf;
    boolean ok = fbuf.open((char*) name, output) != 0;

    if (ok) {
        ostream out(&fbuf);
        WriteEditorInfo(edInfo, out);
        ok = out.good();

        if (ok) {
            _edInfoMap->Unregister(name);
            _edInfoMap->Register(edInfo, name);
        }
    }
    return ok;
}

boolean Catalog::Save (Component* comp, const char* name) {
    ObjectMap* prevMap = _curMap;
    ObjectMap objmap(comp, COMPONENT);
    _curMap = &objmap;
    boolean ok = FileSave((void*) comp, COMPONENT, name);

    if (ok) {
        _compMap->Unregister(name);
        _compMap->Register(comp, name);
    }
    _curMap = prevMap;
    return ok;
}

boolean Catalog::Save (Command* cmd, const char* name) {
    ObjectMap* prevMap = _curMap;
    ObjectMap objmap(cmd, COMMAND);
    _curMap = &objmap;
    boolean ok = FileSave((void*) cmd, COMMAND, name);

    if (ok) {
        _cmdMap->Unregister(name);
        _cmdMap->Register(cmd, name);
    }
    _curMap = prevMap;
    return ok;
}

boolean Catalog::Save (Tool* tool, const char* name) {
    ObjectMap* prevMap = _curMap;
    ObjectMap objmap(tool, TOOL);
    _curMap = &objmap;
    boolean ok = FileSave((void*) tool, TOOL, name);
    _curMap = prevMap;

    if (ok) {
        _toolMap->Unregister(name);
        _toolMap->Register(tool, name);
    }
    return ok;
}

boolean Catalog::Retrieve (const char* name, EditorInfo*& edInfo) {
    boolean ok = true;
    edInfo = (EditorInfo*) _edInfoMap->GetObject(name);

    if (edInfo == nil) {
        filebuf fbuf;
        ok = fbuf.open((char*) name, input) != 0;

        if (ok) {
            istream in(&fbuf);
            edInfo = ReadEditorInfo(in);
            ok = !in.bad();

            if (ok) {
                _edInfoMap->Unregister(name);
                _edInfoMap->Register(edInfo, name);
            }            
        }
    }
    return ok;
}

boolean Catalog::Retrieve (const char* name, Component*& comp) {
    boolean ok = true;
    comp = (Component*) _compMap->GetObject(name);

    if (comp == nil) {
        ObjectMap* prevMap = _curMap;
        ObjectMap objmap(comp, COMPONENT);
        _curMap = &objmap;
        ok = FileRetrieve(name, (void*&) comp);

        if (ok) {
            _compMap->Unregister(name);
            _compMap->Register(comp, name);
        }
        _curMap = prevMap;
    }
    return ok;
}

boolean Catalog::Retrieve (const char* name, Command*& cmd) {
    boolean ok = true;
    cmd = (Command*) _cmdMap->GetObject(name);

    if (cmd == nil) {
        ObjectMap* prevMap = _curMap;
        ObjectMap objmap(cmd, COMMAND);
        _curMap = &objmap;
        ok =  FileRetrieve(name, (void*&) cmd);

        if (ok) {
            _cmdMap->Unregister(name);
            _cmdMap->Register(cmd, name);
        }
        _curMap = prevMap;
    }
    return ok;
}

boolean Catalog::Retrieve (const char* name, Tool*& tool) {
    boolean ok = true;
    tool = (Tool*) _toolMap->GetObject(name);

    if (tool == nil) {
        ObjectMap* prevMap = _curMap;
        ObjectMap objmap(tool, TOOL);
        _curMap = &objmap;
        ok = FileRetrieve(name, (void*&) tool);

        if (ok) {
            _toolMap->Unregister(name);
            _toolMap->Register(tool, name);
        }
        _curMap = prevMap;
    }
    return ok;
}

boolean Catalog::Valid (const char* name, EditorInfo*& obj) {
    obj = (EditorInfo*) _edInfoMap->GetObject(name);
    return obj != nil;
}

boolean Catalog::Valid (const char* name, Component*& obj) {
    obj = (Component*) _compMap->GetObject(name);
    return obj != nil;
}

boolean Catalog::Valid (const char* name, Command*& obj) {
    obj = (Command*) _cmdMap->GetObject(name);
    return obj != nil;
}

boolean Catalog::Valid (const char* name, Tool*& obj) {
    obj = (Tool*) _toolMap->GetObject(name);
    return obj != nil;
}

void Catalog::Forget (void* object, const char* name, NameMap* map) {
    if (name == nil) {
        map->Unregister(object);
    } else {
        map->Unregister(name);
    }
    _substMap->Unregister(object);
}

void Catalog::Forget (EditorInfo* edInfo, const char* name) {
    Forget(edInfo, name, _edInfoMap);
}

void Catalog::Forget (Component* comp, const char* name) {
    Forget(comp, name, _compMap);
}

void Catalog::Forget (Command* cmd, const char* name) {
    Forget(cmd, name, _cmdMap);
}

void Catalog::Forget (Tool* tool, const char* name) {
    Forget(tool, name, _toolMap);
}

Component* Catalog::Copy (Component* comp) {
    return (Component*) CopyObject(comp, COMPONENT);
}

Command* Catalog::Copy (Command* cmd) {
    return (Command*) CopyObject(cmd, COMMAND);
}

Tool* Catalog::Copy (Tool* tool) { return (Tool*) CopyObject(tool, TOOL); }

void Catalog::SetClipboard (Clipboard* o) { _clipboard = o; }
void Catalog::SetEditorInfo (EditorInfo* o) { _edInfo = o; }

const char* Catalog::GetName (EditorInfo* edInfo) {
    return _edInfoMap->GetName(edInfo);
}
const char* Catalog::GetName (Component* comp) {
    return _compMap->GetName(comp);
}

const char* Catalog::GetName (Command* cmd) { return _cmdMap->GetName(cmd); }
const char* Catalog::GetName (Tool* tool) { return _toolMap->GetName(tool); }

void* Catalog::ReadObject (istream& in) {
    void* obj = nil;
    int inst_id;
    ClassId subst_id;
    const char* delim_ptr;
    char delim[CHARBUFSIZE];

    ClassId classId = ReadClassId(in, inst_id, subst_id, delim_ptr);

    if (subst_id != UNDEFINED_CLASS) {
        strcpy(delim, delim_ptr);
    }

    if (classId != UNDEFINED_CLASS) {
        if (_curMap != nil && inst_id) {
            obj = _curMap->GetObject(inst_id);

            if (obj == nil) {
                obj = _creator->Create(classId, in, _curMap, inst_id);
            }
        } else {
            obj = _creator->Create(classId, in);
        }
    }

    if (FileVersion() >= UV_ORIGINAL && subst_id != UNDEFINED_CLASS) {
        if (obj == nil) {
            obj = ReadSubstObject(in, inst_id, classId, subst_id, delim);
        } else {
            in >> buf;
        }
    }

    return obj;
}

void* Catalog::ReadSubstObject (
    istream& in, int inst_id, ClassId orig_id,
    ClassId subst_id, const char* delim
) {
    void* obj = _creator->Create(subst_id, in, _curMap, inst_id);

    if (inst_id == _curMap->GetId(obj)) {
        UArray* extra_data = new UArray(64);
        ReadExtraData(in, delim, extra_data);

        _substMap->Register(obj, inst_id, orig_id, delim, extra_data);
    }
    return obj;
}

static boolean FoundDelim (const char* delim, UArray& data) {
    int n_delim = strlen(delim);
    int n_data = data.Count();
    boolean found = n_data >= n_delim;

    if (found) {
        int j = n_data - n_delim;

        for (int i = 0; i < n_delim; ++i, ++j) {
            char c = (int) data[j];

            if (delim[i] != c) {
                found = false;
                break;
            }
        }
    }
    return found;
}

void Catalog::ReadExtraData (
    istream& in, const char* delim, UArray* extra_data
) {
    for (int i = 0; !in.eof() && !FoundDelim(delim, *extra_data); ++i) {
        char c;
        in.get(c);
        extra_data->Insert((void*) c, i);
    }
}

void* Catalog::CopyObject (void* obj, ClassId base_id) {
    void* copy = nil;

    ObjectMap* prev_subst_map = _substMap;      // no substitution magic for
    ObjectMap empty_subst_map(obj, base_id);    // copies
    _substMap = &empty_subst_map;

#ifdef __GNUG__
    filebuf obuf, ibuf;
    ObjectMap* prevMap = _curMap;
    char* prevTmp = _tmpfile;
    static int stackLvl;

    if (_tmpfile == nil || ++stackLvl > 1) {
        _tmpfile = tempnam("/tmp", ".udcp");
    }
    boolean ok = obuf.open(_tmpfile, output) != 0;

    if (ok) {
	ObjectMap omap(obj, base_id);
        ostream out(&obuf);

	_curMap = &omap;
	ok = SaveObject(obj, base_id, out);
	out.flush();
	obuf.close();
    }
    if (ok) {
	ObjectMap imap(copy, base_id);
        istream in(&ibuf);

        _curMap = &imap;
	ok = ibuf.open(_tmpfile, input) != 0 && RetrieveObject(in, copy);
    }
    if (!ok) {
        cerr << "Unidraw error: couldn't copy object (/tmp unwritable?)\n";
    }
    if (--stackLvl > 0 || !ok) {
        prevTmp = (_tmpfile == prevTmp) ? nil : prevTmp;
        delete _tmpfile;
        _tmpfile = prevTmp;
    }
    _curMap = prevMap;
#else
    ObjectMap* prevMap = _curMap;
    ObjectMap omap(obj, base_id), imap(copy, base_id);

    strstream inout;

    _curMap = &omap;
    boolean ok = SaveObject(obj, base_id, inout);

    if (ok) {
        _curMap = &imap;
        ok = RetrieveObject(inout, copy);
    }
    if (!ok) {
        cerr << "Unidraw error: couldn't copy object\n";
    }
    _curMap = prevMap;
#endif

    _substMap = prev_subst_map;
    return copy;
}

Component* Catalog::ReadComponent (istream& in) {
    return (Component*) ReadObject(in);
}

Command* Catalog::ReadCommand (istream& in) {
    return (Command*) ReadObject(in);
}

Tool* Catalog::ReadTool (istream& in) {
    return (Tool*) ReadObject(in);
}

StateVar* Catalog::ReadStateVar (istream& in) {
    return (StateVar*) ReadObject(in);
}

TransferFunct* Catalog::ReadTransferFunct (istream& in) {
    return (TransferFunct*) ReadObject(in);
}

void Catalog::WriteComponent (Component* comp, ostream& out){
    WriteObject(comp, COMPONENT, out);
}

void Catalog::WriteCommand (Command* cmd, ostream& out) {
    WriteObject(cmd, COMMAND, out);
}

void Catalog::WriteTool (Tool* tool, ostream& out) {
    WriteObject(tool, TOOL, out);
}    

void Catalog::WriteStateVar (StateVar* stateVar, ostream& out) {
    WriteObject(stateVar, STATE_VAR, out);
}

void Catalog::WriteTransferFunct (TransferFunct* transfn, ostream& out) {
    WriteObject(transfn, TRANSFER_FUNCT, out);
}    

static ClassId Narrow (void* obj, ClassId base_id) {
    switch (base_id) {
        case COMPONENT:      return ((Component*) obj)->GetClassId();
        case COMMAND:        return ((Command*) obj)->GetClassId();
        case TOOL:           return ((Tool*) obj)->GetClassId();
        case STATE_VAR:      return ((StateVar*) obj)->GetClassId();
        case TRANSFER_FUNCT: return ((TransferFunct*) obj)->GetClassId();

	default:	     return UNDEFINED_CLASS;
     }
}

static ClassId NarrowSubst (void* obj, ClassId base_id, const char*& delim) {
    switch (base_id) {
        case COMPONENT:      return ((Component*) obj)->GetSubstId(delim);
        case COMMAND:        return ((Command*) obj)->GetSubstId(delim);
        case TOOL:           return ((Tool*) obj)->GetSubstId(delim);
        case STATE_VAR:      return ((StateVar*) obj)->GetSubstId(delim);
        case TRANSFER_FUNCT: return ((TransferFunct*) obj)->GetSubstId(delim);

	default:	     return UNDEFINED_CLASS;
     }
}

void Catalog::WriteIt (void* obj, ClassId base_id, ostream& out) {
    switch (base_id) {
        case COMPONENT:      ((Component*) obj)->Write(out); break;
        case COMMAND:        ((Command*) obj)->Write(out); break;
        case TOOL:           ((Tool*) obj)->Write(out); break;
        case STATE_VAR:      ((StateVar*) obj)->Write(out); break;
        case TRANSFER_FUNCT: ((TransferFunct*) obj)->Write(out); break;
     }

    UArray* extra_data = _substMap->GetExtraData(obj);

    if (extra_data == nil) {
        const char* delim;

        if (NarrowSubst(obj, base_id, delim) != UNDEFINED_CLASS) {
            out << delim;
        }

    } else {
        for (int i = 0; i < extra_data->Count(); ++i) {
            char c = (int) (*extra_data)[i];
            out << c;
        }
    }
}

void Catalog::WriteObject (void* obj, ClassId base_id, ostream& out) {
    if (obj == nil) {
        WriteClassId(UNDEFINED_CLASS, out);

    } else if (_curMap == nil) {
        WriteClassId(obj, base_id, out);
        WriteIt(obj, base_id, out);

    } else {
        int id = _curMap->GetId(obj);

        if (id) {
            WriteClassId(obj, base_id, out, id);

        } else {
            id = (int) obj;
            _curMap->Register(obj, id);
            WriteClassId(obj, base_id, out, id);
            WriteIt(obj, base_id, out);
        }
    }
}    

boolean Catalog::SaveObject (void* obj, ClassId base_id, ostream& out) {
    WriteVersion(_version, out);
    WriteObject(obj, base_id, out);
    csolver->Write(out);
    return out.good();
}    

boolean Catalog::RetrieveObject (istream& in, void*& obj) {
    _fileVersion = ReadVersion(in);
    obj = ReadObject(in);
    csolver->Read(in);
    return in.good();
}

boolean Catalog::FileSave (void* obj, ClassId base_id, const char* name) {
    filebuf fbuf;
    boolean ok = fbuf.open((char*) name, output) != 0;

    if (ok) {
        ostream out(&fbuf);
        ok = SaveObject(obj, base_id, out);
    }
    return ok;
}    

boolean Catalog::FileRetrieve (const char* name, void*& obj) {
    filebuf fbuf;
    boolean ok = fbuf.open((char*) name, input) != 0;

    if (ok) {
        istream in(&fbuf);
        ok = RetrieveObject(in, obj);
    }
    return ok;
}

boolean Catalog::Exists (const char* path) {
    /* cast workaround for DEC C++ prototype bug */
    return access((char*)path, F_OK) >= 0;
}

boolean Catalog::Writable (const char* path) {
    /* cast workaround for DEC C++ prototype bug */
    return access((char*)path, W_OK) >= 0;
}

const char* Catalog::GetAttribute (const char* a) {
    return _world->GetAttribute(a);
}

const char* Catalog::Name (const char* name, int index) {
    sprintf(buf, "%s%d", name, index);
    return buf;
}

int Catalog::GetToken (istream& in, char* buf, int size) {
    int count = 0;

    for (int i = 0; i < size && !in.eof(); ++i) {
        in.get(buf[i]);
        ++count;

        if (isspace(buf[i])) {
            break;
        }
    }
    return count;
}

void Catalog::Skip (istream& in) {
    int len = strlen(MARK);

    while (GetToken(in, buf, CHARBUFSIZE) != 0) {
        if (strncmp(buf, MARK, len) == 0) {
            break;
        }
    }
}

void Catalog::Mark (ostream& out) {
    out << "\n" << MARK << " ";
}

float Catalog::ReadVersion (istream& in) {
    float version;

    Skip(in);
    in >> buf >> version;
    return version;
}

void Catalog::WriteVersion (float version, ostream& out) {
    out << MARK << " Unidraw " << version << " ";
}

ClassId Catalog::ReadClassId (
    istream& in, int& id, ClassId& subst_id, const char*& delim
) {
    long classId;

    Skip(in);

    if (FileVersion() < UV_ORIGINAL) {
        in >> classId >> id;
        subst_id = UNDEFINED_CLASS;

    } else {
        long sid;
        in >> classId >> id >> sid;
        subst_id = (ClassId) sid;

        if (subst_id != UNDEFINED_CLASS) {
            in >> buf;
            delim = buf;
        }
    }

    return (ClassId) classId;
}

void Catalog::WriteClassId (
    ClassId classId, ostream& out, int inst_id, ClassId subst_id,
    const char* delim
) {
    Mark(out);
    out << classId << " " << inst_id << " " << subst_id << " ";

    if (subst_id != UNDEFINED_CLASS) {
        out << delim << " ";
    }
}

void Catalog::WriteClassId (
    void* obj, ClassId base_id, ostream& out, int inst_id
) {
    ClassId orig_id = _substMap->GetOrigClassId(obj);
    const char* delim;
    ClassId subst_id;

    if (orig_id == UNDEFINED_CLASS) {           // wasn't an unknown class
        orig_id = Narrow(obj, base_id);
        subst_id = NarrowSubst(obj, base_id, delim);

    } else {
        subst_id = Narrow(obj, base_id);
        delim = _substMap->GetDelim(obj);
    }

    WriteClassId(orig_id, out, inst_id, subst_id, delim);
}

int Catalog::ReadBgFilled (istream& in) {
    int bgFilled;

    Skip(in);
    in >> bgFilled;
    return bgFilled;
}

void Catalog::WriteBgFilled (boolean bgFilled, ostream& out) {
    Mark(out);
    out << bgFilled << " ";
}

Transformer* Catalog::ReadTransformer (istream& in) {
    Transformer* t = nil;
    char lookahead;

    Skip(in);
    in >> buf;

    if (buf[0] == 't') {
        in >> lookahead;

        if (lookahead != '~') {
            in.putback(lookahead);
            float a00, a01, a10, a11, a20, a21;
            in >> a00 >> a01 >> a10 >> a11 >> a20 >> a21;
            t = new Transformer(a00, a01, a10, a11, a20, a21);
        }
    }
    return t;
}

void Catalog::WriteTransformer (Transformer* t, ostream& out) {
    Mark(out);
    out << "t ";

    if (t == nil) {
        out << "~ ";

    } else {
        float a00, a01, a10, a11, a20, a21;
        t->GetEntries(a00, a01, a10, a11, a20, a21);
        out << a00 << " " << a01 << " " << a10 << " ";
        out << a11 << " " << a20 << " " << a21 << " ";
    }
}

void Catalog::WriteBrush (PSBrush* brush, ostream& out) {
    Mark(out);
    out << "b ";

    if (brush == nil) {
        out << "~ ";

    } else if (brush->None()) {
        out << "n ";

    } else {
        int p = brush->GetLinePattern();
        int w = brush->Width();
        out << p << " " << w << " ";
    }
}

PSBrush* Catalog::ReadBrush (istream& in) {
    PSBrush* brush = nil;

    Skip(in);
    in >> buf;

    if (buf[0] == 'b') {
        char lookahead = '~';
        boolean defined = true;
        boolean none = false;
        int p = 0;
        int w = 0;

        in >> lookahead;

        switch (lookahead) {
        case '~':
            defined = false;
            break;
        case 'n':
            none = true;
            break;
        default:
            in.putback(lookahead);
            in >> p >> w;
            break;
        }

        if (defined && in.good()) {
            if (none) {
                brush = FindNoneBrush();
            } else {
                brush = FindBrush(p, w);
            }
        }
    }
    return brush;
}

PSBrush* Catalog::FindNoneBrush () {
    PSBrush* brush = nil;

    for (UList* u = _brs->First(); u != _brs->End(); u = u->Next()) {
        brush = getbr(u);

        if (brush->None()) {
            return brush;
        }
    }
    brush = new PSBrush;
    Ref(brush);
    _brs->Append(new UList(brush));

    return brush;
}

PSBrush* Catalog::FindBrush (int p, int w) {
    PSBrush* brush = nil;

    for (UList* u = _brs->First(); u != _brs->End(); u = u->Next()) {
        brush = getbr(u);

        if (
            !brush->None() && brush->GetLinePattern() == p &&
            brush->Width() == w
        ) {
            return brush;
        }
    }
    brush = new PSBrush(p, w);
    Ref(brush);
    _brs->Append(new UList(brush));

    return brush;
}

PSBrush* Catalog::ReadBrush (const char* n, int index) {
    const char* def = GetAttribute(Name(n, index));

    if (def == nil) {
	return nil;
    }
    char* definition = strnew(def); // some sscanfs write to their format...

    PSBrush* br = nil;
    int p, w;
    boolean none = (definition[0] == 'n' || definition[0] == 'N');

    if (none) {
	br = FindNoneBrush();

    } else if (sscanf(definition, "%x %d", &p, &w) == 2) {
        br = FindBrush(p, w);
    }
    delete definition;
    return br;
}

void Catalog::WriteColor (PSColor* color, ostream& out) {
    Mark(out);
    out << "c ";

    if (color == nil) {
        out << "~ ";

    } else {
        const char* name = color->GetName();
        out << name << " ";

        if (strcmp(name, "white") == 0 || strcmp(name, "White") == 0) {
            out << "1 1 1 ";

        } else {
            ColorIntensity r, g, b;
            color->GetIntensities(r, g, b);
            out << r << " " << g << " " << b << " ";
        }
    }
}

PSColor* Catalog::ReadColor (istream& in) {
    PSColor* color = nil;

    Skip(in);
    in >> buf;

    if (buf[0] == 'c') {
        char lookahead = '~';
        boolean defined = true;
        char name[CHARBUFSIZE];
        ColorIntensity r = 0, g = 0, b = 0;

        in >> lookahead;

        if (lookahead == '~') {
            defined = false;
        } else {
            in.putback(lookahead);
            in >> name >> r >> g >> b;
        }

        if (defined && in.good()) {
            int ir = round(r * float(0xffff));
            int ig = round(g * float(0xffff));
            int ib = round(b * float(0xffff));
            
            color = FindColor(name, ir, ig, ib);
        }
    }
    return color;
}

PSColor* Catalog::FindColor (const char* name, int ir, int ig, int ib) {
    PSColor* color = nil;

    for (UList* u = _colors->First(); u != _colors->End(); u = u->Next()) {
        color = getcolor(u);

        if (strcmp(color->GetName(), name) == 0) {
            return color;
        }
    }

    ColorIntensity r, g, b;
    if (!Color::find(World::current()->display(), name, r, g, b)) {
	r = float(ir) / float(0xffff);
	g = float(ig) / float(0xffff);
	b = float(ib) / float(0xffff);
    }
    color = new PSColor(r, g, b, name);
    Ref(color);
    _colors->Append(new UList(color));
    return color;
}

PSColor* Catalog::ReadColor (const char* n, int index) {
    const char* def = GetAttribute(Name(n, index));

    if (def == nil) {
	return nil;
    }
    char* definition = strnew(def); // some sscanfs write to their format...

    PSColor* color = nil;
    char name[CHARBUFSIZE];
    int r = 0, g = 0, b = 0;

    if (sscanf(definition, "%s %d %d %d", name, &r, &g, &b) == 4) {
	color = FindColor(name, r, g, b);

    } else if (sscanf(definition, "%s", name) == 1) {
	color = FindColor(name);
    }
    delete definition;
    return color;
}

void Catalog::WriteFont (PSFont* font, ostream& out) {
    Mark(out);
    out << "f ";

    if (font == nil) {
        out << "~ ";

    } else {
        const char* name = font->GetName();
        const char* pf = font->GetPrintFont();
        const char* ps = font->GetPrintSize();
        out << name << " /" << pf << " " << ps << " ";
    }
}

static void ReadName (istream& in, char* buf) {
    for (int i = 0; in.good(); ++i) {
        in >> buf[i];

        if (buf[i] == '/') {
            in.putback(buf[i]);
            buf[i] = '\0';
            break;
        }
    }
}

PSFont* Catalog::ReadFont (istream& in) {
    PSFont* font = nil;

    Skip(in);
    in >> buf;

    if (buf[0] == 'f') {
        char lookahead = '~';
        boolean defined = true;
        char name[CHARBUFSIZE];
        char printfont[CHARBUFSIZE];
        char printsize[CHARBUFSIZE];

        in >> lookahead;

        if (lookahead == '~') {
            defined = false;
        } else {
            in.putback(lookahead);
            ReadName(in, name);
            in >> printfont;
            in >> printsize;
        }

        if (defined && in.good()) {
            font = FindFont(name, &printfont[1], printsize);
        }
    }
    return font;
}

PSFont* Catalog::FindFont (const char* name, const char* pf, const char* ps) {
    PSFont* font = nil;

    for (UList* u = _fonts->First(); u != _fonts->End(); u = u->Next()) {
        font = getfont(u);

        if (
            strcmp(font->GetPrintFont(), pf) == 0
            && strcmp(font->GetPrintSize(), ps) == 0
        ) {
            return font;
        }
    }

    if (Font::exists(World::current()->display(), name)) {
	font = new PSFont(name, pf, ps);
    } else {
        fprintf(stderr, "invalid font name %s, ", name);
        fprintf(stderr, "substituting fixed font\n");
        font = new PSFont("fixed", pf, ps);
    }
    Ref(font);
    _fonts->Append(new UList(font));
    return font;
}

static void NoTrailingSpace (char* string) {
    char* spc = &string[strlen(string)];

    do {
        *spc = '\0';
    } while (spc > string && isspace(*--spc));
}

static char* LastWord (char* string) {
    NoTrailingSpace(string);
    char* spc = &string[strlen(string)];
    while (spc > string && !isspace(*--spc));
    return (spc == string) ? spc : spc+1;
}

PSFont* Catalog::ReadFont (const char* n, int index) {
    const char* def = GetAttribute(Name(n, index));

    if (def == nil) {
	return nil;
    }
    char* definition = strnew(def);

    PSFont* font = nil;
    char pf[CHARBUFSIZE];
    char ps[CHARBUFSIZE];

    char* last_word = LastWord(definition);
    
    if (last_word >= definition) {
        strcpy(ps, last_word);
        *last_word = '\0';

        last_word = LastWord(definition);

        if (last_word >= definition) {
            strcpy(pf, last_word);
            *last_word = '\0';

            NoTrailingSpace(definition);
            font = FindFont(definition, pf, ps);
        }
    }
    delete definition;
    return font;
}

static int CalcBitmap (float graylevel) {
    static const int SHADES = 17;
    static int shades[SHADES] = {
	0xffff, 0xefff, 0xefbf, 0xafbf,
	0xafaf, 0xadaf, 0xada7, 0xa5a7,
	0xa5a5, 0x85a5, 0x8525, 0x0525,
	0x0505, 0x0405, 0x0401, 0x0001,
	0x0000
    };
    return shades[round(graylevel * (SHADES - 1))];
}

static const int* ExpandToFullSize (const int* orig_data, int size) {
    static int data[patternHeight];
    Memory::copy(orig_data, data, patternHeight*sizeof(int));

    if (size == 1) {
	int seed = data[0];

	for (int i = 0; i < 4; i++) {
	    data[i] = (seed & 0xf000) >> 12;
	    data[i] |= data[i] << 4;
	    data[i] |= data[i] << 8;
	    data[i+4] = data[i];
	    data[i+8] = data[i];
	    data[i+12] = data[i];
	    seed <<= 4;
	}

    } else if (size == 8) {
	for (int i = 0; i < 8; i++) {
	    data[i] &= 0xff;
	    data[i] |= data[i] << 8;
	    data[i+8] = data[i];
	}

    } else if (size == patternHeight) {
	const unsigned int patternWidthMask = ~(~0 << patternWidth);

	for (int i = 0; i < patternHeight; i++) {
	    data[i] &= patternWidthMask;
	}

    } else {
	fprintf(stderr, "invalid size passed to ExpandToFullSize\n");
    }
    return data;
}

void Catalog::WritePattern (PSPattern* pattern, ostream& out) {
    Mark(out);
    out << "p ";

    if (pattern == nil) {
        out << "~ ";

    } else if (pattern->None()) {
        out << "n ";

    } else if (pattern->GetSize() > 0) {
        const int* data = pattern->GetData();
        int size = pattern->GetSize();

        out << "< ";

        if (size <= 8) {
            for (int i = 0; i < 8; i++) {
                sprintf(buf, "%02x", data[i] & 0xff);
                out << buf << " ";
            }

        } else {
            for (int i = 0; i < patternHeight; i++) {
                sprintf(buf, "%0*x", patternWidth/4, data[i]);
                out << buf << " ";
            }
        }
        out << "> " << nograylevel << " ";

    } else {
        float graylevel = pattern->GetGrayLevel();
        out << graylevel << " ";
    }
}

PSPattern* Catalog::ReadPattern (istream& in) {
    PSPattern* pattern = nil;

    Skip(in);
    in >> buf;

    if (buf[0] == 'p') {
        char lookahead = '~';
        boolean defined = true;
        boolean none = false;
        float graylevel = 0;
        int data[patternHeight];
        int size = 0;

        in >> lookahead;

        switch (lookahead) {
        case '~':
            defined = false;
            break;
        case 'n':
            none = true;
            break;
        case '<':
            graylevel = nograylevel;
            break;
        default:
            in.putback(lookahead);
            break;
        }

        if (defined && !none && graylevel != nograylevel) {
            in >> graylevel;

        } else if (graylevel == nograylevel) {
            for (int i = 0; in >> buf && i < patternHeight; i++) {
                if (buf[0] == '>' || sscanf(buf, "%x", &data[i]) != 1) {
                    break;
                }
            }

            if (buf[0] == '>') {
                size = i;
            } else {
                defined = false;
            }
        }

        if (defined && in.good()) {
            if (none) {
                pattern = FindNonePattern();
            } else if (graylevel != nograylevel) {
                pattern = FindGrayLevel(graylevel);
            } else {
                pattern = FindPattern(data, size);
            }
        }
    }
    return pattern;
}

PSPattern* Catalog::FindNonePattern () {
    PSPattern* pattern = nil;

    for (UList* u = _pats->First(); u != _pats->End(); u = u->Next()) {
        pattern = getpat(u);

        if (pattern->None()) {
            return pattern;
        }
    }
    pattern = new PSPattern;
    Ref(pattern);
    _pats->Append(new UList(pattern));

    return pattern;
}    

PSPattern* Catalog::FindGrayLevel (float graylevel) {
    PSPattern* pattern = nil;

    for (UList* u = _pats->First(); u != _pats->End(); u = u->Next()) {
        pattern = getpat(u);

        if (pattern->GetGrayLevel() == graylevel) {
            return pattern;
        }
    }
    int shade = CalcBitmap(graylevel);
    pattern = new PSPattern(shade, graylevel);
    Ref(pattern);
    _pats->Append(new UList(pattern));

    return pattern;
}

PSPattern* Catalog::FindPattern (int orig_data[patternHeight], int size) {
    PSPattern* pattern = nil;
    const int* data = ExpandToFullSize(orig_data, size);

    for (UList* u = _pats->First(); u != _pats->End(); u = u->Next()) {
        pattern = getpat(u);

        if (pattern->GetSize() != 0) {
            const int* cmpdata = pattern->GetData();

            if (!Memory::compare(data, cmpdata, patternHeight * sizeof(int))) {
                return pattern;
            }
        }
    }
    pattern = new PSPattern(data, size);
    Ref(pattern);
    _pats->Append(new UList(pattern));

    return pattern;
}

PSPattern* Catalog::ReadPattern (const char* n, int index) {
    const char* def = GetAttribute(Name(n, index));

    if (def == nil) {
	return nil;
    }
    char* definition = strnew(def); // some sscanfs write to their format...

    PSPattern* pat = nil;
    boolean none = (definition[0] == 'n' || definition[0] == 'N');

    if (none) {
	pat = FindNonePattern();

    } else {
	if (strchr(definition, '.') != nil) {
	    float graylevel;

	    if (sscanf(definition, "%f", &graylevel) == 1) {
                pat = FindGrayLevel(graylevel);
	    }

	} else {
	    istrstream in(definition, strlen(definition) + 1);
            int data[patternHeight];

	    for (int i = 0; in >> buf && i < patternHeight; i++) {
		if (sscanf(buf, "%x", &data[i]) != 1) {
		    break;
		}
	    }

	    if (i == 1 || i == 8 || i == patternHeight) {
                pat = FindPattern(data, i);
	    }
	}
    }
    delete definition;
    return pat;
}

inline void GetString (char* string, int count, istream& in) {
    for (char* p = string; p < &string[count]; in.get(*p++));
}

char* Catalog::ReadString (istream& in) {
    int count;
    char quotes;
    char* string = nil;

    Skip(in);
    in >> count;
    
    if (count >= 0) {
        in >> quotes;
        string = new char[count+1];
        GetString(string, count, in);
        in >> quotes;
        string[count] = '\0';
    }
    return string;
}

void Catalog::WriteString (const char* string, ostream& out) {
    Mark(out);

    if (string == nil) {
        out << -1;
    } else {
        out << strlen(string) << "\"" << string << "\"";
    }
}

Bitmap* Catalog::ReadBitmap (istream& in) {
    Skip(in);
    Coord w, h;
    in >> w >> h;

    Bitmap* bitmap = new Bitmap((void*) nil, w, h);
    ReadBitmapData(bitmap, in);

    return bitmap;
}

void Catalog::ReadBitmapData (Bitmap* bitmap, istream& in) {
    Coord w = bitmap->Width();
    Coord h = bitmap->Height();
    
    for (int j = h-1; j >= 0; --j) { 
        Skip(in);

        for (int k = 0; k < w; k += 4) {
            char hexchar;
            in >> hexchar;
            unsigned int val = hexintmap[hexchar];

            for (int i = k; i < w; ++i) {
                bitmap->poke(val & 0x8, i, j);
                val = val << 1;
            }
        }
    }
    bitmap->flush();
}

void Catalog::WriteBitmap (Bitmap* bitmap, ostream& out) {
    Mark(out);

    Coord w = bitmap->Width();
    Coord h = bitmap->Height();
    out << w << " " << h;

    WriteBitmapData(bitmap, out);
}

void Catalog::WriteBitmapData (Bitmap* bitmap, ostream& out) {
    Coord w = bitmap->Width();
    Coord h = bitmap->Height();
    int nybbles = 0;

    for (int j = h-1; j >= 0; --j) {
        Mark(out);

        for (int k = 0; k < w; k += 4) {
            unsigned int bits = 0;

            for (int i = k; i < k+4 && k < w; ++i) {
                unsigned int val = bitmap->peek(i, j);
                int pos = 3 - i % 4;
                bits |= val << pos;
            }
            out << hexcharmap[bits];
            ++nybbles;
        }
    }
    if (nybbles%2 != 0) {
        out << '0';
    }
}

Raster* Catalog::ReadGraymap (istream& in) {
    Skip(in);
    Coord w, h;
    in >> w >> h;

    Raster* raster = new Raster(w, h);
    ReadGraymapData(raster, in);

    return raster;
}

void Catalog::ReadGraymapData (Raster* raster, istream& in) {
    Coord w = raster->Width();
    Coord h = raster->Height();
    ColorIntensity g;

    char enc[hex_gray_encode+1];
    enc[hex_gray_encode] = '\0';

    for (int j = h-1; j >= 0; --j) {
        Skip(in);

        for (int i = 0; i < w; ++i) {
            in.get(enc, hex_gray_encode+1);
            HexGrayDecode(enc, g);
            raster->poke(i, j, g, g, g, 1);
        }
    }
    raster->flush();
}

void Catalog::WriteGraymap (Raster* raster, ostream& out) {
    Mark(out);

    Coord w = raster->Width();
    Coord h = raster->Height();
    out << w << " " << h;

    WriteGraymapData(raster, out);
}

void Catalog::WriteGraymapData (Raster* raster, ostream& out) {
    Coord w = raster->Width();
    Coord h = raster->Height();
    ColorIntensity r, g, b;
    float alpha;

    for (int j = h-1; j >= 0; --j) {
        Mark(out);

        for (int i = 0; i < w; ++i) {
            raster->peek(i, j, r, g, b, alpha);
            out << HexGrayEncode(r, g, b);
        }
    }
}

Raster* Catalog::ReadRaster (istream& in) {
    Skip(in);
    Coord w, h;
    in >> w >> h;

    Raster* raster = new Raster(w, h);
    ReadRasterData(raster, in);

    return raster;
}

void Catalog::ReadRasterData (Raster* raster, istream& in) {
    Coord w = raster->Width();
    Coord h = raster->Height();
    ColorIntensity r, g, b;
    
    char enc[hex_encode+1];
    enc[hex_encode] = '\0';

    for (int j = h-1; j >= 0; --j) {
        Skip(in);

        for (int i = 0; i < w; ++i) {
            in.get(enc, hex_encode+1);
            HexDecode(enc, r, g, b);
            raster->poke(i, j, r, g, b, 1);
        }
    }
    raster->flush();
}

void Catalog::WriteRaster (Raster* raster, ostream& out) {
    Mark(out);

    Coord w = raster->Width();
    Coord h = raster->Height();
    out << w << " " << h;

    WriteRasterData(raster, out);
}

void Catalog::WriteRasterData (Raster* raster, ostream& out) {
    Coord w = raster->Width();
    Coord h = raster->Height();
    ColorIntensity r, g, b;
    float alpha;

    for (int j = h-1; j >= 0; --j) {
        Mark(out);

        for (int i = 0; i < w; ++i) {
            raster->peek(i, j, r, g, b, alpha);
            out << HexEncode(r, g, b);
        }
    }
}

ControlInfo* Catalog::ReadControlInfo (istream& in) {
    ControlInfo* ctrlInfo = nil;

    Skip(in);
    in >> buf;

    if (buf[0] == 'i') {
        char lookahead = '~';
        in >> lookahead;

        if (lookahead != '~') {
            in.putback(lookahead);
            GraphicComp* label = (GraphicComp*) ReadComponent(in);
            char* klbl = ReadString(in);
            char* kcode = ReadString(in);

            ctrlInfo = new ControlInfo(label, klbl, kcode);
            delete klbl;
            delete kcode;
        }
    }
    return ctrlInfo;
}

void Catalog::WriteControlInfo (ControlInfo* ctrlInfo, ostream& out) {
    Mark(out);
    out << "i ";

    if (ctrlInfo == nil) {
        out << "~ ";

    } else {
        WriteComponent(ctrlInfo->GetLabel(), out);
        WriteString(ctrlInfo->GetKeyLabel(), out);
        WriteString(ctrlInfo->GetKeyCode(), out);
    }
}

EditorInfo* Catalog::ReadEditorInfo (istream& in) {
    EditorInfo* edInfo = new EditorInfo;

    char string[CHARBUFSIZE];
    char name[CHARBUFSIZE];
    char info[CHARBUFSIZE];
    char newline;

    /* extra "&& in.good()" is for libg++ */
    while (!in.eof() && in.good()) {
        *string = '\0';
        in.get(string, CHARBUFSIZE);
        in.get(newline);
        int argc = sscanf(string, "%s %s", name, info);

        if (argc > 0 && *name == '#') {
            // comment; do nothing

        } else if (argc == 1) {
            edInfo->Register(name);

        } else if (argc == 2) {
            edInfo->Register(name, info);
        }
    }
    return edInfo;
}

void Catalog::WriteEditorInfo (EditorInfo* edInfo, ostream& out) {
    for (int i = 0; i < edInfo->Count(); ++i) {
        out << edInfo->GetName(i) << " " << edInfo->GetInfo(i) << "\n";
    }
}

void Catalog::Register (EditorInfo* o, const char* name) {
    _edInfoMap->Register(o, name);
}

void Catalog::Register (Component* o, const char* name) {
    _compMap->Register(o, name);
}

void Catalog::Register (Command* o, const char* name) {
    _cmdMap->Register(o, name);
}

void Catalog::Register (Tool* o, const char* name) {
    _toolMap->Register(o, name);
}

/*****************************************************************************/

class VoidIntElem : public UMapElem {
public:
    VoidIntElem(void*, int);

    virtual void* id();
    virtual void* tag();
private:
    void* _object;
    int _id;
};

VoidIntElem::VoidIntElem (void* object, int id) {
    _object = object;
    _id = id;
}

void* VoidIntElem::id () { return _object; }
void* VoidIntElem::tag () { return (void*) _id; }

/*****************************************************************************/

class ObjectMapElem : public UHashElem {
public:
    ObjectMapElem(VoidIntElem*);
    ObjectMapElem(
        VoidIntElem*, ClassId orig_id, const char* delim, UArray* extra_data
    );
    virtual ~ObjectMapElem();

    void* GetObject();
    int GetId();
    ClassId GetOrigClassId();
    const char* GetDelim();
    UArray* GetExtraData();
public:
    VoidIntElem* _elem;
private:
    ClassId _orig_id;
    char* _delim;
    UArray* _extra_data;
};

ObjectMapElem::ObjectMapElem (VoidIntElem* elem) {
    _elem = elem;
    _orig_id = UNDEFINED_CLASS;
    _delim = nil;
    _extra_data = nil;
}

ObjectMapElem::ObjectMapElem (
    VoidIntElem* elem, ClassId orig_id, const char* delim, UArray* extra_data
) {
    _elem = elem;
    _orig_id = orig_id;
    _delim = strnew(delim);
    _extra_data = extra_data;
}

ObjectMapElem::~ObjectMapElem () {
    delete _delim;
    delete _extra_data;
}

inline void* ObjectMapElem::GetObject () { return _elem->id(); }
inline int ObjectMapElem::GetId () { return (int) _elem->tag(); }
inline ClassId ObjectMapElem::GetOrigClassId () { return _orig_id; }
inline const char* ObjectMapElem::GetDelim () { return _delim; }
inline UArray* ObjectMapElem::GetExtraData () { return _extra_data; }

/*****************************************************************************/

static const int SLOTS = 1000;

/*****************************************************************************/

inline ObjectMapElem* ObjectMap::Find (void* obj) {
    return (ObjectMapElem*) _objKeys.Find(obj);
}

inline ObjectMapElem* ObjectMap::Find (int id) {
    return (ObjectMapElem*) _idKeys.Find((void*) id);
}

ObjectMap::ObjectMap (
    void* client, ClassId id
) : _objKeys(SLOTS), _idKeys(SLOTS) {
    _client = client;
    _id = id;
}

void ObjectMap::Register (void* obj, int id) {
    VoidIntElem* elem = new VoidIntElem(obj, id);
    UMap::Register(elem);
    ObjectMapElem* objElem = new ObjectMapElem(elem);
    ObjectMapElem* idElem = new ObjectMapElem(elem);
    _objKeys.Register(obj, objElem);
    _idKeys.Register((void*) id, idElem);
}

void ObjectMap::Register (
    void* obj, int id, ClassId orig_id, const char* delim, UArray* extra_data
) {
    VoidIntElem* elem = new VoidIntElem(obj, id);
    UMap::Register(elem);
    ObjectMapElem* objElem = new ObjectMapElem(
        elem, orig_id, delim, extra_data
    );
    ObjectMapElem* idElem = new ObjectMapElem(elem);
    _objKeys.Register(obj, objElem);
    _idKeys.Register((void*) id, idElem);
}

void ObjectMap::Unregister (void* obj) {
    ObjectMapElem* objElem = Find(obj);

    if (objElem != nil) {
        _idKeys.Unregister((void*) objElem->GetId());
        _objKeys.Unregister(obj);
    }
}

void ObjectMap::Unregister (int id) {
    ObjectMapElem* idElem = Find(id);

    if (idElem != nil) {
        _objKeys.Unregister(idElem->GetObject());
        _idKeys.Unregister((void*) id);
    }
}

void* ObjectMap::GetClient () { return _client; }
ClassId ObjectMap::GetClientId () { return _id; }

void* ObjectMap::GetObject (int id) { 
    ObjectMapElem* idElem = Find(id);
    return (idElem == nil) ? nil : idElem->GetObject();
}

int ObjectMap::GetId (void* obj) {
    ObjectMapElem* objElem = Find(obj);
    return (objElem == nil) ? nil : objElem->GetId();
}

ClassId ObjectMap::GetOrigClassId (void* obj) {
    ObjectMapElem* objElem = Find(obj);
    return (objElem == nil) ? UNDEFINED_CLASS : objElem->GetOrigClassId();
}

const char* ObjectMap::GetDelim (void* obj) {
    ObjectMapElem* objElem = Find(obj);
    return (objElem == nil) ? nil : objElem->GetDelim();
}

UArray* ObjectMap::GetExtraData (void* obj) {
    ObjectMapElem* objElem = Find(obj);
    return (objElem == nil) ? nil : objElem->GetExtraData();
}
