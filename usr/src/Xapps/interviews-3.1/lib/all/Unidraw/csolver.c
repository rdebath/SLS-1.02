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
 * CSolver implementation.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/iterator.h>
#include <Unidraw/uhash.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>

#include <Unidraw/Commands/transforms.h>

#include <Unidraw/Components/cglue.h>
#include <Unidraw/Components/connector.h>
#include <Unidraw/Components/csolver.h>

#include <Unidraw/Graphic/graphic.h>

#include <IV-2_6/InterViews/shape.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <math.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/

class CSGlue {
public:
    CSGlue(
        float nat = 0, float shr = 0, float str = 0,
        float shrlim = hfil, float strlim = hfil
    );
    CSGlue(CSGlue*);

    CSGlue* Series(CSGlue*);            /* series combination */
    CSGlue* Parallel(CSGlue*);          /* parallel combination */
    CSGlue* a_Y(CSGlue* b, CSGlue* c);  /* Y combination ("this" is a) */
    CSGlue* b_Y(CSGlue* b, CSGlue* c);  /* Y combination ("this" is a) */
    CSGlue* c_Y(CSGlue* b, CSGlue* c);  /* Y combination ("this" is a) */

    void Limit(float&);
    void Reverse();
    void Print();
public:
    float _natural, _shrink, _stretch, _shrlim, _strlim;
};

CSGlue::CSGlue (float nat, float shr, float str, float shrlim, float strlim) {
    _natural = nat;
    _shrink = shr;
    _stretch = str;
    _shrlim = shrlim;
    _strlim = strlim;
}

CSGlue::CSGlue (CSGlue* glue) {
    _natural = glue->_natural;
    _shrink = glue->_shrink;
    _stretch = glue->_stretch;
    _shrlim = glue->_shrlim;
    _strlim = glue->_strlim;
}   

void CSGlue::Print () {
    cout << "CSGlue " << (int) this << ":\n";
    cout << "nat/shr/str: ";
    cout << _natural << "/" << _shrink << "/" << _stretch << "\n";
    cout << "shrlim/strlim: ";
    cout << _shrlim << "/" << _strlim << "\n";
}

CSGlue* CSGlue::Series (CSGlue* g) {
    CSGlue* combo = new CSGlue(_natural, _shrink, _stretch, _shrlim, _strlim);
    
    combo->_natural += g->_natural;
    combo->_stretch += g->_stretch;
    combo->_shrink += g->_shrink;
    combo->_strlim += g->_strlim;
    combo->_shrlim += g->_shrlim;
    
    return combo;
}

CSGlue* CSGlue::Parallel (CSGlue* g) {
    CSGlue* combo = new CSGlue;

    combo->_natural = max(_natural, g->_natural);
    combo->_stretch = min(_stretch, g->_stretch);
    combo->_shrink = min(_shrink, g->_shrink);
    combo->_strlim = min(_strlim, g->_strlim);
    combo->_shrlim = min(_shrlim, g->_shrlim);

    return combo;
}

CSGlue* CSGlue::a_Y (CSGlue* b, CSGlue* c) {
    CSGlue* combo = new CSGlue;

    combo->_natural = _natural + b->_natural;
    combo->_stretch = min(_stretch + b->_stretch, c->_stretch);
    combo->_shrink = min(_shrink + b->_shrink, c->_shrink);
    combo->_strlim = _strlim + b->_strlim;
    combo->_shrlim = _shrlim + b->_shrlim;

    return combo;
}

CSGlue* CSGlue::b_Y (CSGlue* b, CSGlue* c) {
    CSGlue* combo = new CSGlue;

    combo->_natural = c->_natural - b->_natural;
    combo->_stretch = min(b->_stretch + c->_stretch, _stretch);
    combo->_shrink = min(b->_shrink + c->_shrink, _shrink);
    combo->_strlim = c->_strlim - b->_strlim;
    combo->_shrlim = c->_shrlim - b->_shrlim;

    return combo;
}

CSGlue* CSGlue::c_Y (CSGlue* b, CSGlue* c) {
    CSGlue* combo = new CSGlue;

    combo->_natural = _natural + c->_natural;
    combo->_stretch = min(_stretch + c->_stretch, b->_stretch);
    combo->_shrink = min(_shrink + c->_shrink, b->_shrink);
    combo->_strlim = _strlim + c->_strlim;
    combo->_shrlim = _shrlim + c->_shrlim;

    return combo;
}

void CSGlue::Limit (float& deform) {
    deform = min(max(-_shrlim, deform), _strlim);
}

void CSGlue::Reverse () {
    _natural = -_natural;

    float tmp = _stretch;
    _stretch = _shrink;
    _shrink = tmp;

    tmp = _strlim;
    _strlim = _shrlim;
    _shrlim = tmp;
}

/*************************************************************************/

class CCnxn {
public:
    CCnxn(Connector* lb = nil, Connector* rt = nil, CSGlue* = nil);
    virtual ~CCnxn();

    void Print();

    void ApplyToSeries(CCnxn*, CCnxn*);
    void ApplyToParallel(CCnxn*, CCnxn*);
    void ApplyToY(CCnxn* eqb, CCnxn* eqc, CCnxn* ca, CCnxn* cb, CCnxn* cc);
    void ApplyNatural();

    void Limit();
    void Reverse();
    boolean IsFixed();
    boolean Contains(Connector*);
    virtual float GetCenter(Connector*);

    virtual CCnxn* Copy();
    void Read(istream&);
    void Write(ostream&);
public:
    Connector* _lbConn, *_rtConn;
    CSGlue* _glue;
    float _pos, _deform;
};

inline void CCnxn::Limit () { 
    _glue->Limit(_deform); 
}

CCnxn* CCnxn::Copy () {
    CCnxn* copy = new CCnxn(_lbConn, _rtConn, new CSGlue(_glue));
    copy->_pos = _pos;
    copy->_deform = _deform;
    return copy;
}

void CCnxn::Print () {
    cout << "Cnxn " << (int) this << ":\n";
    cout << "lb/rt: " << (int) _lbConn << "/" << (int) _rtConn << "\n";
    cout << "pos/deform: " << _pos << "/" << _deform << "\n";
    _glue->Print();
}

CCnxn::CCnxn (Connector* lb, Connector* rt, CSGlue* g) {
    _lbConn = lb;
    _rtConn = rt;
    _glue = g;
    _pos = _deform = 0;
}

CCnxn::~CCnxn () { delete _glue; }

void CCnxn::ApplyToSeries (CCnxn* ca, CCnxn* cb) {
    float d =
        _glue->_natural + _deform - ca->_glue->_natural - cb->_glue->_natural;
    float sa, sb;
    
    if (d < 0) {
        sa = ca->_glue->_shrink;
        sb = cb->_glue->_shrink;
    } else {
        sa = ca->_glue->_stretch;
        sb = cb->_glue->_stretch;
    }
    if (sa == 0 && sb == 0) {
        ca->_deform = 0;
    } else {
        ca->_deform = _deform * sa / (sa + sb);
    }

    ca->Limit();
    cb->_deform = _deform - ca->_deform;
    cb->Limit();
    ca->_deform = _deform - cb->_deform;
    ca->Limit();
    ca->_pos = _pos;
    cb->_pos = ca->_pos + ca->_glue->_natural + ca->_deform;
}

void CCnxn::ApplyToParallel (CCnxn* ca, CCnxn* cb) {
    ca->_pos = cb->_pos = _pos;
    ca->_deform = _glue->_natural + _deform - ca->_glue->_natural;
    cb->_deform = _glue->_natural + _deform - cb->_glue->_natural;
    ca->Limit();
    cb->Limit();
}

void CCnxn::ApplyToY (CCnxn*, CCnxn* eqc, CCnxn* ca, CCnxn* cb, CCnxn* cc) {
    float d =
        _glue->_natural + _deform - ca->_glue->_natural - cb->_glue->_natural;
    float sa, sb;
    
    if (d < 0) {
        sa = ca->_glue->_shrink;
        sb = cb->_glue->_shrink;
    } else {
        sa = ca->_glue->_stretch;
        sb = cb->_glue->_stretch;
    }
    if (sa == 0 && sb == 0) {
        ca->_deform = 0;
    } else {
        ca->_deform = _deform * sa / (sa + sb);
    }

    ca->Limit();
    cb->_deform = _deform - ca->_deform;
    cb->Limit();
    ca->_deform = _deform - cb->_deform;
    ca->Limit();
    cc->_deform = eqc->_deform - ca->_deform;
    cc->Limit();
    ca->_deform = eqc->_deform - cc->_deform;
    ca->Limit();
    cb->_deform = _deform - ca->_deform;
    cb->Limit();

    ca->_pos = _pos;
    cb->_pos = cc->_pos = ca->_pos + ca->_glue->_natural + ca->_deform;
}

void CCnxn::ApplyNatural () {
    if (IsFixed()) {
        _pos = GetCenter(_lbConn);
        _deform = GetCenter(_rtConn) - _pos - _glue->_natural;

    } else {
        _deform = 0;
        if (_rtConn->GetMobility() == Fixed) {
            _pos = GetCenter(_rtConn) - _glue->_natural;
        } else {
            _pos = GetCenter(_lbConn);
        }
    }
}

void CCnxn::Reverse () {
    Connector* tmp = _lbConn;
    _lbConn = _rtConn;
    _rtConn = tmp;
    _pos += _glue->_natural + _deform;
    _deform = -_deform;
    _glue->Reverse();
}

boolean CCnxn::IsFixed () { 
    return _lbConn->GetMobility() == Fixed && _rtConn->GetMobility() == Fixed;
}

boolean CCnxn::Contains (Connector* c) { return _lbConn == c || _rtConn == c; }
float CCnxn::GetCenter (Connector*) { return 0; }

void CCnxn::Read (istream& in) {
    Catalog* cat = unidraw->GetCatalog();

    cat->Skip(in);
    _glue = new CSGlue;
    
    in >> _pos >> _deform;
    in >> _glue->_natural >> _glue->_shrink >> _glue->_stretch;
    in >> _glue->_shrlim >> _glue->_strlim;

    _lbConn = (Connector*) cat->ReadComponent(in);
    _rtConn = (Connector*) cat->ReadComponent(in);
}

void CCnxn::Write (ostream& out) {
    Catalog* cat = unidraw->GetCatalog();

    cat->Mark(out);
    
    out << _pos << " " << _deform << " ";
    out << _glue->_natural << " ";
    out << _glue->_shrink << " ";
    out << _glue->_stretch << " ";
    out << _glue->_shrlim << " ";
    out << _glue->_strlim << " ";

    cat->WriteComponent(_lbConn, out);
    cat->WriteComponent(_rtConn, out);
}

/*************************************************************************/

class HCnxn : public CCnxn {
public:
    HCnxn(Connector* lb, Connector* rt, CSGlue*);

    virtual float GetCenter(Connector*);
    virtual CCnxn* Copy();
};

HCnxn::HCnxn (Connector* lb, Connector* rt, CSGlue* g) : CCnxn(lb, rt, g) { }

float HCnxn::GetCenter (Connector* c) {
    float cx, cy;
    
    c->GetCenter(cx, cy);
    return cx;
}

CCnxn* HCnxn::Copy () {
    CCnxn* copy = new HCnxn(_lbConn, _rtConn, new CSGlue(_glue));
    copy->_pos = _pos;
    copy->_deform = _deform;
    return copy;
}

/*************************************************************************/

class VCnxn : public CCnxn {
public:
    VCnxn(Connector* lb, Connector* rt, CSGlue*);

    virtual float GetCenter(Connector*);
    virtual CCnxn* Copy();
};

VCnxn::VCnxn (Connector* lb, Connector* rt, CSGlue* g) : CCnxn(lb, rt, g) { }

float VCnxn::GetCenter (Connector* c) {
    float cx, cy;
    
    c->GetCenter(cx, cy);
    return cy;
}

CCnxn* VCnxn::Copy () {
    CCnxn* copy = new VCnxn(_lbConn, _rtConn, new CSGlue(_glue));
    copy->_pos = _pos;
    copy->_deform = _deform;
    return copy;
}

/*************************************************************************/

class CNet : public UList {
public:
    void Print();

    void Append(CNet*);
    void Remove(CNet*);
    CNet* Find(CCnxn*);
    CNet* First();
    CNet* Last();
    CNet* End();
    CNet* Next();
    boolean IsEmpty();

    CCnxn* Cnxn();
    virtual CCnxn* CreateCnxn(
        Connector* lb = nil, Connector* rt = nil, CSGlue* = nil
    );
    virtual CNet* CreateNetwork(CCnxn* = nil);
    boolean Includes(Connector*);
    boolean IsDegenerate();
protected:
    CNet(CCnxn* = nil);
};

CNet::CNet (CCnxn* c) : UList(c) { }
inline CCnxn* CNet::Cnxn () { return (CCnxn*) _object; }
inline CNet* CNet::First () { return (CNet*) UList::First(); }
inline CNet* CNet::Last () { return (CNet*) UList::Last(); }
inline CNet* CNet::End () { return (CNet*) UList::End(); }
inline CNet* CNet::Next () { return (CNet*) UList::Next(); }
inline boolean CNet::IsEmpty () { return UList::IsEmpty(); }
inline CNet* CNet::Find (CCnxn* cnxn) { return (CNet*) UList::Find(cnxn); }
inline boolean CNet::IsDegenerate () { return First() == Last(); }

void CNet::Print () {
    for (CNet* nw = First(); nw != End(); nw = nw->Next()) {
        nw->Cnxn()->Print();
        cout << "\n";
    }
    cout.flush();
}    

CCnxn* CNet::CreateCnxn (Connector*, Connector*, CSGlue*) { return nil; }
CNet* CNet::CreateNetwork (CCnxn*) { return nil; }
void CNet::Append (CNet* nw) { UList::Append(nw); }
void CNet::Remove (CNet* nw) { UList::Remove(nw); }

boolean CNet::Includes (Connector* c) {
    for (CNet* nw = First(); nw != End(); nw = nw->Next()) {
        CCnxn* cnxn = nw->Cnxn();

        if (cnxn->_lbConn == c || cnxn->_rtConn == c) {
            return true;
        }
    }
    return false;
}

/*************************************************************************/

class HNet : public CNet {
public:
    HNet(CCnxn* = nil);

    virtual CCnxn* CreateCnxn(
        Connector* lb = nil, Connector* rt = nil, CSGlue* = nil
    );
    virtual CNet* CreateNetwork(CCnxn* = nil);
};

HNet::HNet (CCnxn* c) : CNet(c) { }
CNet* HNet::CreateNetwork (CCnxn* c) { return new HNet(c); }

CCnxn* HNet::CreateCnxn (Connector* lb, Connector* rt, CSGlue* g) {
    return new HCnxn(lb, rt, g);
}

/*************************************************************************/

class VNet : public CNet {
public:
    VNet(CCnxn* = nil);

    virtual CCnxn* CreateCnxn(
        Connector* lb = nil, Connector* rt = nil, CSGlue* = nil
    );
    virtual CNet* CreateNetwork(CCnxn* = nil);
};

VNet::VNet (CCnxn* c) : CNet(c) { }
CNet* VNet::CreateNetwork (CCnxn* c) { return new VNet(c); }

CCnxn* VNet::CreateCnxn (Connector* lb, Connector* rt, CSGlue* g) {
    return new VCnxn(lb, rt, g);
}

/*****************************************************************************/

static const int SLOTS = 1000;

/*****************************************************************************/

class CCnxn_HashElem : public UHashElem {
public:
    CCnxn_HashElem(Connector* = nil, CCnxn* = nil);
    Connector* GetConn();
    CCnxn* GetCnxn();
    void SetCnxn(CCnxn*);
private:
    CCnxn* _cnxn;
};

CCnxn_HashElem::CCnxn_HashElem (Connector* c, CCnxn* cnxn) : UHashElem(c) {
    _cnxn = cnxn;
}

inline Connector* CCnxn_HashElem::GetConn () { return (Connector*) GetKey(); }
inline CCnxn* CCnxn_HashElem::GetCnxn () { return _cnxn; }
inline void CCnxn_HashElem::SetCnxn (CCnxn* cnxn) { _cnxn = cnxn; }

/*****************************************************************************/

class CU_HashElem : public UHashElem {
public:
    CU_HashElem(Component* = nil, float = 0, float = 0);
    Component* GetComp();
    float GetX();
    float GetY();
    void SetX(float);
    void SetY(float);
private:
    float _px, _py;
};

CU_HashElem::CU_HashElem (Component* c, float px, float py) : UHashElem(c) {
    _px = px;
    _py = py;
}

inline Component* CU_HashElem::GetComp () { return (Component*) GetKey(); }

inline float CU_HashElem::GetX () { return _px; }
inline float CU_HashElem::GetY () { return _py; }

inline void CU_HashElem::SetX (float px) { _px = px; }
inline void CU_HashElem::SetY (float py) { _py = py; }

/*****************************************************************************/

class CS_HashTable : public UHashTable {
public:
    virtual ~CS_HashTable();

    void First(Iterator&);
    void Next(Iterator&);
    boolean Done(Iterator);
    void Remove(Iterator&);
    UHashElem* GetElem(Iterator);

    virtual void Register(void* key, UHashElem* = nil);
protected:
    CS_HashTable();
    UList* ULElem(Iterator);
    UHashElem* Elem(UList*);
private:
    UList* _elems;
};

UHashElem* CS_HashTable::Elem (UList* r) { return (UHashElem*) (*r)(); }
UList* CS_HashTable::ULElem (Iterator i) { return (UList*) i.GetValue(); }
boolean CS_HashTable::Done (Iterator i) { return ULElem(i) == _elems->End(); }
void CS_HashTable::First (Iterator& i) { i.SetValue(_elems->First()); }
void CS_HashTable::Next (Iterator& i) { i.SetValue(ULElem(i)->Next()); }
UHashElem* CS_HashTable::GetElem (Iterator i) { return Elem(ULElem(i)); }

void CS_HashTable::Remove (Iterator& i) {
    UList* doomed = ULElem(i);
    Next(i);
    _elems->Remove(doomed);
    delete doomed;
}

CS_HashTable::CS_HashTable () : UHashTable(SLOTS) { _elems = new UList; }
CS_HashTable::~CS_HashTable () { delete _elems; }

void CS_HashTable::Register (void* key, UHashElem* elem) {
    UHashTable::Register(key, elem);
    elem = (elem == nil) ? Find(key) : elem;
    _elems->Append(new UList(elem));
}

/*****************************************************************************/

class CCnxn_HashTable : public CS_HashTable {
public:
    CCnxn_HashTable();

    CCnxn_HashElem* GetElem(Iterator);
    CCnxn_HashElem* Find(Connector*);
protected:
    virtual UHashElem* CreateElem();
};

CCnxn_HashTable::CCnxn_HashTable () { }
UHashElem* CCnxn_HashTable::CreateElem () { return new CCnxn_HashElem; }

CCnxn_HashElem* CCnxn_HashTable::Find (Connector* c) {
    return (CCnxn_HashElem*) CS_HashTable::Find(c);
}

CCnxn_HashElem* CCnxn_HashTable::GetElem (Iterator i) {
    return (CCnxn_HashElem*) CS_HashTable::GetElem(i);
}

/*****************************************************************************/

class CU_HashTable : public CS_HashTable {
public:
    CU_HashTable();

    CU_HashElem* GetElem(Iterator);
    CU_HashElem* Find(Component*);
protected:
    virtual UHashElem* CreateElem();
};

CU_HashTable::CU_HashTable () { }
UHashElem* CU_HashTable::CreateElem () { return new CU_HashElem; }

CU_HashElem* CU_HashTable::Find (Component* c) {
    return (CU_HashElem*) CS_HashTable::Find(c);
}

CU_HashElem* CU_HashTable::GetElem (Iterator i) {
    return (CU_HashElem*) CS_HashTable::GetElem(i);
}

/*************************************************************************/

class CUpdater {
public:
    CUpdater();
    void AddHCnxn(CCnxn*);
    void AddVCnxn(CCnxn*);
    void Update();
private:
    void AddCnxn(Connector*, CCnxn*, Orientation);
    void UpdateCnxns();
    void UpdateParents();

    float Position(Connector*, CCnxn*);
    void CalcTranslation(Connector*, float, float, float&, float&);
private:
    CU_HashTable _hash;
};

CUpdater::CUpdater () { }

void CUpdater::AddHCnxn (CCnxn* cnxn) {
    AddCnxn(cnxn->_lbConn, cnxn, Horizontal);
    AddCnxn(cnxn->_rtConn, cnxn, Horizontal);
}

void CUpdater::AddVCnxn (CCnxn* cnxn) {
    AddCnxn(cnxn->_lbConn, cnxn, Vertical);
    AddCnxn(cnxn->_rtConn, cnxn, Vertical);
}

void CUpdater::Update () {
    UpdateCnxns();
    UpdateParents();
}

void CUpdater::AddCnxn (Connector* c, CCnxn* cnxn, Orientation orient) {
    if (c->GetMobility() != Fixed) {
        CU_HashElem* elem = _hash.Find(c);

        if (elem == nil) {
            elem = new CU_HashElem;
            _hash.Register(c, elem);
        }
        if (orient == Horizontal) {
            elem->SetX(Position(c, cnxn));
        } else {
            elem->SetY(Position(c, cnxn));
        }
    }
}

void CUpdater::CalcTranslation (
    Connector* c, float px, float py, float& dx, float& dy
) {
    const float thresh = .0001;
    float cx, cy, ox, oy;
    Transformer rel;

    c->GetGraphic()->Parent()->TotalTransformation(rel);
    c->GetCenter(cx, cy);
    rel.InvTransform(0., 0., ox, oy);
    rel.InvTransform(px - cx, py - cy, dx, dy);
    dx -= ox;
    dy -= oy;
    dx = (fabs(dx) < thresh) ? 0 : dx;
    dy = (fabs(dy) < thresh) ? 0 : dy;
}   

void CUpdater::UpdateCnxns () {
    float dx, dy;
    Iterator i;
    _hash.First(i);

    while (!_hash.Done(i)) {
        CU_HashElem* elem = _hash.GetElem(i);
        CalcTranslation(
            (Connector*) elem->GetComp(), elem->GetX(), elem->GetY(), dx, dy
        );

        if (dx == 0 && dy == 0) {
            _hash.Remove(i);

        } else {
            _hash.Next(i);
            MoveCmd move((Editor*) nil, dx, dy);
            elem->GetComp()->Interpret(&move);
        }
    }
}

void CUpdater::UpdateParents () {
    Iterator i;
    CU_HashTable parents;

    for (_hash.First(i); !_hash.Done(i); _hash.Next(i)) {
        Component* parent = _hash.GetElem(i)->GetComp()->GetParent();

        if (parents.Find(parent) == nil) {
            parents.Register(parent);
        }
    }
    for (parents.First(i); !parents.Done(i); parents.Next(i)) {
        parents.GetElem(i)->GetComp()->Update();
    }
}

float CUpdater::Position (Connector* c, CCnxn* cnxn) {
    if (c == cnxn->_lbConn) {
        return cnxn->_pos;
    } else {                                    // c == cnxn->_rtConn
        return cnxn->_pos + cnxn->_glue->_natural + cnxn->_deform;
    }
}

/*****************************************************************************/

class PeerInfo {
public:
    PeerInfo(Connector* = nil, int = 1);
public:
    Connector* _peer;
    int _ncnxns;
};

PeerInfo::PeerInfo (Connector* peer, int ncnxns) {
    _peer = peer;
    _ncnxns = ncnxns;
}

/*****************************************************************************/

class ConnInfo {                     // horiz/vert info stored in CSolverInfo 
public:
    ConnInfo(CNet* nw = nil);
    ~ConnInfo();

    void Include(Connector*);
    void Exclude(Connector*);

    void First(Iterator&);
    void Next(Iterator&);
    boolean Done(Iterator);
    void Append(Connector*);
    void Remove(Iterator&);
    PeerInfo* GetInfo(Iterator);
    Connector* FindParallelPeer();

    CNet* GetNetwork();
    void SetNetwork(CNet*);
    int NumPeers();
    int NumParallels();
private:
    PeerInfo* Info(UList*);
    UList* Elem(Iterator);
private:
    CNet* _net;                     // connector's network
    UList* _peers;                  // list of PeerInfos
    int _npeers;
    int _nparallels;                // tot # of peers connected in parallel
};

inline PeerInfo* ConnInfo::Info (UList* u) { return (PeerInfo*) (*u)(); }
inline UList* ConnInfo::Elem (Iterator i) { return (UList*) i.GetValue(); }

inline void ConnInfo::First (Iterator& i) { i.SetValue(_peers->First()); }
inline void ConnInfo::Next (Iterator& i) { i.SetValue(Elem(i)->Next()); }
inline boolean ConnInfo::Done (Iterator i) { return Elem(i) == _peers->End(); }
inline PeerInfo* ConnInfo::GetInfo (Iterator i) { return Info(Elem(i)); }

inline CNet* ConnInfo::GetNetwork () { return _net; }
inline void ConnInfo::SetNetwork (CNet* nw) { _net = nw; }
inline int ConnInfo::NumPeers () { return _npeers; }
inline int ConnInfo::NumParallels () { return _nparallels; }

void ConnInfo::Append (Connector* peer) {
    _peers->Append(new UList(new PeerInfo(peer)));
    ++_npeers;
}

void ConnInfo::Remove (Iterator& i) {
    UList* doomed = Elem(i);
    Next(i);
    _peers->Remove(doomed);
    delete doomed;
    --_npeers;
}

ConnInfo::ConnInfo (CNet* nw) {
    _net = nw; 
    _peers = new UList; 
    _npeers = _nparallels = 0;
}

ConnInfo::~ConnInfo () {
    while (!_peers->IsEmpty()) {
        UList* u = _peers->First();
        _peers->Remove(u);
        PeerInfo* info = Info(u);
        delete info;
        delete u;
    }
    delete _peers;
}

void ConnInfo::Include (Connector* peer) {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        PeerInfo* info = GetInfo(i);

        if (info->_peer == peer) {
            ++info->_ncnxns;
            ++_nparallels;
            return;
        }
    }
    Append(peer);
}

void ConnInfo::Exclude (Connector* peer) {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        PeerInfo* info = GetInfo(i);

        if (info->_peer == peer) {
            if (info->_ncnxns > 1) {
                --info->_ncnxns;
                --_nparallels;
            } else {
                delete info;
                Remove(i);
            }
            break;
        }
    }
}

Connector* ConnInfo::FindParallelPeer () {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        PeerInfo* info = GetInfo(i);

        if (info->_ncnxns > 1) {
            return info->_peer;
        }
    }
    return nil;
}

/*****************************************************************************/

class CSolverInfo {             /* info stored in each connector */
public:
    CSolverInfo();
    ~CSolverInfo();
public:
    ConnInfo* _hinfo, *_vinfo;
};

CSolverInfo::CSolverInfo () { _hinfo = new ConnInfo; _vinfo = new ConnInfo; }
CSolverInfo::~CSolverInfo () { delete _hinfo; delete _vinfo; }

/*************************************************************************/

CSolverState::CSolverState (CCnxn* cnxn, Orientation orient) {
    _cnxn = cnxn;
    _orient = orient;
    _next = nil;
}

CSolverState::~CSolverState () { delete _cnxn; delete _next; }

inline CSolverState* CSolverState::First () { return _next; }
inline CSolverState* CSolverState::Next () { return _next; }

void CSolverState::Append (CSolverState* state) {
    state->_next = _next;
    _next = state;
}


/*************************************************************************/

inline CNet* CSolver::Network (UList* u) { return (CNet*) (*u)(); }

void CSolver::Print () {
    CNet* hnet = Network(_hnets->Last());
    hnet->Print();
    cout << "----------------\n\n";
    cout.flush();
}

CSolver::CSolver () {
    _hnets = new UList;
    _vnets = new UList;
    _hwritten = new CCnxn_HashTable;
    _vwritten = new CCnxn_HashTable;
}

CSolver::~CSolver () {
    DestroyCnxns();
    delete _hnets;
    delete _vnets;
    delete _hwritten;
    delete _vwritten;
}

void CSolver::Connect (Connector* c1, Connector* c2, CGlue* g) {
    if (c1 != c2) {
        CSGlue* gh, *gv;

        if (g == nil) {
            gh = new CSGlue;
            gv = new CSGlue;
        } else {
            gh = new CSGlue(
                g->_width, g->_hshrink, g->_hstretch, g->_hshrlim, g->_hstrlim
            );
            gv = new CSGlue(
                g->_height, g->_vshrink, g->_vstretch, g->_vshrlim, g->_vstrlim
            );
        }
        UpdateInfo(new HCnxn(c1, c2, gh), Horizontal);
        UpdateInfo(new VCnxn(c1, c2, gv), Vertical);
    }
}

void CSolver::DestroyCnxns () {
    UList* hu, *vu;

    for (
        hu = _hnets->First(), vu = _vnets->First(); 
        hu != _hnets->End(); 
        hu = hu->Next(), vu = vu->Next()
    ) {
        DestroyCnxns(Network(hu));
        DestroyCnxns(Network(vu));
    }
}

void CSolver::DestroyCnxns (CNet* nets) {
    for (CNet* nw = nets->First(); nw != nets->End(); nw = nw->Next()) {
        CCnxn* cnxn = nw->Cnxn();
        CSolverInfo* lbinfo = cnxn->_lbConn->_csinfo;
        CSolverInfo* rtinfo = cnxn->_rtConn->_csinfo;
        delete lbinfo;
        delete rtinfo;
        cnxn->_lbConn->_csinfo = nil;
        cnxn->_rtConn->_csinfo = nil;
    }
}

void CSolver::Solve () {
    SolveAll(_hnets, Horizontal);
    SolveAll(_vnets, Vertical);
    Update();
}

void CSolver::SolveAll (UList* nets, Orientation orient) {
    for (UList* u = nets->First(); u != nets->End(); u = u->Next()) {
        Solve(Network(u), orient);
    }
}

void CSolver::Solve (CNet* net, Orientation orient) {
    CNet* nwa, *nwb, *nwc, *eqa, *eqb, *eqc;
    Connector* c1, *c2;
    boolean rva, rvb, rvc;

    if (net->IsDegenerate()) {
        DefaultPosition(net);

    } else if (FoundFixed(net, nwa)) {
        SubstFixedEquiv(net, nwa, eqa, orient);
        Solve(net, orient);
        ReplaceFixed(net, nwa, eqa, orient);

    } else if (FoundSeries(net, nwa, nwb, orient)) {
        SubstSeriesEquiv(net, nwa, nwb, eqa, rva, rvb, orient);
        Solve(net, orient);
        ReplaceSeries(net, nwa, nwb, eqa, rva, rvb, orient);

    } else if (FoundParallel(net, nwa, nwb, orient)) {
        SubstParallelEquiv(net, nwa, nwb, eqa, rva, orient);
        Solve(net, orient);
        ReplaceParallel(net, nwa, nwb, eqa, rva, orient);
        
    } else if (FoundY(net, nwa, nwb, nwc, orient)) {
        SubstYEquiv(net, nwa, nwb, nwc, eqa, eqb, eqc, rva, rvb, rvc, orient);
        Solve(net, orient);
        ReplaceY(net, nwa, nwb, nwc, eqa, eqb, eqc, rva, rvb, rvc, orient);

    } else if (Found2Fixed(net, c1, c2)) {
        SubstPseudoFixed(net, c1, c2, eqa, orient);
        Solve(net, orient);
        ReplacePseudoFixed(net, c1, c2, eqa, orient);

    } else {
        DefaultPosition(net);
    }
}

void CSolver::Update () {
    CUpdater cupdater;
    UList* hu, *vu;

    for (
        hu = _hnets->First(), vu = _vnets->First(); 
        hu != _hnets->End(); 
        hu = hu->Next(), vu = vu->Next()
    ) {
        CNet* hcl = Network(hu);
        CNet* vcl = Network(vu);
        CNet* nw;

        for (nw = hcl->First(); nw != hcl->End(); nw = nw->Next()) {
            cupdater.AddHCnxn(nw->Cnxn());
        }
        for (nw = vcl->First(); nw != vcl->End(); nw = nw->Next()) {
            cupdater.AddVCnxn(nw->Cnxn());
        }
    }
    cupdater.Update();
}

void CSolver::InitInfo (Connector* conn) {
    if (conn->_csinfo == nil) {
        conn->_csinfo = new CSolverInfo;
    }
}

void CSolver::UpdateInfo (CCnxn* cnxn, Orientation orient) {
    Connector* lb = cnxn->_lbConn;
    Connector* rt = cnxn->_rtConn;

    InitInfo(lb);
    InitInfo(rt);

    ConnInfo* lbinfo, *rtinfo;
    UList* nets;

    if (orient == Horizontal) {
        lbinfo = lb->_csinfo->_hinfo;
        rtinfo = rt->_csinfo->_hinfo;
        nets = _hnets;

    } else {
        lbinfo = lb->_csinfo->_vinfo;
        rtinfo = rt->_csinfo->_vinfo;
        nets = _vnets;
    }
    UpdateInfo(cnxn, lbinfo, rtinfo, nets);
}

void CSolver::UpdateInfo (
    CCnxn* cnxn, ConnInfo* lbinfo, ConnInfo* rtinfo, UList* nets
) {
    if (lbinfo->GetNetwork() == nil) {
        if (rtinfo->GetNetwork() == nil) {     // create new nw, add it to nets
            CreateNetwork(cnxn, lbinfo, rtinfo, nets);

        } else {                               // add cnxn to rtConn's nw
            CNet* nw = rtinfo->GetNetwork();
            lbinfo->SetNetwork(nw);
            nw->Append(nw->CreateNetwork(cnxn));
        }

    } else if (rtinfo->GetNetwork() == nil) {  // add cnxn to lbConn's nw
        CNet* nw = lbinfo->GetNetwork();
        rtinfo->SetNetwork(nw);
        nw->Append(nw->CreateNetwork(cnxn));

    } else if (lbinfo->GetNetwork() != rtinfo->GetNetwork()) {
        CNet* nw = lbinfo->GetNetwork();       // merge their nets
        MergeNetworks(nw, rtinfo->GetNetwork(), nets);
        nw->Append(nw->CreateNetwork(cnxn));

    } else {                                   // add cnxn to their nw
        CNet* nw = lbinfo->GetNetwork();
        nw->Append(nw->CreateNetwork(cnxn));
    }

    lbinfo->Include(cnxn->_rtConn);
    rtinfo->Include(cnxn->_lbConn);
}    

void CSolver::CreateNetwork (
    CCnxn* cnxn, ConnInfo* lbinfo, ConnInfo* rtinfo, UList* nets
) {
    CNet* nw = (nets == _hnets) ? (CNet*) new HNet : (CNet*) new VNet;

    lbinfo->SetNetwork(nw);
    rtinfo->SetNetwork(nw);
    nw->Append(nw->CreateNetwork(cnxn));
    nets->Append(new UList(nw));
}

void CSolver::MergeNetworks (CNet* merge, CNet* doomed, UList* nets) {
    if (nets == _hnets) {
        while (!doomed->IsEmpty()) {
            CNet* nw = doomed->First();
            doomed->Remove(nw);
            merge->Append(nw);

            CCnxn* cnxn = nw->Cnxn();
            cnxn->_lbConn->_csinfo->_hinfo->SetNetwork(merge);
            cnxn->_rtConn->_csinfo->_hinfo->SetNetwork(merge);
        }

    } else {
        while (!doomed->IsEmpty()) {
            CNet* nw = doomed->First();
            doomed->Remove(nw);
            merge->Append(nw);

            CCnxn* cnxn = nw->Cnxn();
            cnxn->_lbConn->_csinfo->_vinfo->SetNetwork(merge);
            cnxn->_rtConn->_csinfo->_vinfo->SetNetwork(merge);
        }
    }
    nets->Delete(doomed);
    delete doomed;
}

void CSolver::Disconnect (Connector* c) {
    CSolverInfo* csinfo = c->_csinfo;

    if (csinfo != nil) {
        DeleteCnxnsTo(c, csinfo->_hinfo->GetNetwork(), _hnets);
        DeleteCnxnsTo(c, csinfo->_vinfo->GetNetwork(), _vnets);
        DeletePeerInfo(c, csinfo->_hinfo);
        DeletePeerInfo(c, csinfo->_vinfo);

        delete csinfo;
        c->_csinfo = nil;
    }        
}

void CSolver::DeleteCnxnsTo (Connector* c, CNet* net, UList* nets) {
    CNet* next;

    if (net != nil) {
        for (CNet* nw = net->First(); nw != net->End(); nw = next) {
            CCnxn* cnxn = nw->Cnxn();
            next = nw->Next();

            if (cnxn->Contains(c)) {
                net->Remove(nw);
                delete cnxn;
                delete nw;
            }
        }
        if (net->IsEmpty()) {
            nets->Delete(net);
            delete net;
        }
    }
}

void CSolver::Disconnect (Connector* c1, Connector* c2) {
    CSolverInfo* csinfo1 = c1->_csinfo;
    CSolverInfo* csinfo2 = c2->_csinfo;

    if (csinfo1 != nil && csinfo2 != nil) {
        CNet* hnw = csinfo1->_hinfo->GetNetwork();
        CNet* vnw = csinfo1->_vinfo->GetNetwork();

        DeleteCnxnsBetween(c1, c2, hnw);
        DeleteCnxnsBetween(c1, c2, vnw);
        DeletePeerInfo(c1, csinfo1->_hinfo, c2);
        DeletePeerInfo(c1, csinfo1->_vinfo, c2);

        boolean bothEmpty = hnw->IsEmpty() && vnw->IsEmpty();

        if (bothEmpty || hnw->IsEmpty()) {
            _hnets->Delete(hnw);
            delete hnw;
        }
        if (bothEmpty || vnw->IsEmpty()) {
            _vnets->Delete(vnw);
            delete vnw;
        }
        if (bothEmpty) {
            delete csinfo1;
            delete csinfo2;
            c1->_csinfo = nil;
            c2->_csinfo = nil;
        }
    }
}

void CSolver::DeleteCnxnsBetween (Connector* c1, Connector* c2, CNet* net) {
    CNet* next;

    if (net != nil) {
        for (CNet* nw = net->First(); nw != net->End(); nw = next) {
            CCnxn* cnxn = nw->Cnxn();
            next = nw->Next();

            if (cnxn->Contains(c1) && cnxn->Contains(c2)) {
                net->Remove(nw);
                delete cnxn;
                delete nw;
            }
        }
    }
}

void CSolver::DeletePeerInfo (Connector* c, ConnInfo* info) {
    Iterator i;

    for (info->First(i); !info->Done(i); info->First(i)) {
        DeletePeerInfo(c, info, info->GetInfo(i)->_peer);
    }
}

void CSolver::DeletePeerInfo (Connector* c, ConnInfo* info, Connector* peer) {
    info->Exclude(peer);
    ConnInfo* pci;

    if (info == c->_csinfo->_hinfo) {
        pci = peer->_csinfo->_hinfo;
    } else {
        pci = peer->_csinfo->_vinfo;
    }
    pci->Exclude(c);
    
    if (pci->NumPeers() == 0) {
        pci->SetNetwork(nil);
    }
    if (info->NumPeers() == 0) {
        info->SetNetwork(nil);
    }
}

CSolverState* CSolver::GetState (Connector* c) {
    CSolverInfo* csinfo = c->_csinfo;
    CSolverState* state = nil;

    if (csinfo != nil) {
        state = new CSolverState;
        GetState(c, csinfo->_hinfo, Horizontal, state);
        GetState(c, csinfo->_vinfo, Vertical, state);
    }
    return state;
}

void CSolver::GetState (
    Connector* c, ConnInfo* info, Orientation orient, CSolverState* state
) {
    CNet* net = info->GetNetwork();

    if (net != nil) {
        for (CNet* nw = net->First(); nw != net->End(); nw = nw->Next()) {
            CCnxn* cnxn = nw->Cnxn();

            if (cnxn->Contains(c)) {
                state->Append(new CSolverState(cnxn->Copy(), orient));
            }
        }
    }
}

void CSolver::SetState (CSolverState* state) {
    for (CSolverState* s = state->First(); s != nil; s = s->Next()) {
        UpdateInfo(s->_cnxn->Copy(), s->_orient);

        Connector* lbConn = s->_cnxn->_lbConn;
        Connector* rtConn = s->_cnxn->_rtConn;

        if (!lbConn->ConnectedTo(rtConn)) {
            lbConn->Connector::Connect(rtConn);
        }
    }        
}

void CSolver::Wrote (Connector* c) {
    if (c->_csinfo != nil) {
        Wrote(c, c->_csinfo->_hinfo->GetNetwork(), _hwritten);
        Wrote(c, c->_csinfo->_vinfo->GetNetwork(), _vwritten);
    }
}

void CSolver::Wrote (Connector* c, CNet* nw, CCnxn_HashTable* written) {
    if (nw != nil && written->Find(c) == nil) {
        for (CNet* n = nw->First(); n != nw->End(); n = n->Next()) {
            CCnxn* cnxn = n->Cnxn();
            Connector* lbConn = cnxn->_lbConn;
            Connector* rtConn = cnxn->_rtConn;

            if (
                lbConn == c && written->Find(rtConn) == nil || 
                rtConn == c && written->Find(lbConn) == nil
            ) {
                CCnxn_HashElem* elem = new CCnxn_HashElem;
                elem->SetCnxn(cnxn);
                written->Register(c, elem);

            } else if (lbConn == c || rtConn == c) {
                written->Register(c);
            }
        }
    }
}

void CSolver::InitConnectors (CNet* hnw, CNet* vnw) {
    CCnxn* hcnxn, *vcnxn;
    CNet* h = hnw->First();
    CNet* v = vnw->First();

    for (;;) {
        if (h == hnw->End()) {
            if (v == vnw->End()) {
                break;
            } else {
                vcnxn = v->Cnxn();
                v = v->Next();
                UpdateInfo(vcnxn, Vertical);
            }

        } else {
            hcnxn = h->Cnxn();
            h = h->Next();

            if (v != vnw->End()) {
                vcnxn = v->Cnxn();
                v = v->Next();
                UpdateInfo(vcnxn, Vertical);
            }
            UpdateInfo(hcnxn, Horizontal);
        }
    }
}

void CSolver::ReadConnectors (istream& in, CNet* nw) {
    unidraw->GetCatalog()->Skip(in);

    int count;
    in >> count;
    
    for (int i = 0; i < count; ++i) {
        CCnxn* cnxn = nw->CreateCnxn();
        cnxn->Read(in);
        nw->Append(nw->CreateNetwork(cnxn));

        Connector* lbConn = cnxn->_lbConn;
        Connector* rtConn = cnxn->_rtConn;

        if (!lbConn->ConnectedTo(rtConn)) {
            lbConn->Connector::Connect(rtConn);
        }
    }
}

void CSolver::WriteConnectors (ostream& out, CCnxn_HashTable* written) {
    unidraw->GetCatalog()->Mark(out);
    Iterator i;
    int count = 0;

    for (written->First(i); !written->Done(i); written->Next(i)) {
        CCnxn* cnxn = written->GetElem(i)->GetCnxn();

        if (cnxn != nil) {
            Connector* lbConn = cnxn->_lbConn;
            Connector* rtConn = cnxn->_rtConn;

            if (written->Find(lbConn) != nil && written->Find(rtConn) != nil) {
                ++count;
            }
        }
    }
    out << count << " ";

    for (written->First(i); !written->Done(i); written->Next(i)) {
        CCnxn* cnxn = written->GetElem(i)->GetCnxn();

        if (cnxn != nil) {
            Connector* lbConn = cnxn->_lbConn;
            Connector* rtConn = cnxn->_rtConn;

            if (written->Find(lbConn) != nil && written->Find(rtConn) != nil) {
                cnxn->Write(out);
            }
        }
    }
}

void CSolver::Read (istream& in) {
    HNet hnet;
    VNet vnet;
    ReadConnectors(in, &hnet);
    ReadConnectors(in, &vnet);
    InitConnectors(&hnet, &vnet);
}

void CSolver::Write (ostream& out) {
    WriteConnectors(out, _hwritten);
    WriteConnectors(out, _vwritten);
    delete _hwritten;
    delete _vwritten;
    _hwritten = new CCnxn_HashTable;
    _vwritten = new CCnxn_HashTable;
}

boolean CSolver::FoundFixed (CNet* net, CNet*& nw) { 
    for (nw = net->First(); nw != net->End(); nw = nw->Next()) {
        if (nw->Cnxn()->IsFixed()) {
            return true;
        }
    }
    return false;
}

inline Connector* Shared (CCnxn* c1, CCnxn* c2) {
    if (c1->_lbConn == c2->_lbConn || c1->_lbConn == c2->_rtConn) {
        return c1->_lbConn;
    } else {
        return c1->_rtConn;
    }
}

inline boolean IsCrossover (CCnxn* c1, CCnxn* c2) {
    return c1->_lbConn == c2->_rtConn && c1->_rtConn == c2->_lbConn;
}

ConnInfo* CSolver::Info (Connector* c, Orientation orient) {
    return (orient == Horizontal) ? c->_csinfo->_hinfo : c->_csinfo->_vinfo;
}

static Connector* FindFixed (CNet*& nw, CNet* end) {
    for (nw = nw->Next(); nw != end; nw = nw->Next()) {
        CCnxn* c = nw->Cnxn();

        if (c->_lbConn->GetMobility() == Fixed) {
            return c->_lbConn;

        } else if (c->_rtConn->GetMobility() == Fixed) {
            return c->_rtConn;
        }
    }
    return nil;
}

static CNet* FindSeries (Connector* conn, CNet* begin, CNet* end) {
    for (CNet* nw = begin; nw != end; nw = nw->Next()) {
        if (nw->Cnxn()->Contains(conn)) {
            return nw;
        }
    }
    return nil;                                 // shouldn't happen
}

static void FindParallel (
    Connector* conn, ConnInfo* info, CNet* begin, CNet* end,
    CNet*& nwa, CNet*& nwb
) {
    Connector* peer = info->FindParallelPeer();

    for (nwa = begin; nwa != end; nwa = nwa->Next()) {
        CCnxn* test = nwa->Cnxn();

        if (test->Contains(conn) && test->Contains(peer)) {
            for (nwb = nwa->Next(); nwb != end; nwb = nwb->Next()) {
                test = nwb->Cnxn();

                if (test->Contains(conn) && test->Contains(peer)) {
                    return;
                }
            }
        }
    }
}

static void FindY (
    Connector* conn, CNet* begin, CNet* end, CNet*& nwb, CNet*& nwc
) {
    for (nwb = begin; nwb != end; nwb = nwb->Next()) {
        if (nwb->Cnxn()->Contains(conn)) {
            for (nwc = nwb->Next(); nwc != end; nwc = nwc->Next()) {
                if (nwc->Cnxn()->Contains(conn)) {
                    return;
                }
            }
        }
    }
}

boolean CSolver::Found2Fixed (CNet* net, Connector*& c1, Connector*& c2) {
    CNet* cur = net, *end = net->End();
    c1 = FindFixed(cur, end);

    if (c1 == nil) {
        return false;
    }

    do {
        c2 = FindFixed(cur, end);
    } while (c2 != nil && c2 == c1);

    return c2 != nil;
}

boolean CSolver::FoundSeries (
    CNet* net, CNet*& nwa, CNet*& nwb, Orientation orient
) {
    for (nwa = net->First(); nwa != net->End(); nwa = nwa->Next()) {
        CCnxn* cnxn = nwa->Cnxn();
        ConnInfo* lbinfo = Info(cnxn->_lbConn, orient);
        ConnInfo* rtinfo = Info(cnxn->_rtConn, orient);

        if (
            lbinfo->NumPeers() == 2 && lbinfo->NumParallels() == 0 &&
            cnxn->_lbConn->GetMobility() != Fixed
        ) {
            nwb = FindSeries(cnxn->_lbConn, nwa->Next(), net->End());
            return true;
        }

        if (
            rtinfo->NumPeers() == 2 && rtinfo->NumParallels() == 0 &&
            cnxn->_rtConn->GetMobility() != Fixed
        ) {
            nwb = FindSeries(cnxn->_rtConn, nwa->Next(), net->End());
            return true;
        }
    }
    return false;
}

boolean CSolver::FoundParallel (
    CNet* net, CNet*& nwa, CNet*& nwb, Orientation orient
) {
    for (nwa = net->First(); nwa != net->End(); nwa = nwa->Next()) {
        CCnxn* cnxn = nwa->Cnxn();
        ConnInfo* lbinfo = Info(cnxn->_lbConn, orient);
        ConnInfo* rtinfo = Info(cnxn->_rtConn, orient);

        if (lbinfo->NumParallels() > 0) {
            FindParallel(cnxn->_lbConn, lbinfo, nwa, net->End(), nwa, nwb);
            return true;
        }

        if (rtinfo->NumParallels() > 0) {
            FindParallel(cnxn->_rtConn, rtinfo, nwa, net->End(), nwa, nwb);
            return true;
        }
    }
    return false;
}

boolean CSolver::FoundY (
    CNet* net, CNet*& nwa, CNet*& nwb, CNet*& nwc, Orientation orient
) {
    for (nwa = net->First(); nwa != net->End(); nwa = nwa->Next()) {
        CCnxn* cnxn = nwa->Cnxn();
        ConnInfo* lbinfo = Info(cnxn->_lbConn, orient);
        ConnInfo* rtinfo = Info(cnxn->_rtConn, orient);

        if (
            lbinfo->NumPeers() == 3 && lbinfo->NumParallels() == 0 &&
            cnxn->_lbConn->GetMobility() != Fixed
        ) {
            FindY(cnxn->_lbConn, nwa->Next(), net->End(), nwb, nwc);
            return true;
        }

        if (
            rtinfo->NumPeers() == 3 && rtinfo->NumParallels() == 0 &&
            cnxn->_rtConn->GetMobility() != Fixed
        ) {
            FindY(cnxn->_rtConn, nwa->Next(), net->End(), nwb, nwc);
            return true;
        }
    }
    return false;
}

void CSolver::SubstFixedEquiv (
    CNet* net, CNet* nw, CNet*& next, Orientation orient
) {
    next = nw->Next();
    net->Remove(nw);
    SubstFixedInfo(nw, orient);
}

void CSolver::SubstSeriesEquiv (
    CNet* net, CNet* nwa, CNet* nwb, CNet*& equiv, 
    boolean& rva, boolean& rvb, Orientation orient
) {
    CCnxn* ca = nwa->Cnxn();
    CCnxn* cb = nwb->Cnxn();
    Connector* shared = Shared(ca, cb);

    if (rva = (shared == ca->_lbConn)) ca->Reverse();
    if (rvb = (shared == cb->_rtConn)) cb->Reverse();

    equiv = net->CreateNetwork(
        net->CreateCnxn(
            ca->_lbConn, cb->_rtConn, ca->_glue->Series(cb->_glue)
        )
    );
    nwa->Append(equiv);
    net->Remove(nwa);
    net->Remove(nwb);

    SubstSeriesInfo(nwa, nwb, orient);
}

void CSolver::SubstParallelEquiv (
    CNet* net, CNet* nwa, CNet* nwb, CNet*& equiv, boolean& reversed,
    Orientation orient
) {
    CCnxn* ca = nwa->Cnxn();
    CCnxn* cb = nwb->Cnxn();

    reversed = IsCrossover(ca, cb);
    if (reversed) {
        ca->Reverse();
    }

    equiv = net->CreateNetwork(
        net->CreateCnxn(
            ca->_lbConn, cb->_rtConn, ca->_glue->Parallel(cb->_glue)
        )
    );
    nwa->Append(equiv);
    net->Remove(nwa);
    net->Remove(nwb);

    SubstParallelInfo(nwa, orient);
}

void CSolver::SubstYEquiv (
    CNet* net, 
    CNet* nwa, CNet* nwb, CNet* nwc, CNet*& nweqa, CNet*& nweqb, CNet*& nweqc,
    boolean& rva, boolean& rvb, boolean& rvc, Orientation orient
) {
    CCnxn* ca = nwa->Cnxn();
    CCnxn* cb = nwb->Cnxn();
    CCnxn* cc = nwc->Cnxn();
    Connector* shared = Shared(ca, cb);

    if (rva = (shared == ca->_lbConn)) ca->Reverse();
    if (rvb = (shared == cb->_rtConn)) cb->Reverse();
    if (rvc = (shared == cc->_rtConn)) cc->Reverse();

    nweqa = net->CreateNetwork(
        net->CreateCnxn(
            ca->_lbConn, cb->_rtConn, ca->_glue->a_Y(cb->_glue, cc->_glue)
        )
    );
    nweqb = net->CreateNetwork(
        net->CreateCnxn(
            cb->_rtConn, cc->_rtConn, ca->_glue->b_Y(cb->_glue, cc->_glue)
        )
    );
    nweqc = net->CreateNetwork(
        net->CreateCnxn(
            ca->_lbConn, cc->_rtConn, ca->_glue->c_Y(cb->_glue, cc->_glue)
        )
    );
    nwa->Append(nweqa);
    nwb->Append(nweqb);
    nwc->Append(nweqc);
    net->Remove(nwa);
    net->Remove(nwb);
    net->Remove(nwc);

    SubstYInfo(nwa, nwb, nwc, orient);
}

void CSolver::SubstPseudoFixed (
    CNet* net, Connector* c1, Connector* c2, CNet*& equiv, Orientation orient
) {
    float cx1, cy1, cx2, cy2; 
    c1->GetCenter(cx1, cy1);
    c2->GetCenter(cx2, cy2);
    
    float nat = (orient == Horizontal) ? (cx2 - cx1) : (cy2 - cy1);

    equiv = net->CreateNetwork(
        net->CreateCnxn(c1, c2, new CSGlue(nat, 0, 0, 0, 0))
    );
    net->Append(equiv);
    SubstPseudoFixedInfo(equiv, orient);

    c2->SetMobility(Floating);
}

void CSolver::ReplaceFixed (
    CNet*, CNet* nw, CNet*& next, Orientation orient
) {
    ReplaceFixedInfo(nw, orient);
    nw->Cnxn()->ApplyNatural();
    next->Append(nw);
}

void CSolver::ReplaceSeries (
    CNet* net, CNet* nwa, CNet* nwb, CNet*& equiv, 
    boolean rva, boolean rvb, Orientation orient
) {
    CCnxn* ca = nwa->Cnxn();
    CCnxn* cb = nwb->Cnxn();
    CCnxn* eq = equiv->Cnxn();

    ReplaceSeriesInfo(nwa, nwb, orient);
    eq->ApplyToSeries(ca, cb);

    equiv->Append(nwa);
    equiv->Append(nwb);
    net->Remove(equiv);
    delete eq;
    delete equiv;
    equiv = nil;

    if (rva) ca->Reverse();
    if (rvb) cb->Reverse();
}

void CSolver::ReplaceParallel (
    CNet* net, CNet* nwa, CNet* nwb, CNet*& equiv, boolean reversed,
    Orientation orient
) {
    CCnxn* ca = nwa->Cnxn();
    CCnxn* cb = nwb->Cnxn();
    CCnxn* eq = equiv->Cnxn();

    ReplaceParallelInfo(nwa, orient);
    eq->ApplyToParallel(ca, cb);

    equiv->Append(nwa);
    equiv->Append(nwb);
    net->Remove(equiv);
    delete eq;
    delete equiv;
    equiv = nil;

    if (reversed) ca->Reverse();
}

void CSolver::ReplaceY (
    CNet* net, 
    CNet* nwa, CNet* nwb, CNet* nwc, CNet*& nweqa, CNet*& nweqb, CNet*& nweqc,
    boolean rva, boolean rvb, boolean rvc, Orientation orient
) {
    CCnxn* ca = nwa->Cnxn();
    CCnxn* cb = nwb->Cnxn();
    CCnxn* cc = nwc->Cnxn();
    CCnxn* eqa = nweqa->Cnxn();
    CCnxn* eqb = nweqb->Cnxn();
    CCnxn* eqc = nweqc->Cnxn();

    ReplaceYInfo(nwa, nwb, nwc, orient);
    eqa->ApplyToY(eqb, eqc, ca, cb, cc);

    nweqa->Append(nwa);
    nweqb->Append(nwb);
    nweqc->Append(nwc);
    net->Remove(nweqa);
    net->Remove(nweqb);
    net->Remove(nweqc);

    delete eqa;
    delete eqb;
    delete eqc;
    delete nweqa;
    delete nweqb;
    delete nweqc;
    nweqa = nweqb = nweqc = nil;

    if (rva) ca->Reverse();
    if (rvb) cb->Reverse();
    if (rvc) cc->Reverse();
}

void CSolver::ReplacePseudoFixed (
    CNet* net, Connector*, Connector* c2, CNet*& equiv, Orientation orient
) {
    CCnxn* eq = equiv->Cnxn();

    ReplacePseudoFixedInfo(equiv, orient);
    net->Remove(equiv);
    delete eq;
    delete equiv;
    equiv = nil;

    c2->SetMobility(Fixed);
}

void CSolver::DefaultPosition (CNet* net) {
    for (CNet* nw = net->First(); nw != net->End(); nw = nw->Next()) {
        nw->Cnxn()->ApplyNatural();
    }
}

void CSolver::SubstFixedInfo (CNet* nwa, Orientation orient) {
    CCnxn* cnxn = nwa->Cnxn();
    ConnInfo* lbinfo = Info(cnxn->_lbConn, orient);
    ConnInfo* rtinfo = Info(cnxn->_rtConn, orient);

    lbinfo->Exclude(cnxn->_rtConn);
    rtinfo->Exclude(cnxn->_lbConn);
}

void CSolver::SubstSeriesInfo (CNet* nwa, CNet* nwb, Orientation orient) {
    CCnxn* ca = nwa->Cnxn();
    CCnxn* cb = nwb->Cnxn();
    Connector* shared = Shared(ca, cb);
    Connector* conna = (ca->_lbConn == shared) ? ca->_rtConn : ca->_lbConn;
    Connector* connb = (cb->_lbConn == shared) ? cb->_rtConn : cb->_lbConn;
    ConnInfo* infoa = Info(conna, orient);
    ConnInfo* infob = Info(connb, orient);

    infoa->Include(connb);
    infob->Include(conna);
    infoa->Exclude(shared);
    infob->Exclude(shared);
}

void CSolver::SubstParallelInfo (CNet* nwa, Orientation orient) {
    CCnxn* cnxn = nwa->Cnxn();
    ConnInfo* lbinfo = Info(cnxn->_lbConn, orient);
    ConnInfo* rtinfo = Info(cnxn->_rtConn, orient);
    
    lbinfo->Exclude(cnxn->_rtConn);
    rtinfo->Exclude(cnxn->_lbConn);
}

void CSolver::SubstYInfo (CNet* nwa, CNet* nwb, CNet* nwc, Orientation orient){
    CCnxn* ca = nwa->Cnxn();
    CCnxn* cb = nwb->Cnxn();
    CCnxn* cc = nwc->Cnxn();
    Connector* shared = Shared(ca, cb);
    Connector* conna = (ca->_lbConn == shared) ? ca->_rtConn : ca->_lbConn;
    Connector* connb = (cb->_lbConn == shared) ? cb->_rtConn : cb->_lbConn;
    Connector* connc = (cc->_lbConn == shared) ? cc->_rtConn : cc->_lbConn;
    ConnInfo* infoa = Info(conna, orient);
    ConnInfo* infob = Info(connb, orient);
    ConnInfo* infoc = Info(connc, orient);

    infoa->Include(connb);
    infoa->Include(connc);
    infob->Include(conna);
    infob->Include(connc);
    infoc->Include(conna);
    infoc->Include(connb);

    infoa->Exclude(shared);
    infob->Exclude(shared);
    infoc->Exclude(shared);
}

void CSolver::SubstPseudoFixedInfo (CNet* nwa, Orientation orient) {
    CCnxn* cnxn = nwa->Cnxn();
    ConnInfo* lbinfo = Info(cnxn->_lbConn, orient);
    ConnInfo* rtinfo = Info(cnxn->_rtConn, orient);

    lbinfo->Include(cnxn->_rtConn);
    rtinfo->Include(cnxn->_lbConn);
}

void CSolver::ReplaceFixedInfo (CNet* nwa, Orientation orient) {
    CCnxn* cnxn = nwa->Cnxn();
    ConnInfo* lbinfo = Info(cnxn->_lbConn, orient);
    ConnInfo* rtinfo = Info(cnxn->_rtConn, orient);
    
    lbinfo->Include(cnxn->_rtConn);
    rtinfo->Include(cnxn->_lbConn);
}

void CSolver::ReplaceSeriesInfo (CNet* nwa, CNet* nwb, Orientation orient) {
    CCnxn* ca = nwa->Cnxn();
    CCnxn* cb = nwb->Cnxn();
    Connector* shared = Shared(ca, cb);
    Connector* conna = (ca->_lbConn == shared) ? ca->_rtConn : ca->_lbConn;
    Connector* connb = (cb->_lbConn == shared) ? cb->_rtConn : cb->_lbConn;
    ConnInfo* infoa = Info(conna, orient);
    ConnInfo* infob = Info(connb, orient);

    infoa->Include(shared);
    infob->Include(shared);
    infoa->Exclude(connb);
    infob->Exclude(conna);
}

void CSolver::ReplaceParallelInfo (CNet* nwa, Orientation orient) {
    CCnxn* cnxn = nwa->Cnxn();
    ConnInfo* lbinfo = Info(cnxn->_lbConn, orient);
    ConnInfo* rtinfo = Info(cnxn->_rtConn, orient);
    
    lbinfo->Include(cnxn->_rtConn);
    rtinfo->Include(cnxn->_lbConn);
}

void CSolver::ReplaceYInfo (
    CNet* nwa, CNet* nwb, CNet* nwc, Orientation orient
) {
    CCnxn* ca = nwa->Cnxn();
    CCnxn* cb = nwb->Cnxn();
    CCnxn* cc = nwc->Cnxn();
    Connector* shared = Shared(ca, cb);
    Connector* conna = (ca->_lbConn == shared) ? ca->_rtConn : ca->_lbConn;
    Connector* connb = (cb->_lbConn == shared) ? cb->_rtConn : cb->_lbConn;
    Connector* connc = (cc->_lbConn == shared) ? cc->_rtConn : cc->_lbConn;
    ConnInfo* infoa = Info(conna, orient);
    ConnInfo* infob = Info(connb, orient);
    ConnInfo* infoc = Info(connc, orient);

    infoa->Include(shared);
    infob->Include(shared);
    infoc->Include(shared);

    infoa->Exclude(connb);
    infoa->Exclude(connc);
    infob->Exclude(conna);
    infob->Exclude(connc);
    infoc->Exclude(conna);
    infoc->Exclude(connb);
}

void CSolver::ReplacePseudoFixedInfo (CNet* nwa, Orientation orient) {
    CCnxn* cnxn = nwa->Cnxn();
    ConnInfo* lbinfo = Info(cnxn->_lbConn, orient);
    ConnInfo* rtinfo = Info(cnxn->_rtConn, orient);
    
    lbinfo->Exclude(cnxn->_rtConn);
    rtinfo->Exclude(cnxn->_lbConn);
}
