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
 * Implementation of Connectors.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/path.h>
#include <Unidraw/statevars.h>
#include <Unidraw/transfn.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/command.h>
#include <Unidraw/Commands/data.h>

#include <Unidraw/Components/cglue.h>
#include <Unidraw/Components/connector.h>
#include <Unidraw/Components/csolver.h>

#include <Unidraw/Graphic/graphic.h>

#include <stream.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

class _CSData : public Data {
public:
    _CSData(CSolverState*);
    virtual ~_CSData();
public:
    CSolverState* _state;
};

_CSData::_CSData (CSolverState* state) {
    _state = state;
}

_CSData::~_CSData () {
    delete _state;
}

/*****************************************************************************/

ClassId Connector::GetClassId () { return CONNECTOR; }

boolean Connector::IsA (ClassId id) {
    return CONNECTOR == id || GraphicComp::IsA(id);
}

Connector::Connector (Graphic* g) : GraphicComp(g) {
    _cnxns = new UList;
    _stateVar = nil;
    _transMethod = InOut;
    _csinfo = nil;
}

Connector::~Connector () {
    DisconnectAll();
    delete _cnxns;
}

void Connector::Interpret (Command* cmd) {
    if (cmd->IsA(DELETE_CMD) || cmd->IsA(CUT_CMD)) {
        _CSData* data = new _CSData(csolver->GetState(this));
        cmd->Store((Component*) _cnxns, data);
        DisconnectAll();
    }
    GraphicComp::Interpret(cmd);
}

void Connector::Uninterpret (Command* cmd) {
    if (cmd->IsA(DELETE_CMD) || cmd->IsA(CUT_CMD)) {
        _CSData* data = (_CSData*) cmd->Recall((Component*) _cnxns);

        if (data->_state != nil) {
            csolver->SetState(data->_state);
        }
    } 
    GraphicComp::Uninterpret(cmd);
}    

void Connector::Read (istream& in) {
    GraphicComp::Read(in);
    Catalog* catalog = unidraw->GetCatalog();
    int transMethod;

    in >> transMethod;
    _transMethod = TransMethod(transMethod);
    _stateVar = catalog->ReadStateVar(in);
}

void Connector::Write (ostream& out) {
    GraphicComp::Write(out);
    int transMethod = _transMethod;

    out << transMethod << " ";
    unidraw->GetCatalog()->WriteStateVar(_stateVar, out);
    csolver->Wrote(this);
}

void Connector::Connect (Connector* c, CGlue*) {
    if (c != this) {
        _cnxns->Append(new UList(c));
        ConnectMe(c);
    }
}

void Connector::Disconnect (Connector* c) {
    csolver->Disconnect(this, c);
    _cnxns->Delete(c);
    DisconnectMe(c);
}

Connector* Connector::Conn (UList* r) { return (Connector*) (*r)(); }

void Connector::DisconnectAll () {
    csolver->Disconnect(this);

    while (!_cnxns->IsEmpty()) {
        UList* r = _cnxns->First();
        Connector* conn = Conn(r);
        DisconnectMe(conn);
        _cnxns->Remove(r);
        delete r;
    }
}

boolean Connector::ConnectedTo (Connector* c) { return _cnxns->Find(c) != nil;}
void Connector::ConnectMe (Connector* c) { c->_cnxns->Append(new UList(this));}
void Connector::DisconnectMe (Connector* c) { c->_cnxns->Delete(this); }
void Connector::GetCenter (float& x, float& y) {GetGraphic()->GetCenter(x, y);}

void Connector::SetBinding (StateVar* s) {
    if (_stateVar != s) {
        if (_stateVar != nil) {
            _stateVar->SetBinding(nil);
        }
        _stateVar = s;
        _stateVar->SetBinding(this);
    }        
}

StateVar* Connector::GetBinding () { return _stateVar; }

void Connector::SetTransMethod (TransMethod tm) { _transMethod = tm; }
TransMethod Connector::GetTransMethod () { return _transMethod; }

void Connector::Transmit (Path* path) {
    if (path == nil) {
        Path newPath;
        Retransmit(&newPath);

    } else {
        Retransmit(path);
    }
}

void Connector::Retransmit (Path* path) {
    if (path->Visited(this)) {
        return;
    }
    boolean forking = _cnxns->First() != _cnxns->Last(); // fork if > 1 cnxn
    path->Visit(this);
    
    for (UList* u = _cnxns->First(); u != _cnxns->End(); u = u->Next()) {
        Connector* peer = Conn(u);

        if (!path->Visited(peer)) {
            if (forking) {
                Path fork(path);
                Retransmit(peer, &fork);
            } else {
                Retransmit(peer, path);
            }
        }
    }
}

void Connector::Retransmit (Connector* peer, Path* path) {
    if (Transferable(peer)) {
        Component* parent = peer->GetParent();
        TransferFunct* transfn = parent->GetTransferFunct();

        if (transfn != nil) {
            transfn->Evaluate(path);
        }
        parent->Update();
        peer->Retransmit(path);
    }
}

boolean Connector::Transferable (Connector* peer) {
    boolean transferable = false;
    StateVar* myVar = GetBinding();
    StateVar* peerVar = peer->GetBinding();

    if (myVar != nil && peerVar != nil) {
        TransMethod myTrans = GetTransMethod();
        TransMethod peerTrans = peer->GetTransMethod();

        transferable = 
            (myTrans == Out || myTrans == InOut) &&
            (peerTrans == In || peerTrans == InOut);

        if (transferable) {
            *peerVar = *myVar;
        }
    }
    return transferable;
}

/*****************************************************************************/

Connector* ConnectorView::GetConnector () { return (Connector*) GetSubject(); }
ConnectorView::ConnectorView (Connector* subj) : GraphicView(subj) { }
ClassId ConnectorView::GetClassId () { return CONNECTOR_VIEW; }

boolean ConnectorView::IsA (ClassId id) {
    return CONNECTOR_VIEW == id || GraphicView::IsA(id);
}
