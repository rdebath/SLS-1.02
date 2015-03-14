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
 * Connector - object for defining and maintaining connectivity between
 * components.
 */

#ifndef unidraw_components_connector_h
#define unidraw_components_connector_h

#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>

class CGlue;
class CSolver;
class CSolverInfo;
class Path;
class StateVar;

class Connector : public GraphicComp {
public:
    virtual ~Connector();
    
    virtual void Connect(Connector*, CGlue* = nil);
    virtual void Disconnect(Connector*);
    virtual boolean ConnectedTo(Connector*);
    void DisconnectAll();

    virtual void GetCenter(float&, float&);
    virtual void SetBinding(StateVar*);
    virtual StateVar* GetBinding();
    virtual void SetTransMethod(TransMethod);
    virtual TransMethod GetTransMethod();
    virtual void Transmit(Path* = nil);

    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    Connector(Graphic* = nil);

    Connector* Conn(UList*);
    virtual void ConnectMe(Connector*);
    virtual void DisconnectMe(Connector*);

    void Retransmit(Path*);
    void Retransmit(Connector* peer, Path*);
    boolean Transferable(Connector* peer);
protected:
    UList* _cnxns;                             /* list of connections */
    StateVar* _stateVar;
    TransMethod _transMethod;
private:
    friend class CSolver;
    CSolverInfo* _csinfo;
};

class ConnectorView : public GraphicView {
public:
    Connector* GetConnector();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    ConnectorView(Connector* = nil);
};

#endif
