#include <Unidraw/Graphic/grblock.h> 
#include <Unidraw/Graphic/picture.h> 
#include <Dispatch/rpcwriter.h>
#include <Dispatch/rpcstream.h>
#include <Dispatch/rpchdr.h>
#include <InterViews/canvas.h>
#include <InterViews/event.h>
#include "VMouse.h"
#include <IV-2_6/_enter.h>

static port_id = 1040;

class VMouseWriter : public RpcWriter {
public:
    VMouseWriter(const char* host, int port);
    void Request(int);
};

VMouseWriter::VMouseWriter (
    const char* host, int port
) : RpcWriter(host, port, true, false) {}

void VMouseWriter::Request(int w) {
    RpcHdr rpchdr(0);
    *_server << rpchdr << w;
    _server->flush();
}

VMouse::VMouse(const char* name, const char* host) : VMouse_core(name) {
    Graphic* parent;
    parent = _pbutton1->Parent();
    parent->Remove(_pbutton1);
    parent = _pbutton2->Parent();
    parent->Remove(_pbutton2);
    parent = _pbutton3->Parent();
    parent->Remove(_pbutton3);
    _vwriter = new VMouseWriter(host, port_id);
}

VMouse::~VMouse () {
    delete _vwriter;
}

void VMouse::Handle (Event& e) {
    if (e.leftmouse) {
        Sim_Press(_button1, _pbutton1, e);
    }
    if (e.middlemouse) {
        Sim_Press(_button2, _pbutton2, e);
    }
    if (e.rightmouse) {
        Sim_Press(_button3, _pbutton3, e);
    }
}

void VMouse::Sim_Press(Graphic* orig, Graphic* porig, Event& e) {
    Canvas* screen = _myglass->GetCanvas();
    Graphic* parent;
    if (e.eventType == DownEvent) {
        parent = orig->Parent();
        if (parent != nil) {
            orig->Erase(screen);
            parent->Remove(orig);
            parent->Append(porig);
            porig->Draw(screen);
            if (e.leftmouse) {
                _vwriter->Request(1);

            } else if (e.middlemouse) {
                _vwriter->Request(2);

            } else if (e.rightmouse) {
                _vwriter->Request(3);
            }
        }
            
    } else if (e.eventType == UpEvent) {
        parent = porig->Parent();
        if (parent != nil) {
            porig->Erase(screen);
            parent->Remove(porig);
            parent->Append(orig);
            orig->Draw(screen);
        }
    }
}
