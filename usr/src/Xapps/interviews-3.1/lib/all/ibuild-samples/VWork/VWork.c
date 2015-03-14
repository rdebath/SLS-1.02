#include <Dispatch/dispatcher.h>
#include <Unidraw/Graphic/grblock.h> 
#include <Unidraw/Graphic/picture.h> 
#include <Unidraw/Graphic/polygons.h> 
#include <Unidraw/Graphic/pspaint.h>
#include <InterViews/canvas.h>
#include <Dispatch/rpcservice.h>
#include <Dispatch/rpcreader.h>
#include <Dispatch/rpchdr.h>
#include <Dispatch/rpcstream.h>
#include <Dispatch/rpcbuf.h>
#include "VWork.h"
#include <IV-2_6/_enter.h>

static port_id = 1040;

class VWorkReader : public RpcReader {
public:
    VWorkReader(int fd);
    virtual void connectionClosed(int fd);
};

VWorkReader::VWorkReader(int fd) : RpcReader(fd, 1, false) {
    _function[0] = &VWork::RpcReceive;
}

void VWorkReader::connectionClosed(int) {}


class VWorkService : public RpcService {
public:
    VWorkService(int port);
protected:
    virtual void createReader(int fd);
};

VWorkService::VWorkService(int port) : RpcService(port) {}

void VWorkService::createReader(int fd) {
    new VWorkReader(fd);
}
 
VWork* VWork::_vwork;

VWork::VWork(const char* name) : VWork_core(name) {
    _red = new PSColor(1.0, 0.0, 0.0, "red");
    _red->ref();
    _black = new PSColor(0.0, 0.0, 0.0, "black");
    _black->ref();
    _vwork = this;
    new VWorkService(port_id);
}

void VWork::RpcReceive(RpcReader*, RpcHdr&, rpcstream& rs) {
    int request;
    rs >> request;
    if (request == 1) {
        _vwork->Window1();
    } else if (request == 2) {
        _vwork->Window2();
    } else if (request == 3) {
        _vwork->Window3();
    }
}

void VWork::Reset (Graphic* window) {
    window->SetColors(_black, window->GetBgColor());
}

void VWork::Set (Graphic* window) {
    window->SetColors(_red, window->GetBgColor());
}

void VWork::Display () {
    Canvas* screen = _myglass->GetCanvas();
    _display->Draw(screen);
}

void VWork::Window1 () {
    Reset(_window33);
    Reset(_window3);
    Reset(_window22);
    Reset(_window2);
    Set(_window11);
    Set(_window1);
    _display->Remove(_frame1);
    _display->Append(_frame1);
    Display();
}
    
void VWork::Window2 () {
    Reset(_window33);
    Reset(_window3);
    Reset(_window11);
    Reset(_window1);
    Set(_window22);
    Set(_window2);
    _display->Remove(_frame2);
    _display->Append(_frame2);
    Display();
}
    
void VWork::Window3 () {
    Reset(_window11);
    Reset(_window1);
    Reset(_window22);
    Reset(_window2);
    Set(_window33);
    Set(_window3);
    _display->Remove(_frame3);
    _display->Append(_frame3);
    Display();
}
    
