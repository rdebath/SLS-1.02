#ifndef VWork_h
#define VWork_h

#include "VWork-core.h"

class PSColor;
class RpcReader;
class RpcHdr;
class rpcstream;

class VWork : public VWork_core {
public:
    VWork(const char*);

    static void RpcReceive(RpcReader*, RpcHdr&, rpcstream&);
protected:
    void Window1();
    void Window2();
    void Window3();

private:
    void Display();
    void Reset(Graphic*);
    void Set(Graphic*);
private:
    PSColor* _black;
    PSColor* _red;
    static VWork* _vwork;
};

#endif
