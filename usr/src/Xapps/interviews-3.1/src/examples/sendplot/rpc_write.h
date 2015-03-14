#ifndef __rpc_write_h
#define __rpc_write_h

#include <Dispatch/rpcwriter.h>

class Writer : public RpcWriter {
public:
    enum { STRING, INT, NFCNS };
    Writer(int port);
    void send(char* string);
    void send(int i);
    void flush();
protected:
    boolean fatal() { return false; }
    boolean binary() { return false; }
};

#endif
