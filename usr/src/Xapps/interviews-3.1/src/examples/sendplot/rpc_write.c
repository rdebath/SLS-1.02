#include "rpc_write.h"
#include <Dispatch/rpchdr.h>
#include <Dispatch/rpcservice.h>
#include <Dispatch/rpcstream.h>

//_______________________________________________________________________

Writer::Writer (int port) : RpcWriter("localhost", port, fatal(), binary()) {}

void Writer::flush() {
    server().flush();
}

void Writer::send(char* string) {
    RpcHdr header(this, STRING);
    server() << header << string;
}

void Writer::send(int i) {
    RpcHdr header(this, INT);
    server() << header << i;
}

