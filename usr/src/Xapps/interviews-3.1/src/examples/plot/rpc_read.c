#include "rpc_read.h"
#include <Dispatch/rpchdr.h>
#include <Dispatch/rpcstream.h>
#include <Dispatch/rpcwriter.h>
//_______________________________________________________________________

Reader::Reader(int port, RpcService* service) : RpcReader(port, NFCNS, binary()),
    _service(service)
{
    _function[STRING] = &Reader::receive_STRING;
    _function[INT] = &Reader::receive_INT;
}

Reader::~Reader() {}

void Reader::connectionClosed(int) {
    _service->quitRunning();
}

void Callback(char* string);  // The user must supply this routine.

void Reader::receive_STRING(RpcReader*, RpcHdr&, rpcstream& rs) {
    char string[80];
    rs >> string;
    Callback(string);
}

void Reader::receive_INT(RpcReader*, RpcHdr&, rpcstream& rs) {
    int i;
    rs >> i;
	cerr << "receive_INT: " << i << "\n";
}

//_______________________________________________________________________

Service::Service(int port) : RpcService(port),
    _reader(nil) {}

Service::~Service() {
    delete _reader;
}

void Service::createReader(int port) {
    _reader = new Reader(port, this);
}

