#ifndef __rpc_read_h
#define __rpc_read_h

#include <Dispatch/rpcreader.h>
#include <Dispatch/rpcservice.h>

class Reader : public RpcReader {
public:
    Reader(int port, RpcService* service);
    virtual ~Reader();
protected:
    virtual void connectionClosed(int port);
    RpcService* _service;
    enum { STRING, INT, NFCNS };
    static void receive_STRING(RpcReader*, RpcHdr&, rpcstream&);
    static void receive_INT(RpcReader*, RpcHdr&, rpcstream&);
    boolean binary() { return false; }
};

class Service : public RpcService {
public:
    Service(int port);
    virtual ~Service();
protected:
    virtual void createReader(int port);
protected:
    Reader* _reader;
};

#endif
