// -*- c++ -*-
/* 
Copyright (C) 1991 Peter Bersen

This file is part of the rpc++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

Modified and partially rewritten March 1992 by Michael N. Lipp,
mnl@dtro.e-technik.th-darmstadt.de. The original copyright terms and
conditions apply without change to any modified or new parts.
*/

static char _rpcpp_stub_cc_[]
= "stub.cc,v 2.5 1992/12/06 18:57:55 mnl Exp";

// stub.cc,v
// Revision 2.5  1992/12/06  18:57:55  mnl
// Fixed various bugs and added some Methods.
//
// Revision 2.4  1992/08/08  13:23:13  mnl
// Space allocated for result turned out to be local to a CLIENT*, thus
// "res", "resmax" and "resproc" have been made member variables of
// RpcStub.
//
// Revision 2.3  1992/06/15  19:12:46  mnl
// Fixed a few bugs, clarified interface.
//
// Revision 2.2  1992/06/13  14:27:04  mnl
// Adapted to (patched) gcc-2.2. Fixed several bugs.
//
// Revision 2.1.1.1  1992/03/08  13:28:42  mnl
// Initial mnl version.
//

#ifdef __GNUG__
#pragma implementation
#endif

#include <stream.h>
#include <memory.h>
#include <assert.h>
#include "rpc++/stub.h"

timeval RpcStub::defaultTimeout = { 25, 0 };

void RpcStub::init (u_long prog, u_long vers,
	       char* srv, timeval timo, bool connect)
{
  errorState = noError;
  program = prog;
  version = vers;
  server = srv;
  timeout = timo;
  svc = 0;
  res = 0;
  resmax = 0;
  resproc = 0;
  if (connect)
    Reconnect ();
  else
    errorState = notConnected;
}

RpcStub::~RpcStub ()
{
  if (resproc) // "Call" has been called at least once,
    clnt_freeres (svc, resproc, res); // free any data allocated by clnt_call
  if (svc)
    clnt_destroy (svc);
}

void* RpcStub::HandleError ()
{
  switch (errorState)
    {
    case notConnected:
      cerr << "rpc++: Stub has not been connected to server.\n";
    case cantCreate:
      cerr << clnt_spcreateerror ("rpc++") << '\n';
      break;
    case cantCall:
      cerr << clnt_sperror (svc, "rpc++") << '\n';
      exit (1);
    }
  return 0; // suppress compiler warning
}

void RpcStub::Reconnect (bool handle_errors)
{
  if (resproc) // "Call" has been called
    {
      clnt_freeres (svc, resproc, res); // free data allocated by clnt_call
      resproc = 0;
    }
  if (svc)
    {
      clnt_destroy (svc);
    }
  svc = clnt_create (server, program, version, "tcp"); // connect to client
  if (svc == 0) // failed ?
    {
      if (handle_errors)
	HandleError (cantCreate);
      errorState = notConnected;
      return;
    }
  clnt_control (svc, CLSET_TIMEOUT, &timeout);
  errorState = noError;
}

void* RpcStub::Call (RpcRequest& req, void* in, bool handle_errors)
{
  void* args[] = { in };
  return DoCall (req, args, handle_errors);
}

void* RpcStub::Call (RpcRequest& req, void* in0, void* in1, bool handle_errors)
{
  void* args[] = { in0, in1 };
  return DoCall (req, args, handle_errors);
}

void* RpcStub::Call (RpcRequest& req, void* in0, void* in1, void* in2,
		     bool handle_errors)
{
  void* args[] = { in0, in1, in2 };
  return DoCall (req, args, handle_errors);
}

void* RpcStub::Call (RpcRequest& req, void* in0, void* in1, void* in2,
		     void* in3, bool handle_errors)
{
  void* args[] = { in0, in1, in2, in3 };
  return DoCall (req, args, handle_errors);
}

void* RpcStub::Call (RpcRequest& req, void* in0, void* in1, void* in2,
		     void* in3, void* in4, bool handle_errors)
{
  void* args[] = { in0, in1, in2, in3, in4 };
  return DoCall (req, args, handle_errors);
}

void* RpcStub::Call (RpcRequest& req, void* in0, void* in1, void* in2,
		     void* in3, void* in4, void* in5, bool handle_errors)
{
  void* args[] = { in0, in1, in2, in3, in4, in5 };
  return DoCall (req, args, handle_errors);
}

void* RpcStub::Call (RpcRequest& req, void* in0, void* in1, void* in2,
		     void* in3, void* in4, void* in5, void* in6,
		     bool handle_errors)
{
  void* args[] = { in0, in1, in2, in3, in4, in5, in6 };
  return DoCall (req, args, handle_errors);
}

void* RpcStub::Call (RpcRequest& req, void** ins, bool handle_errors)
{
  return DoCall (req, ins, handle_errors);
}

void* RpcStub::DoCall (RpcRequest& req, void** args, bool handle_errors)
{
  if (! OK () )
    {
      if (! handle_errors)
	return 0;
      return HandleError ();
    }
  if (resproc) // "Call" has been called previously,
    clnt_freeres (svc, resproc, res); // free any data allocated by clnt_call
  resproc = 0;
  if (req.OutInfo()->Size () > resmax) // enough space for result?
    {
      delete res; // delete old result buffer
      res = new char[resmax = req.OutInfo()->Size ()]; // get a new one
    }
  if (req.OutInfo()->Size () > 0 ) // preset result (everyone does it, why?)
    memset (res, 0, req.OutInfo()->Size ());

  curReq = &req;
  curArgs = args;
  return ReCall (handle_errors);
}

void* RpcStub::ReCall (bool handle_errors)
{
  static const timeval nullTimeout = { 0, 0 };

  if (resproc) // "ReCall" has been called before.
    clnt_freeres (svc, resproc, res); // free any data allocated by clnt_call
  resproc = curReq->OutInfo()->Proc (); // note current output deserializer
  XdrSeqInfo xsi = { curReq->InInfo (), curArgs };
  if (curReq->Type () == RpcRequest::normal)
    {
      if (clnt_call (svc, curReq->RequestNumber (), // do call
		     Xdr::XdrParams, &xsi,
		     curReq->OutInfo()->Proc (), res,
		     timeout) != RPC_SUCCESS)
	{
	  if (! handle_errors)
	    return 0;
	  return HandleError (cantCall);
	}
      return res ? res : &nullTimeout; // if the return type is void,
      // res may be 0 although the call succeeded. To simplify error
      // checking if handle_errors is FALSE, we return a value != 0.
    }

  // curReq->Type () is batched or async
  enum clnt_stat callres;
  callres = clnt_call (svc, curReq->RequestNumber (), // do call
		       Xdr::XdrParams, &xsi,
		       (curReq->Type () == RpcRequest::batched
		        ? (xdrproc_t)0 : xdr_void), res,
		       nullTimeout);
  if (callres != RPC_SUCCESS && callres != RPC_TIMEDOUT)
    {
      if (! handle_errors)
	return 0;
      return HandleError (cantCall);
    }
  return res ? res : &nullTimeout;
}
