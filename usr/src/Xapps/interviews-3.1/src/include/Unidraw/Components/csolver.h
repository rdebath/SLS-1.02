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
 * CSolver - connector constraint solver.
 */

#ifndef unidraw_csolver_h
#define unidraw_csolver_h

#include <Unidraw/globals.h>

class CCnxn;
class CCnxn_HashTable;
class CGlue;
class CNet;
class ConnInfo;
class Connector;
class CSolverState;

class istream;
class ostream;

class CSolver {
public:
    CSolver();
    ~CSolver();

    void Connect(Connector*, Connector*, CGlue* = nil);
    void Disconnect(Connector*, Connector*);
    void Disconnect(Connector*);
    void Solve();
    void Print();                               // for debugging only

    CSolverState* GetState(Connector*);
    void SetState(CSolverState*);
    
    void Read(istream&);
    void Write(ostream&);
private:
    friend class Connector;
    void Wrote(Connector*);

    CNet* Network(UList*);
    ConnInfo* Info(Connector*, Orientation);
    void SolveAll(UList*, Orientation);
    void Solve(CNet*, Orientation);
    void DestroyCnxns();
    void DestroyCnxns(CNet*);

    void GetState(Connector*, ConnInfo*, Orientation, CSolverState*);
    void Update();
    void UpdateInfo(CCnxn*, Orientation);
    void UpdateInfo(CCnxn*, ConnInfo*, ConnInfo*, UList*);

    void CreateNetwork(CCnxn*, ConnInfo*, ConnInfo*, UList*);
    void MergeNetworks(CNet*, CNet*, UList*);
    void DeleteCnxnsTo(Connector*, CNet*, UList*);
    void DeleteCnxnsBetween(Connector*, Connector*, CNet*);
    void DeletePeerInfo(Connector*, ConnInfo*);
    void DeletePeerInfo(Connector*, ConnInfo*, Connector*);
    void InitInfo(Connector*);

    void ReadConnectors(istream&, CNet*);
    void InitConnectors(CNet*, CNet*);
    void WriteConnectors(ostream&, CCnxn_HashTable*);
    void Wrote(Connector*, CNet* cl, CCnxn_HashTable* written);

    boolean FoundFixed(CNet*, CNet*&);
    boolean FoundSeries(CNet*, CNet*&, CNet*&, Orientation);
    boolean FoundParallel(CNet*, CNet*&, CNet*&, Orientation);
    boolean FoundY(CNet*, CNet*&, CNet*&, CNet*&, Orientation);
    boolean Found2Fixed(CNet* net, Connector*& c1, Connector*& c2);

    void SubstFixedEquiv(CNet*, CNet*, CNet*&, Orientation);
    void SubstSeriesEquiv(
        CNet*, CNet*, CNet*, CNet*&, boolean&, boolean&, Orientation
    );
    void SubstParallelEquiv(CNet*, CNet*, CNet*, CNet*&, boolean&,Orientation);
    void SubstYEquiv(
        CNet*, CNet*, CNet*, CNet*, CNet*&, CNet*&, CNet*&,
        boolean&, boolean&, boolean&, Orientation
    );
    void SubstPseudoFixed(CNet*, Connector*, Connector*, CNet*&, Orientation);

    void ReplaceFixed(CNet*, CNet*, CNet*&, Orientation);
    void ReplaceSeries(
        CNet*, CNet*, CNet*, CNet*&, boolean, boolean, Orientation
    );
    void ReplaceParallel(CNet*, CNet*, CNet*, CNet*&, boolean, Orientation);
    void ReplaceY(
        CNet*, CNet*, CNet*, CNet*, CNet*&, CNet*&, CNet*&,
        boolean, boolean, boolean, Orientation
    );
    void ReplacePseudoFixed(CNet*, Connector*, Connector*, CNet*&,Orientation);

    void SubstFixedInfo(CNet*, Orientation);
    void SubstSeriesInfo(CNet*, CNet*, Orientation);
    void SubstParallelInfo(CNet*, Orientation);
    void SubstYInfo(CNet*, CNet*, CNet*, Orientation);
    void SubstPseudoFixedInfo(CNet*, Orientation);

    void ReplaceFixedInfo(CNet*, Orientation);
    void ReplaceSeriesInfo(CNet*, CNet*, Orientation);
    void ReplaceParallelInfo(CNet*, Orientation);
    void ReplaceYInfo(CNet*, CNet*, CNet*, Orientation);
    void ReplacePseudoFixedInfo(CNet*, Orientation);

    void DefaultPosition(CNet*);
private:
    UList* _hnets, *_vnets;
    CCnxn_HashTable* _hwritten, *_vwritten;
};

class CSolverState {
public:
    ~CSolverState();
private:
    friend class CSolver;
    CSolverState(CCnxn* = nil, Orientation = Horizontal);

    CSolverState* First();
    CSolverState* Next();
    void Append(CSolverState*);
private:
    CCnxn* _cnxn;
    Orientation _orient;
    CSolverState* _next;
};

#endif
