#ifndef VMouse_h
#define VMouse_h

#include "VMouse-core.h"

class Event;
class VMouseWriter;

class VMouse : public VMouse_core {
public:
    VMouse(const char*, const char*);
    virtual ~VMouse();
    virtual void Handle(Event&);
private:
    void Sim_Press(Graphic* orig, Graphic* porig, Event&);
private:
    VMouseWriter* _vwriter;
};

#endif
