#ifndef Dial_h
#define Dial_h

#include "Dial-core.h"

class Damage;
class Graphic;

class Dial : public Dial_core {
public:
    Dial(const char*, Graphic*);
    virtual ~Dial();

    void Damaged(Graphic*);
    void Repair();
protected:
    virtual void Resize();
private:
    Damage* _damage;
};

#endif
