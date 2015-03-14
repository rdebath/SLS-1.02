#ifndef Clock_h
#define Clock_h

#include "Clock-core.h"

class Clock : public Clock_core {
public:
    Clock(const char*);

    void Run();
    virtual void Update();
protected:
    virtual void Resize();
private:
    void Time(int& h, int& m, int& s);
private:
    float _time;
};

#endif
