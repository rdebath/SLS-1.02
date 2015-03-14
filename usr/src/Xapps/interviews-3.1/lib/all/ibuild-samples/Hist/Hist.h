#ifndef Hist_h
#define Hist_h

#include "Hist-core.h"

class Hist : public Hist_core {
public:
    Hist(const char*, float max_value = 10.);
    virtual ~Hist();

    void Include(const char* label, float value);
private:
    void RemoveDummies();
    void InitValueAxis();
private:
    int _count;
    float _max_value;
};

#endif
