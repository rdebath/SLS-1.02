#include <InterViews/event.h>

#include <Unidraw/Graphic/lines.h>
#include <Unidraw/Graphic/picture.h>
#include <Unidraw/Graphic/polygons.h>

#include <sys/time.h>

#include "Clock.h"
#include "Dial.h"
#include <IV-2_6/_enter.h>

Clock::Clock(const char* name) : Clock_core(name) {
    _time = 0;
}

void Clock::Run () {
    for (;;) {
        Event e;
        Read(1, 0, e);

        if (e.eventType == KeyEvent && e.len == 1 && *e.keystring == 'q') {
            break;

        } else if (canvas != nil) {
            Update();
        }
    }
}
    
void Clock::Resize () {
    Clock_core::Resize();
    Update();
}

void Clock::Update () {
    int h, m, s;
    Time(h, m, s);

    float new_time = h%12 + m/60.0 + s/3600.0;
    float dt = _time - new_time;

    if (dt != 0) {
        float cx, cy;
        _clock->GetCenter(cx, cy);

        _dial->Damaged(_hour_hand);
        _dial->Damaged(_min_hand);
        _dial->Damaged(_sec_hand);

        _hour_hand->Rotate(dt * 30, cx, cy);
        _min_hand->Rotate(dt * 30 * 12, cx, cy);
        _sec_hand->Rotate(dt * 30 * 12 * 60, cx, cy);

        _dial->Damaged(_hour_hand);
        _dial->Damaged(_min_hand);
        _dial->Damaged(_sec_hand);

        _dial->Repair();
        _time = new_time;
    }
}

void Clock::Time (int& h, int& m, int& s) {
    struct timeval gmt;
    struct tm local;

    gettimeofday(&gmt, 0);
#ifdef hpux
    local = *localtime((time_t*) &gmt.tv_sec);
#else
    local = *localtime(&gmt.tv_sec);
#endif
    h = local.tm_hour;
    m = local.tm_min;
    s = local.tm_sec;
}
