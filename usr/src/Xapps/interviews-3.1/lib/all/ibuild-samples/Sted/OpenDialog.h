#ifndef OpenDialog_h
#define OpenDialog_h

#include "OpenDialog-core.h"

class OpenDialog : public OpenDialog_core {
public:
    OpenDialog(const char*);
    FileBrowser* GetFileBrowser() { return openFB; }
};

#endif
