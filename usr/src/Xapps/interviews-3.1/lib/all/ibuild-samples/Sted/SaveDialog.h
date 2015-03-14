#ifndef SaveDialog_h
#define SaveDialog_h

#include "SaveDialog-core.h"

class SaveDialog : public SaveDialog_core {
public:
    SaveDialog(const char*);
    StringEditor* GetStringEditor() { return saveSE; }
};

#endif
