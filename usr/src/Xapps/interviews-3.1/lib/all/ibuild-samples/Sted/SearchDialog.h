#ifndef SearchDialog_h
#define SearchDialog_h

#include "SearchDialog-core.h"

class SearchDialog : public SearchDialog_core {
public:
    SearchDialog(const char*);
    StringEditor* GetStringEditor() { return searchSE; }
};

#endif
