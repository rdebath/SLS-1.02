#ifndef idraw_h
#define idraw_h

#include <InterViews/enter-scope.h>
#include <OS/enter-scope.h>

class InputFile;
class GraphicMaster;

class IdrawReader {
public:
    static GraphicMaster* load(InputFile*);
    static GraphicMaster* load(const char*);
};

#endif
