#ifndef Sted_h
#define Sted_h

#include "Sted-core.h"

class Sted : public Sted_core {
public:
    Sted(const char*);

    virtual void New();
    virtual void Open();
    virtual void Save();
    virtual void Quit();
    virtual void Cut();
    virtual void Copy();
    virtual void Paste();
    virtual void Search();

    virtual void Handle(Event&);
private:
    void InsertChar(char);
    void InsertDialog(Interactor*);
    void RemoveDialog(Interactor*);
private:
    class FileManager* _mgr;
};

class FileManager {
public:
    FileManager();
    class TextBuffer* GetTextBuffer();

    void Open(const char* filename);
    void Save(const char* filename);

    void Copy(int beg, int end);
    const char* Clipboard();

    boolean FwdSearch(const char*, int& beg, int& end);
    boolean BwdSearch(const char*, int& beg, int& end);
private:
    TextBuffer* _text;
    char* _buffer;
    int _size;
    char* _clipboard;
};

inline TextBuffer* FileManager::GetTextBuffer () { return _text; }
inline const char* FileManager::Clipboard () { return _clipboard; }

#endif
