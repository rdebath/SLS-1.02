/*
 * morphing primitives
 */

#ifndef morpher_h
#define morpher_h

class Canvas;
class Graphic;
class GlyphEditor;
class MorpherImpl;

class Morpher {
public:
    enum {fr, rp, rps, r, p, cp, fp, fps, ff};

    Morpher(long size, long steps, GlyphEditor* ged);
    virtual ~Morpher();

    virtual void init();
    virtual void new_session();
    virtual void execute(unsigned int);
    virtual long count();
    virtual Graphic* item(long index) const;
    virtual void prepend(Graphic*);
    virtual void append(Graphic*);
    virtual void insert(long, Graphic*);
    virtual void remove(long);
    virtual void remove_all();
protected:
    virtual void tick(long, long);
    virtual void rewind();
protected:
    MorpherImpl* _m_impl;
};

#endif
