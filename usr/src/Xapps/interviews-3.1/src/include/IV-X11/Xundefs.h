#undef Bitmap
#undef Colormap
#undef Cursor
#undef Display
#undef Drawable
#undef Font
#undef Screen
#undef Window

#define boolean _lib_os(boolean)
#define Bitmap _lib_iv(Bitmap)
#define Colormap _lib_iv(Colormap)
#define Cursor _lib_iv(Cursor)
#define Display _lib_iv(Display)
#define Drawable _lib_iv(Drawable)
#define Font _lib_iv(Font)
#define Screen _lib_iv(Screen)
#define Window _lib_iv(Window)

#ifdef iv__2_6_h
#include <IV-2_6/_names.h>
#endif
