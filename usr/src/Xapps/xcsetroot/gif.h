#define GIF_ID "GIF87a"

#define ConvertToInt(X) ((X).lsb + ((X).msb << 8))
#define IMAGESEP 0x2c
#define INTERLACEMASK 0x40
#define COLORMAPMASK 0x80

#define Corrupt(f) ((f).corrupt)
#define CurrentByte(f) ((f)->byte_buffer[f->current_byte_offset & 3])
#define NextByte(f) ((f)->byte_buffer[f->current_byte_offset + 1 & 3])
#define NextNextByte(f) ((f)->byte_buffer[f->current_byte_offset + 2 & 3])

#define FixFilePosition(f, new_offset) \
  while((new_offset) > (f)->current_byte_offset) AdvanceByte(f);

#define AdvanceByte(f) \
{ \
    ++(f)->current_byte_offset; \
    if((f)->bytes_left-- == 0) \
      (f)->corrupt = ((f)->bytes_left = getc(f->file_ptr) - 1) < 0; \
    NextNextByte(f) = getc(f->file_ptr); \
}

typedef struct _twobyte {
  Byte lsb, msb;
} B16int;

typedef struct _RawGifHeader {
  char id[6];
  B16int width;
  B16int height;
  Byte colormap_info;
  Byte background_color;
  Byte null_byte;
} RawGifHeader;

typedef struct _GifHeader {
  char id[6];
  int width;
  int height;
  int colormap_info;
  int background_color;
} GifHeader;

typedef struct _RawColor {
  Byte Red, Green, Blue;
} RawColor;

typedef struct _RawImageSpec {
  Byte image_separator;
  B16int left_offset;
  B16int top_offset;
  B16int width;
  B16int height;
  Byte interlace;
  Byte data_size;
} RawImageSpec;

typedef struct _ImageSpec {
  int left_offset;
  int top_offset;
  int width;
  int height;
  int interlace;
  int data_size;
} ImageSpec;

typedef struct _ByteStream {
  FILE *file_ptr;
  Bool corrupt;
  long current_byte_offset;
  int byte_buffer[4];
  int bytes_left;
} ByteStream;
