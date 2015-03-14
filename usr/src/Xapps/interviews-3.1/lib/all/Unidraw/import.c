/*
 * Copyright (c) 1991 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * ImportCmd implementation.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/import.h>

#include <Unidraw/Components/rastercomp.h>
#include <Unidraw/Components/stencilcomp.h>

#include <Unidraw/Graphic/rasterrect.h>
#include <Unidraw/Graphic/ustencil.h>

#include <IV-look/dialogs.h>
#include <InterViews/bitmap.h>
#include <InterViews/raster.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/tiff.h>
#include <InterViews/window.h>

#include <TIFF/format.h>
#include <OS/string.h>

#include <stdio.h>
#include <string.h>

/*****************************************************************************/

static int hexmap[] = {
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  0,  0,  0,  0,  0,
     0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
};

/*****************************************************************************/

static FILE* CheckCompression(
	FILE* file, const char *filename, boolean& compressed
) {
    char cmd[256];
    
    if (!file || !fgets (cmd, 4, file)) {
        compressed = false;

    } else if (*((unsigned short *)cmd) == COMPRESS_MAGIC_NUM) {
        fclose (file);
        sprintf (cmd, "uncompress < %s", filename);
        file = popen (cmd, "r");

        if (!file) {
            return NULL;
        }
        compressed = true;

    } else {
        rewind (file);
        compressed = false;
    }

    return file;
}

static const char* ReadCreator (const char* filename) {
    char* buf = nil;
    FILE* file = fopen(filename, "r");
    
    if (file != nil) {
        boolean compressed;
        static char creator[CHARBUFSIZE];
        char line[CHARBUFSIZE];
        
        file = CheckCompression(file, filename, compressed);

        if (!file) {
            return NULL;
        }

        if (fgets(line, CHARBUFSIZE, file) != NULL) {

            /* Two-byte magic numbers */

            switch(*((unsigned short *)line)) {
            case TIFF1_MAGIC_NUM:
            case TIFF2_MAGIC_NUM:
                return ("TIFF");
            case SUN_MAGIC_NUM:
                return ("SUN");
            }

            /* One-byte Magic numbers */

            switch (line[0]) {
            case BM_MAGIC_NUM:
                return ("BM");
            case PBM_MAGIC_NUM:
                return ("PBM");
            case ATK_MAGIC_NUM:
                return ("ATK");
            case MP_MAGIC_NUM:
                return ("MP");
            case X11_MAGIC_NUM:
                return ("X11");
            case PCX_MAGIC_NUM:
                return ("PCX");
            case IFF_MAGIC_NUM:
                return ("IFF");
            case GIF_MAGIC_NUM:
                return ("GIF");
            case RLE_MAGIC_NUM:
                return ("RLE");
            }

        } else {
            return NULL;
        }

        do {
            if (sscanf(line, "%%%%Creator: %s", creator)) {
                buf = creator;
                break;
                
            } else if (strcmp(line, "%%EndComments\n") == 0) {
                break;
            }
        } while (fgets(line, CHARBUFSIZE, file) != NULL);

        if (compressed) {
            pclose(file);

        } else {
            fclose(file);
        }
    }
    return buf;
}

static int gethex (FILE* file) {
    int c;
    while ((c = getc(file)) == ' ' || c == '\n') { }
    return (hexmap[c] << 4) + hexmap[getc(file)];
}

/*****************************************************************************/

ClassId ImportCmd::GetClassId () { return IMPORT_CMD; }

boolean ImportCmd::IsA (ClassId id) {
    return IMPORT_CMD == id || Command::IsA(id);
}

ImportCmd::ImportCmd (ControlInfo* c, FileChooser* f) : Command(c) { Init(f); }
ImportCmd::ImportCmd (Editor* ed, FileChooser* f) : Command(ed) { Init(f); }
ImportCmd::~ImportCmd () { Resource::unref(chooser_); }
void ImportCmd::Init (FileChooser* f) {
    chooser_ = f;
    Resource::ref(chooser_);
}

Command* ImportCmd::Copy () {
    ImportCmd* copy = new ImportCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void ImportCmd::Execute () {
    GraphicComp* comp = PostDialog();

    if (comp != nil) {
	PasteCmd* paste_cmd = new PasteCmd(GetEditor(), new Clipboard(comp));
	paste_cmd->Execute();
	paste_cmd->Log();
	GetEditor()->GetViewer()->Align(comp, /* Center */ 4);
    }
}

boolean ImportCmd::Reversible () { return false; }

GraphicComp* ImportCmd::PostDialog () {
    boolean imported = false;
    GraphicComp* comp = nil;
    Editor* ed = GetEditor();

    Style* style;
    boolean reset_caption = false;
    if (chooser_ == nil) {
	style = new Style(Session::instance()->style());
	style->attribute("subcaption", "Import graphic from file:");
	style->attribute("open", "Import");
	chooser_ = DialogKit::instance()->file_chooser(".", style);
	Resource::ref(chooser_);
    } else {
	style = chooser_->style();
    }
    while (chooser_->post_for(ed->GetWindow())) {
	const String* str = chooser_->selected();
	if (str != nil) {
	    NullTerminatedString ns(*str);
	    comp = Import(ns.string());
	    if (comp != nil) {
		break;
	    }
	    style->attribute("caption", "Import failed!");
	    reset_caption = true;
	}
    }
    if (reset_caption) {
	style->attribute("caption", "");
    }
    return comp;
}

GraphicComp* ImportCmd::Import (const char* filename) {
    GraphicComp* comp = nil;
    const char* creator = ReadCreator(filename);

    if (creator == nil || strcmp(creator, "idraw") == 0) {
        Catalog* catalog = unidraw->GetCatalog();

        if (catalog->Valid(filename, (Component*&) comp)) {
            comp = (GraphicComp*) comp->Copy();

        } else if (catalog->Retrieve(filename, (Component*&) comp)) {
            catalog->Forget(comp);
        }

    } else {
        if (strcmp(creator, "X11") == 0) {
            comp = XBitmap_Image(filename);

        } else if (strcmp(creator, "TIFF") == 0) {
            comp = TIFF_Image(filename);

        } else if (strcmp(creator, "pgmtops") == 0) {
            comp = PGM_Image(filename);

        } else if (strcmp(creator, "ppmtops") == 0) {
            comp = PPM_Image(filename);
        }
    }
    return comp;
}

GraphicComp* ImportCmd::TIFF_Image (const char* filename) {
    GraphicComp* comp = nil;
    Raster* raster = TIFFRaster::load(filename);

    if (raster != nil) {
        raster->ref();
        raster->flush();
        comp = new RasterComp(new RasterRect(raster), filename);
    }

    return comp;
}

GraphicComp* ImportCmd::PGM_Image (const char* filename) {
    GraphicComp* comp = nil;
    FILE* file = fopen(filename, "r");

    if (file != nil) {
        char line[1000];
        do {
            fgets(line, 1000, file);
        } while (strcmp(line, "gsave\n") != 0);

        fgets(line, 1000, file);                    // translate
        fgets(line, 1000, file);                    // scale
        fgets(line, 1000, file);                    // sizes
        int w, h, d;
        sscanf(line, "%d %d %d", &w, &h, &d);
        fgets(line, 1000, file);                    // [ ... ]
        fgets(line, 1000, file);                    // { ... }
        fgets(line, 1000, file);                    // image

        Raster* raster = new Raster(w, h);

        for (int row = h - 1; row >= 0; --row) {
            for (int column = 0; column < w; ++column) {
                int byte = gethex(file);
                float g = float(byte) / 0xff;
                raster->poke(column, row, g, g, g, 1.0);
            }
        }
        raster->flush();
        comp = new RasterComp(new RasterRect(raster), filename);
    }
    fclose(file);
    return comp;
}

GraphicComp* ImportCmd::PPM_Image (const char* filename) {
    GraphicComp* comp = nil;
    FILE* file = fopen(filename, "r");
    boolean compressed;
    file = CheckCompression(file, filename, compressed);

    if (file != nil) {
        char line[1000];
        do {
            fgets(line, 1000, file);
        } while (strcmp(line, "gsave\n") != 0);

        fgets(line, 1000, file);                    // translate
        fgets(line, 1000, file);                    // scale
        fgets(line, 1000, file);                    // scale
        fgets(line, 1000, file);                    // sizes
        int w, h, d;
        sscanf(line, "%d %d %d", &w, &h, &d);
        fgets(line, 1000, file);                    // [ ... ]
        fgets(line, 1000, file);                    // { ... }
        fgets(line, 1000, file);                    // false 3
        fgets(line, 1000, file);                    // colorimage

        Raster* raster = new Raster(w, h);

        for (int row = h - 1; row >= 0; --row) {
            for (int column = 0; column < w; ++column) {
                int red = gethex(file);
                int green = gethex(file);
                int blue = gethex(file);
                raster->poke(
                    column, row,
                    float(red)/0xff, float(green)/0xff, float(blue)/0xff, 1.0
                );
            }
        }
        raster->flush();
        comp = new RasterComp(new RasterRect(raster), filename);
    }
    
    if (compressed) {
        pclose(file);

    } else {
        fclose(file);
    }
    return comp;
}

GraphicComp* ImportCmd::XBitmap_Image (const char* filename) {
    GraphicComp* comp = nil;
    FILE* file = fopen(filename, "r");

    if (file != nil) {
        Bitmap* bm = Bitmap::open(filename);

        if (bm != nil) {
            comp = new StencilComp(
	   	new UStencil(bm, bm, stdgraphic), filename
	    );
        }
    }
    fclose(file);
    return comp;
}
