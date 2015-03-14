#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/character.h>
#include <InterViews/composition.h>
#include <InterViews/label.h>
#include <InterViews/layout.h>
#include <InterViews/texcomp.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <OS/file.h>
#include <OS/math.h>
#include <OS/string.h>
#include <stdio.h>
#include <stdlib.h>

class DocumentView : public MonoGlyph {
public:
    DocumentView(InputFile*, WidgetKit&, const LayoutKit&);
    virtual ~DocumentView();

    virtual Adjustable* adjustable() const;
private:
    Composition* page_;
    ScrollBox* box_;
    Glyph* begin_par_;
    Glyph* end_par_;
    Glyph* begin_line_;
    Glyph* end_line_;
    Glyph* word_space_;
    Glyph* interline_;
    Glyph* vfil_glue_;

    void add(const String&, WidgetKit&, const LayoutKit&);
};

static OptionDesc options[] = {
    { "-justify", "*alignment", OptionValueImplicit, "justify" },
    { "-center", "*alignment", OptionValueImplicit, "center" },
    { "-left", "*alignment", OptionValueImplicit, "left" },
    { "-right", "*alignment", OptionValueImplicit, "right" },
    { nil }
};

static PropertyData props[] = {
    { "*alignment", "justify" },
    { nil }
};

int main(int argc, char** argv) {
    Session* session = new Session("Text", argc, argv, options, props);
    if (argc != 2) {
	fprintf(stderr, "Usage: %s file\n", argv[0]);
	exit(1);
    }
    WidgetKit& kit = *WidgetKit::instance();
    const LayoutKit& layout = *LayoutKit::instance();
    InputFile* file = InputFile::open(argv[1]);
    if (file == nil) {
	fprintf(stderr, "can't open %s\n", argv[1]);
	exit(1);
    }
    DocumentView* view = new DocumentView(file, kit, layout);
    return session->run_window(
	new ApplicationWindow(
	    layout.hbox(
		kit.inset_frame(
		    kit.vscroll_bar(view->adjustable())
		),
		new Background(
		    layout.variable_span(
			layout.natural_span(
			    layout.vcenter(view, 1.0), 4*72.0, 6*72.0
			)
		    ),
		    kit.background()
		)
	    )
	)
    );
}

DocumentView::DocumentView(
    InputFile* file, WidgetKit& kit, const LayoutKit& layout
) {
    const Font* f = kit.font();
    const Color* fg = kit.foreground();
    word_space_ = layout.spaces(2, 0.5, f, fg);
    interline_ = layout.vglue();
    vfil_glue_ = layout.vglue();

    String v("justify");
    kit.style()->find_attribute("alignment", v);
    if (v == "left") {
	begin_line_ = layout.vstrut(0);
	end_line_ = layout.strut(f, 0, fil, 0);
	begin_par_ = layout.vstrut(0);
	end_par_ = layout.strut(f, 0, fil, 0);
    } else if (v == "right") {
	begin_line_ = layout.vstrut(0, 0, 0, fil, 0);
	end_line_ = layout.strut(f);
	begin_par_ = layout.vstrut(0, 0, 0, fil, 0);
	end_par_ = layout.strut(f);
    } else if (v == "center") {
	begin_line_ = layout.vstrut(0, 0, 0, fil, 0);
	end_line_ = layout.strut(f, 0, fil, 0);
	begin_par_ = layout.vstrut(0, 0, 0, fil, 0);
	end_par_ = layout.strut(f, 0, fil, 0);
    } else if (v == "justify") {
	begin_line_ = layout.vstrut(0);
	end_line_ = layout.strut(f);
	begin_par_ = layout.vstrut(0);
	end_par_ = layout.strut(f, 0, fil, 0);
    } else {
	fprintf(stderr, "Unknown alignment: %.*s\n", v.length(), v.string());
	exit(1);
    }

    Resource::ref(begin_par_);
    Resource::ref(end_par_);
    Resource::ref(begin_line_);
    Resource::ref(end_line_);
    Resource::ref(word_space_);
    Resource::ref(interline_);
    Resource::ref(vfil_glue_);

    box_ = layout.vscrollbox();
    box_->small_scroll(Dimension_Y, 2);
    page_ = new LRComposition(
	box_, new TeXCompositor(10), nil, 6*72.0, fil, fil, file->length()
    );
    const char* data;
    for (;;) {
	int len = file->read(data);
	if (len <= 0) {
	    break;
	}
	add(String(data, len), kit, layout);
    }
    page_->append(vfil_glue_);
    page_->repair();
    body(page_);
}

DocumentView::~DocumentView() {
    Resource::unref(begin_par_);
    Resource::unref(end_par_);
    Resource::unref(begin_line_);
    Resource::unref(end_line_);
    Resource::unref(word_space_);
    Resource::unref(interline_);
    Resource::unref(vfil_glue_);
}

void DocumentView::add(
    const String& data, WidgetKit& kit, const LayoutKit& layout
) {
    const char* p = data.string();
    const char* end = p + data.length();
    const Font* f = kit.font();
    const Color* fg = kit.foreground();
    Glyph* g[256];
    for (int i = 0; i < 256; i++) {
	g[i] = new Character(i, f, fg);
    }

    Resource::unref(g['\n']);
    g['\n'] = layout.discretionary(
	PenaltyGood,
	end_par_,
	end_par_,
	layout.discretionary(0, interline_, vfil_glue_, nil, nil),
	begin_par_
    );

    Resource::unref(g[' ']);
    g[' '] = layout.discretionary(
	0,
	word_space_,
	end_line_,
	layout.discretionary(0, interline_, vfil_glue_, nil, nil),
	begin_line_
    );

    Resource::unref(g['\t']);
    // g['\t'] = layout.shape_of(g['M']);
    g['\t'] = new Label("        ", f, fg);

    page_->append(begin_par_);
    for (; p < end; p++) {
	page_->append(g[*((unsigned char*)p)]);
    }
}

Adjustable* DocumentView::adjustable() const {
    return box_;
}
