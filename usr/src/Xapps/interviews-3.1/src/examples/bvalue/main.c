#include <IV-look/kit.h>
#include <IV-look/telltale.h>
#include <InterViews/adjust.h>
#include <InterViews/background.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <OS/string.h>
#include <stdio.h>

class App : public Observer {
public:
    App(Adjustable*, TelltaleState*);
    virtual ~App();

    void print_value();

    virtual void update(Observable*);
    virtual void disconnect(Observable*);
private:
    Adjustable* adjustable_;
    TelltaleState* continuous_;
};

App::App(Adjustable* a, TelltaleState* s) {
    adjustable_ = a;
    a->attach(Dimension_X, this);
    continuous_ = s;
}

App::~App() {
    if (adjustable_ != nil) {
	adjustable_->detach(Dimension_X, this);
    }
}

void App::print_value() {
    printf("%.5f\n", adjustable_->cur_lower(Dimension_X));
}

void App::update(Observable*) {
    if (continuous_->test(TelltaleState::is_chosen)) {
	print_value();
    }
}

void App::disconnect(Observable*) {
    adjustable_ = nil;
}

declareActionCallback(App);
implementActionCallback(App);

class BoundedValue : public Adjustable {
protected:
    BoundedValue();
public:
    BoundedValue(Coord lower, Coord upper);
    virtual ~BoundedValue();

    virtual void lower_bound(Coord);
    virtual void upper_bound(Coord);
    virtual void current_value(Coord);
    virtual void scroll_incr(Coord);
    virtual void page_incr(Coord);

    virtual Coord lower(DimensionName) const;
    virtual Coord upper(DimensionName) const;
    virtual Coord length(DimensionName) const;
    virtual Coord cur_lower(DimensionName) const;
    virtual Coord cur_upper(DimensionName) const;
    virtual Coord cur_length(DimensionName) const;

    virtual void scroll_to(DimensionName, Coord position);
    virtual void scroll_forward(DimensionName);
    virtual void scroll_backward(DimensionName);
    virtual void page_forward(DimensionName);
    virtual void page_backward(DimensionName);
private:
    Coord curvalue_;
    Coord lower_;
    Coord span_;
    Coord scroll_incr_;
    Coord page_incr_;
};

BoundedValue::BoundedValue() {
    scroll_incr_ = 0.0;
    page_incr_ = 0.0;
}

BoundedValue::BoundedValue(Coord lower, Coord upper) {
    lower_ = lower;
    span_ = upper - lower;
    scroll_incr_ = span_ * 0.04;
    page_incr_ = span_ * 0.4;
    curvalue_ = (lower + upper) * 0.5;
}

BoundedValue::~BoundedValue() { }

void BoundedValue::lower_bound(Coord c) { lower_ = c; }
void BoundedValue::upper_bound(Coord c) { span_ = c - lower_; }

void BoundedValue::current_value(Coord value) {
    curvalue_ = value;
    constrain(Dimension_X, curvalue_);
    notify(Dimension_X);
    notify(Dimension_Y);
}

void BoundedValue::scroll_incr(Coord c) { scroll_incr_ = c; }
void BoundedValue::page_incr(Coord c) { page_incr_ = c; }

#define access_function(name,value) \
Coord BoundedValue::name(DimensionName) const { \
    return value; \
}

access_function(lower,lower_)
access_function(upper,lower_ + span_)
access_function(length,span_)
access_function(cur_lower,curvalue_)
access_function(cur_upper,curvalue_)
access_function(cur_length,0)

void BoundedValue::scroll_to(DimensionName d, Coord position) {
    Coord p = position;
    constrain(d, p);
    if (p != curvalue_) {
	curvalue_ = p;
	notify(Dimension_X);
	notify(Dimension_Y);
    }
}

#define scroll_function(name,expr) \
void BoundedValue::name(DimensionName d) { \
    scroll_to(d, curvalue_ + expr); \
}

scroll_function(scroll_forward,+scroll_incr_)
scroll_function(scroll_backward,-scroll_incr_)
scroll_function(page_forward,+page_incr_)
scroll_function(page_backward,-page_incr_)

int main(int argc, char** argv) {
    Session* session = new Session("Himom", argc, argv);
    WidgetKit& kit = *WidgetKit::instance();
    const LayoutKit& layout = *LayoutKit::instance();
    Button* continuous_button = kit.check_box("Continuous", nil);
    BoundedValue* b = new BoundedValue(0.0, 100.0);
    App* a = new App(b, continuous_button->state());
    b->current_value(50.0);
    b->scroll_incr(5.0);
    b->page_incr(20.0);
    return session->run_window(
	new ApplicationWindow(
	    kit.inset_frame(
		layout.hbox(
		    layout.margin(
			layout.vbox(
			    layout.hcenter(
				kit.push_button(
				    "Print value",
				    new ActionCallback(App)(
					a, &App::print_value
				    )
				)
			    ),
			    layout.vglue(10.0),
			    layout.hcenter(continuous_button)
			),
			20.0
		    ),
		    kit.inset_frame(kit.vscroll_bar(b))
		)
	    )
	)
    );
}
