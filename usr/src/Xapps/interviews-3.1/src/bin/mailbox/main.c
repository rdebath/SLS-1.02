/*
 * Copyright (c) 1987, 1988, 1989 Stanford University
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
 * Watch a mailbox and display its contents.
 */

/*
 * Modified by Scott Meyers (sdm) to allow for the specification of an
 * alias file that causes mailbox to map incoming email addresses to
 * user-defined strings.  For details on this file, see the comments in
 * the function MailBox::initializeAliases.  There is also a new
 * command-line option, "lookup," which, when specified, will cause mailbox
 * to use the system password file as an address->name mapping of last
 * resort.  Everything modifed by sdm for this enhancement is flagged by
 * a comment containing the string "sdm".
 */

#include <Dispatch/dispatcher.h>
#include <Dispatch/iocallback.h>
#include <InterViews/border.h>
#include <InterViews/box.h>
#include <InterViews/frame.h>
#include <InterViews/glue.h>
#include <InterViews/interactor.h>
#include <InterViews/paint.h>
#include <InterViews/painter.h>
#include <InterViews/sensor.h>
#include <InterViews/shape.h>
#include <InterViews/world.h>
#include <OS/types.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <pwd.h>

const int MAX_NAMES = 500;

char* lowercasifyString(char* string) {
    char* scan = string;
    while (*scan != '\0') {
	if (isupper(*scan)) {
	    *scan = tolower(*scan);
	}
	scan++;
    }
    return string;
}

World* world;
char mailpath[128];

#if defined(SYSV)
const char* mail_path = "/usr/mail/";
const char* mail_domain = ".UUCP";
#define lstat stat
#else
const char* mail_path = "/usr/spool/mail/";
const char* mail_domain = ".stanford";
#endif

class Viewer {
public:
    Viewer (Interactor *i) { view = i; next = nil; }
    Interactor* view;
    Viewer* next;
};

class MailBox;

class MailView : public Interactor {
public:
    MailView(MailBox*);
    ~MailView();

    void Update () { Draw(); }
protected:
    MailBox* mailbox;
};

class MailBeep : public MailView {
public:
    MailBeep(MailBox* m) : MailView(m) { lastcount = 0; }
    void Update();
private:
    int lastcount;
};

class MailText : public MailView {
public:
    MailText(MailBox* m, int r, int c);
    void Reconfig();
    void Redraw(Coord, Coord, Coord, Coord);
private:
    int rows, cols;
};

class MailFlag : public MailView {
public:
    MailFlag(MailBox*, boolean showcount=false);
    ~MailFlag();

    virtual void Update();
    virtual void Redraw(Coord, Coord, Coord, Coord);
protected:
    void Reconfig();
private:
    boolean showcount;
    int lastcount;
    Painter* highlight;
};

const int MaxItemCount = 100;
const int MaxItemSize = 100;

enum Status { All, New, Unread };

class MailBox : public MonoScene {
public:
    MailBox(
	char* path, int delay, Status stat,
	boolean noflag=false, boolean notext=false, boolean silent=false,
	boolean lookup = false,
	boolean showcount=false,
	int rows=0, int columns=0,
	int width=0, int height=0
    );
    virtual ~MailBox();

    void Attach(Interactor*);
    void Detach(Interactor*);
    char* GetItem(int);
    int GetCount();
    void Handle(Event&);
    void Notify();
    void Scan();
    void Tick(long, long);
private:
    int width;
    int height;
    Viewer* views;
    char* mailboxpath;
    off_t lastsize;
    Status status;
    int count_;
    char* items[MaxItemCount];
    int delay_;
    IOHandler* tick_;

    char* addresses[MAX_NAMES+1];           /* array of email addresses */
    char* aliases[MAX_NAMES+1];             /* array of aliases */
    boolean lookupNames;                    /* whether to lookup */
                                            /*   user names in passwd file */
    char* addressToAlias(char*, boolean);
    void initializeAliases();

    virtual void Reconfig();
};

declareIOCallback(MailBox)
implementIOCallback(MailBox)

inline char* MailBox::GetItem (int i) { return items[i]; }
inline int MailBox::GetCount () { return count_; }

MailView::MailView (MailBox* m) {
    mailbox = m;
    m->Attach(this);
}

MailView::~MailView () {
     mailbox->Remove(this);
}

MailText::MailText (MailBox* m, int r, int c) : MailView(m) {
    rows = r;
    cols = c;
}

void MailText::Reconfig () {
    if (rows != 0 && cols != 0) {
	const Font* f = output->GetFont();
	shape->width = cols * f->Width("m");
	shape->height = rows * f->Height();
    }
}

void MailText::Redraw (Coord x1, Coord y1, Coord x2, Coord y2) {
    output->ClearRect(canvas, x1, y1, x2, y2);
    output->Clip(canvas, x1, y1, x2, y2);
    int height = output->GetFont()->Height();
    Coord h;
    int i;
    for (
	i = 0, h = ((ymax+1) % height) / 2;
	i < mailbox->GetCount() && h <= (ymax+1)-height;
	++i, h += height
    ) {
	output->Text(canvas, mailbox->GetItem(i), 0, h);
    }
    output->NoClip();
}

void MailBeep::Update() {
    int count = mailbox->GetCount();
    if (count > lastcount) {
	::world->RingBell(0);
	::world->RingBell(0);
    }
    lastcount = count;
}

MailFlag::MailFlag (MailBox* m, boolean count) : MailView(m) {
    showcount = count;
    lastcount = 0;
    highlight = nil;
}

MailFlag::~MailFlag () {
    Unref(highlight);
}

void MailFlag::Reconfig () {
    Unref(highlight);
    highlight = new Painter(output);
    highlight->Reference();
    highlight->SetColors(output->GetBgColor(), output->GetFgColor());
    const Font* f = output->GetFont();
    shape->width = f->Width("M") + 6;
    shape->height = f->Height();
    shape->Rigid(0, vfil/1000, 0, vfil);
}

void MailFlag::Update () {
    int count = mailbox->GetCount();
    if (count > lastcount) {
	int i;
	const int bignum = 10000;
	lastcount = 0;
	Draw();
	Sync();
	for (i = 0; i < bignum; ++i);
	lastcount = count;
	Draw();
	Sync();
	for (i = 0; i < bignum; ++i);
	lastcount = 0;
	Draw();
	Sync();
	for (i = 0; i < bignum; ++i);
	lastcount = count;
	Draw();
	Sync();
    } else {
	lastcount = count;
	Draw();
    }
}

void MailFlag::Redraw (Coord x1, Coord y1, Coord x2, Coord y2) {
    Painter* p = (lastcount > 0) ? highlight : output;
    const Font* f = p->GetFont();
    int height = f->Height();
    p->ClearRect(canvas, x1, y1, x2, y2);
    if (ymax+1 < height) {
	;
    } else if (showcount) {
	char c;
	if (mailbox->GetCount() <= 0) {
	    c = '-';
	} else if (mailbox->GetCount() > 9) {
	    c = '*';
	} else {
	    c = '0' + mailbox->GetCount();
	}
	p->Text(canvas, &c, 1, xmax/2 - f->Width("M")/2, ymax/2 - height/2);
    } else if (ymax+1 < 4*height) {
	p->Text(canvas, "M", xmax/2 - f->Width("M")/2, ymax/2 - height/2);
    } else {
	p->Text(canvas, "M", xmax/2 - f->Width("M")/2, ymax/2 + height);
	p->Text(canvas, "a", xmax/2 - f->Width("a")/2, ymax/2);
	p->Text(canvas, "i", xmax/2 - f->Width("i")/2, ymax/2 - height);
	p->Text(canvas, "l", xmax/2 - f->Width("l")/2, ymax/2 - 2*height);
    }
}

MailBox::MailBox(
    char* path, int delay, Status stat,
    boolean noflag, boolean notext, boolean silent, 
    boolean lookup,
    boolean showcount,
    int rows, int cols, int w, int h
) {
    lookupNames = lookup;
    initializeAliases();

    mailboxpath = path;
    status = stat;
    lastsize = 0;
    for (int i = 0; i < MaxItemCount; i++) {
	items[i] = nil;
    }
    count_ = 0;
    delay_ = delay;
    tick_ = new IOCallback(MailBox)(this, &MailBox::Tick);
    input = new Sensor;
    input->Catch(KeyEvent);
    views = nil;
    if (!silent) {
	new MailBeep(this);
    }

    HBox* contents = new HBox;
    if (!noflag) {
	contents->Insert(new MailFlag(this, showcount));
    }
    if (!noflag && !notext) {
	contents->Insert(new VBorder);
    }
    if (!notext) {
	contents->Insert(new HGlue(2, 0, 0));
	contents->Insert(
	    new VBox(
		new VGlue(2, 0, 0),
		new MailText(this, rows, cols),
		new VGlue(2, 0, 0)
	    )
	);
	contents->Insert(new HGlue(2, 0, 0));
    }
    Insert(contents);
    width = w;
    height = h;
}

void MailBox::Reconfig () {
    MonoScene::Reconfig();
    if (width != 0 && height != 0) {
	shape->width = width;
	shape->height = height;
    }
}

void MailBox::Handle (Event &e) {
    if (e.eventType == KeyEvent && e.len > 0 && e.keystring[0] == 'q') {
	e.target = nil;
    }
}

MailBox::~MailBox () {
    while (views != nil) {
	Viewer* doomed = views;
	delete views->view;
	views = views->next;
	delete doomed;
    }
}

void MailBox::Attach (Interactor* i) {
    Viewer* newViewer = new Viewer(i);
    newViewer->next = views;
    views = newViewer;
}

void MailBox::Detach (Interactor* i) {
    register Viewer* v, * prev;

    prev = nil;
    for (v = views; v != nil; v = v->next) {
	if (v->view == i) {
	    if (prev == nil) {
		views = v->next;
	    } else {
		prev->next = v->next;
	    }
	    delete v;
	    break;
	} else {
	    prev = v;
	}
    }
}

void MailBox::Notify() {
    register Viewer* v;

    for (v = views; v != nil; v = v->next) {
	v->view->Update();
    }
}

void MailBox::Tick(long /* sec */, long /* usec */) {
    struct stat statBuffer;

    if (lstat(mailboxpath,&statBuffer) >= 0) {
	if (statBuffer.st_size != lastsize) {
	    count_ = 0;
	    Scan();
	    lastsize = statBuffer.st_size;
	    Notify();
	}
    } else if (lastsize > 0) {
	count_ = 0;
	lastsize = 0;
	Notify();
    }
    Dispatcher::instance().startTimer(delay_, 0, tick_);
}

void MailBox::Scan () {
    FILE* f = fopen(mailboxpath, "r");
    char line[256];
    char info[256];
    char mail[256];
    while (fgets(line, 255, f) != 0) {
        if (sscanf(line, "From %s", info) > 0) {
	    if (info[0] == ':') {
		continue;
	    }
	    char* ip = info;
	    while (ip[0] == '<' || ip[0] == '(') {
		ip ++;
	    }
	    if (ip[0] == '@') {
		ip ++;
		char* colonp = strchr(ip, ':');
		if (colonp != nil) {
		    ip = colonp + 1;
		}
	    }
	    char* atp = strchr(ip, '@');
	    if (atp != nil) {
		char* dotp = strchr(atp, '.');
		if (dotp != nil) {
		    for (int i = 0; i < strlen(mail_domain); ++i) {
			char c = dotp[i];
			c = isupper(c) ? c - 'A' + 'a' : c;
			if (c != mail_domain[i]) {
			    break;
			}
		    }
		    if (i == strlen(mail_domain)) {
			*dotp = '\0';
		    }
		}
	    }
	    for (char* end = & ip[strlen(ip) - 1];
		    *end == '>' || *end == ')';
		    end --
	    ) {
		*end = '\0';
	    }
	    strcpy(mail, addressToAlias(ip, lookupNames));
	} else if (*mail != '\0' &&
	    sscanf(line, "Subject: %s", info) > 0
	) {
	    char* p = strchr(line, '\n');
	    if (p != 0) {
		*p = '\0';
	    }
	    strcat(mail, " << ");
	    strcat(mail, line+9);
	} else if (*mail != '\0' && sscanf(line, "Status: %s", info) > 0) {
	    switch (status) {
	    case New:
		if (strchr(info, 'O') != nil) {
		    *mail = '\0';
		}
		break;
	    case Unread:
		if (strchr(info, 'R') != nil) {
		    *mail = '\0';
		}
		break;
	    default:
		break;
	    }
        } else if (*line == '\n' && *mail != '\0') {
	    delete items[MaxItemCount-1];
	    for (int i=MaxItemCount-2; i>=0; --i) {
		items[i+1] = items[i];
	    }
	    items[0] = new char[MaxItemSize];
	    strncpy(items[0], mail, MaxItemSize - 1);
	    *mail = '\0';
	    ++count_;
        }
    }
    fclose(f);
}

/*
 * If there is an alias for the given email address, make the pointer point
 * to alias instead of the email address
 */

char* MailBox::addressToAlias(char* emailAddress, boolean lookupNames) {
    lowercasifyString(emailAddress);
  
    /* look for a match in the list of aliases */
    for (int i = 0; addresses[i]; i++) {
	if (strcmp(emailAddress, addresses[i]) == 0) {
	    emailAddress = strcpy(new char[strlen(aliases[i])+1], aliases[i]);
	    return emailAddress;
	}
    }

    /* now check the password file */
    if (lookupNames) {
	passwd *pwinfo = getpwnam(emailAddress);
	if (pwinfo) {
	    emailAddress = strcpy(
		new char[strlen(pwinfo->pw_gecos)+1], pwinfo->pw_gecos
	    );
	    char* commaPtr = strchr(emailAddress, ',');
	    if (commaPtr != nil) {
		*commaPtr = '\0';
	    }
	    return emailAddress;
	}
    }
    return emailAddress;
}

void MailBox::initializeAliases() {
    /*
     * Find out the name of the user's home directory.  If we can't find it,
     * quit without building an alias list.
     */
    char* homeDirectory = getenv("HOME");
    if (homeDirectory == 0) {
	return;
    }

    /*
     * Find out the name of the user's alias file;  by default it's 
     * "~/.mailbox_aliases".
     */
    char* aliasesFile = getenv("MAILBOX_ALIASES");
    if (aliasesFile == 0) {
	aliasesFile = ".mailbox_aliases";
    }

    char* aliasFilePath = new char[
	strlen(homeDirectory) + strlen(aliasesFile) + 2
    ];
    *aliasFilePath = '\0';
    strcat(aliasFilePath, homeDirectory);
    strcat(aliasFilePath, "/");
    strcat(aliasFilePath, aliasesFile);

    FILE* f = fopen(aliasFilePath, "r");
    if (f == 0) {
	addresses[0] = nil;
	return;
    }

    /*
     * The format of the alias file is <Alias>|<email address>.  Here is an
     * example:
     *
     *    Scott|sdm
     *    Kathy|kpk
     *    Gail|gms
     *    Tim Farley|timf@buster.wr.tek.com
     *    Mary|ST401792@brownvm.brown.edu
     *    Stan Lippman|stan@mozart.att.com
     *    Dave Lion|davel@hitl.washington.edu
     *    Uncle Don|dfrench@iaps.COM
     *
     * It could be made prettier, but I'm in a hurry.
     */
    char line[256];
    int aliasCount = 0;
    while ((fgets(line, 255, f) != 0) && (aliasCount < MAX_NAMES)) {
	line[strlen(line)-1] = '\0';
	char* aliasPtr = strtok(line, "|");
	if (aliasPtr) {
	    aliases[aliasCount] = strcpy(
		new char[strlen(aliasPtr)+1], aliasPtr
	    );
	    char* addressPtr = line+strlen(aliasPtr)+1;
	    addresses[aliasCount] = strcpy(
		new char[strlen(addressPtr)+1],
		lowercasifyString(addressPtr)
	    );
	    aliasCount++;
        }
    }
    addresses[aliasCount] = nil;
    fclose(f);
}

MailBox* mailbox;

int cols = 45;		    /* size of the text part - cols */
int rows = 4;		    /* .. and lines */
int width = 0;		    /* initial width of window */
int height = 0;		    /* ... height */
int xpos = 0;		    /* initial position of lower left corner - x */
int ypos = 0;		    /* ... y */
int delay = 60;		    /* seconds between checks of mailbox */
Status status = All;	    /* which messages to display */
boolean count = false;	    /* display a count of the mail items in flag */
boolean noflag = false;	    /* show flag by default */
boolean notext = false;	    /* show text by default */
boolean silent = false;	    /* ring bell when mail arrives by default */
boolean lookup = false;

static OptionDesc options[] = {
    { "font=", "*font", OptionValueAfter },
    { nil }
};

int main(int argc, char* argv[]) {
    int i, p1, p2;
    char* curarg;

    world = new World("Mailbox", argc, argv, options);
    for (i = 1; i < argc; i++) {
	curarg = argv[i];
	if (sscanf(curarg, "delay=%d", &p1) == 1) {
	    delay = p1;
	} else if (sscanf(curarg, "pos=%d,%d", &p1, &p2) == 2) {
	    xpos = p1; ypos = p2;
	} else if (sscanf(curarg, "size=%d,%d", &p1, &p2) == 2) {
	    width = p1; height = p2; rows=0; cols=0;
	} else if (sscanf(curarg, "rows=%d", &p1) == 1) {
	    rows = p1; height = 0; width = 0;
	} else if (sscanf(curarg, "cols=%d", &p1) == 1) {
	    cols = p1; height = 0; width = 0;
	} else if (sscanf(curarg, "mailbox=%s", mailpath) == 1) {
	    /* nothing else to do */
	} else if (strcmp(curarg, "count") == 0) {
	    count = true;
	} else if (strcmp(curarg, "new") == 0) {
	    status = New;
	} else if (strcmp(curarg, "unread") == 0) {
	    status = Unread;
	} else if (strcmp(curarg, "all") == 0) {
	    status = All;
	} else if (strcmp(curarg, "noflag") == 0) {
	    noflag = true; notext = false;
	} else if (strcmp(curarg, "notext") == 0) {
	    notext = true; noflag = false;
	} else if (strcmp(curarg, "silent") == 0) {
	    silent = true;
	} else if (strcmp(curarg, "lookup") == 0) {
	    lookup = true;
	} else {
	    fprintf(stderr, "%s: unexpected argument '%s'\n", argv[0], curarg);
	    fprintf(stderr, "usage: %s %s %s %s %s\n",
		argv[0], "[pos=#,#] [size=#,#] [rows=#] [cols=#] [delay=#]",
		"[new] [unread] [all]",
		"[count] [silent] [noflag] [notext]",
		"[font=name] [mailbox=path]"
	    );
	    exit(1);
	}
    }

    if (strlen(mailpath) == 0) {
	strcpy(mailpath, mail_path);
	strcat(mailpath, getenv("USER"));
    }

    mailbox = new MailBox(
	mailpath, delay, status,
	noflag, notext, silent, lookup, count,
	rows, cols, width, height
    );
    if (xpos == 0 && ypos == 0) {
	world->InsertApplication(mailbox);
    } else {
	world->InsertApplication(mailbox, xpos, ypos);
    }

    mailbox->Tick(0, 0);
    mailbox->Run();
    return 0;
}
