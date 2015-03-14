# include <ctype.h>

# define  PRT_MENU	"Choose a printer: arrow keys to move, return to select, 'q' to quit"
# define  PRINT_PATH	"/usr/local/lib/menus/printers"
# ifndef HPUX
# define  PRT_TO_SCR	"/usr/bin/more"
# else
# define  PRT_TO_SCR	"/usr/bin/more"
# endif HPUX
# define  OTHER_LINE	"n)ext page p)revious page"

# define	UP		252
# define	DOWN		253
# define	LEFT		254
# define	RIGHT		255

# define	MAX_PRINTERS	24
# define	MAX_ENT		1024
# define	ENT_LEN		20

# define	isposodd(x) (x % 2)
# define	in_col_one(x) ((x / 2) + (x % 2))
# define	in_col_two(x) (x - in_col_one(x))

struct print_options
{
	char selection[80];
	int option;
	int x;
	int y;
};

static struct print_options print_opts[6] = {
	{"CANCEL",0,5,20},
	{"PRINT",0,16,20},
	{"(Spacing %d)",0,26,20},
	{"(Skip Page Break %s)",0,41,20},
	{"(Copies %d)",0,65,20}
};

typedef struct
{
	char comment[40];
	char command[128];
}printer;

typedef struct 
{
	int max_row;
	int max_col;
}dir_info;

extern DIR *opendir();
# if defined (SUN) || defined (BSD43)
struct direct *readdir();
# else
# endif (SUN || BSD43)
# ifdef BSD43
extern struct direct *readdir();
void closedir();
# else
int closedir();
# endif BSD43

int strcmp();

