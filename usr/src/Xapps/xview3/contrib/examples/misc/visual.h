/*=================================================================
 * visual.h - definitions and declarations used by visual.c
 *
 * dmaustin Sat Oct 20 16:26:50 1990
 *=================================================================
 */

/*
 * Information about a screen
 */
typedef struct {
    Xv_Screen    screen;
    int          num_visuals;
    XVisualInfo *visuals;
} Screen_Info;

typedef struct server_info {
    Xv_server    server;
    char        *name;
    Display     *display;
    int          num_screens;
    Screen_Info *screens;
    struct server_info *next;
} Server_Info;

