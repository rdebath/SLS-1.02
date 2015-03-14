/*
 * menu_dir.c -
 * Demonstrate the use of an XView menu in a canvas subwindow.
 * A menu is brought up with the MENU mouse button and displays
 * menu choices representing the files in the directory.  If a
 * directory entry is found, a new pullright item is created with
 * that subdir as the pullright menu's contents.  This implementation
 * creates the entire directory tree initially.  Do not attempt to
 * build a tree from /.  You will most likely run out of resources.
 *
 * argv[1] indicates which directory to start from.
 */
#include <xview/xview.h>
#include <xview/canvas.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <X11/Xos.h>
#ifndef MAXPATHLEN
#include <sys/param.h> /* probably sun/BSD specific */
#endif /* MAXPATHLEN */

Frame   frame;

/*
 * main -
 *      Create a frame, canvas and menu.
 *      A canvas receives input in its canvas_paint_window().
 *      Its callback procedure calls menu_show().
 */
main(argc,argv)
int     argc;
char    *argv[];
{
    Canvas      canvas;
    extern void exit();
    void        my_event_proc();
    Menu        menu;
    Menu_item   mi, add_path_to_menu();

    xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

    frame = (Frame)xv_create(NULL, FRAME,
        FRAME_LABEL,            argv[1]? argv[1] : "cwd",
        FRAME_SHOW_FOOTER,      TRUE,
        NULL);
    canvas = (Canvas)xv_create(frame, CANVAS,
        FRAME_LABEL,    argv[0],
        XV_WIDTH,       400,
        XV_HEIGHT,      100,
        NULL);

    mi = add_path_to_menu(argc > 1? argv[1] : ".");
    menu = (Menu)xv_get(mi, MENU_PULLRIGHT);

    /* associate the menu to the canvas win for easy retrieval */
    xv_set(canvas_paint_window(canvas),
        WIN_CONSUME_EVENTS,     WIN_MOUSE_BUTTONS, NULL,
        WIN_EVENT_PROC,         my_event_proc,
        WIN_CLIENT_DATA,        menu,
        NULL);

    window_fit(frame);
    window_main_loop(frame);
}

/*
 * my_action_proc - display the selected item in the frame footer.
 */
void
my_action_proc(menu, menu_item)
Menu    menu;
Menu_item       menu_item;
{
    xv_set(frame,
        FRAME_LEFT_FOOTER,      xv_get(menu_item, MENU_STRING),
        NULL);
}

/*
 * Call menu_show() to display menu on right mouse button push.
 */
void
my_event_proc(canvas, event)
Canvas  canvas;
Event *event;
{
    if ((event_id(event) == MS_RIGHT) && event_is_down(event)) {
        Menu menu = (Menu)xv_get(canvas, WIN_CLIENT_DATA);
        menu_show(menu, canvas, event, NULL);
    }
}

/*
 * return an allocated char * that points to the last item in a path.
 */
char *
getfilename(path)
char *path;
{
    char *p;

    if (p = rindex(path, '/'))
        p++;
    else
        p = path;
    return strcpy(malloc(strlen(p)+1), p);
}

/*
 * The path passed in is scanned via readdir().  For each file in the
 * path, a menu item is created and inserted into a new menu.  That
 * new menu is made the PULLRIGHT_MENU of a newly created panel item
 * for the path item originally passed it.  Since this routine is
 * recursive, a new menu is created for each subdirectory under the
 * original path.
 */
Menu_item
add_path_to_menu(path)
char *path;
{
    DIR                 *dirp;
    struct direct       *dp;
    struct stat         s_buf;
    Menu_item           mi;
    Menu                next_menu;
    char                buf[MAXPATHLEN];

    /* don't add a folder to the list if user can't read it */
    if (stat(path, &s_buf) == -1 || !(s_buf.st_mode & S_IREAD))
        return NULL;
    if (s_buf.st_mode & S_IFDIR) {
        int cnt = 0;
        if (!(dirp = opendir(path)))
            /* don't bother adding to list if we can't scan it */
            return NULL;
        next_menu = (Menu)xv_create(XV_NULL, MENU, NULL);
        while (dp = readdir(dirp))
            if (strcmp(dp->d_name, ".") && strcmp(dp->d_name, "..")) {
                (void) sprintf(buf, "%s/%s", path, dp->d_name);
                if (!(mi = add_path_to_menu(buf)))
                    /* unreadable file or dir - deactivate item */
                    mi = xv_create(XV_NULL, MENUITEM,
                        MENU_STRING,         getfilename(dp->d_name),
                        MENU_RELEASE,
                        MENU_RELEASE_IMAGE,
                        MENU_INACTIVE,       TRUE,
                        NULL);
                xv_set(next_menu, MENU_APPEND_ITEM, mi, NULL);
                cnt++;
            }
        closedir(dirp);
        mi = xv_create(XV_NULL, MENUITEM,
            MENU_STRING,        getfilename(path),
            MENU_RELEASE,
            MENU_RELEASE_IMAGE,
            MENU_NOTIFY_PROC,   my_action_proc,
            NULL);
        if (!cnt) {
            xv_destroy(next_menu);
            /* An empty or unsearchable directory - deactivate item */
            xv_set(mi, MENU_INACTIVE, TRUE, NULL);
        } else {
            xv_set(next_menu, MENU_TITLE_ITEM, getfilename(path), NULL);
            xv_set(mi, MENU_PULLRIGHT, next_menu, NULL);
        }
        return mi;
    }
    return (Menu_item)xv_create(NULL, MENUITEM,
        MENU_STRING,            getfilename(path),
        MENU_RELEASE,
        MENU_RELEASE_IMAGE,
        MENU_NOTIFY_PROC,       my_action_proc,
        NULL);
}
