/*
 * notice.c --
 * This application creates a frame, a panel, and 3 panel buttons.
 * A message button, a Quit button (to exit the program) and a 
 * dummy "commit" button.  Extra data is attached to the panel 
 * items by the use of XV_KEY_DATA.  The callback routine for the 
 * quit and Commit buttons is generalized enough that it can apply 
 * to either button (or any arbitrary button) because it extracts 
 * the expected "data" (via XV_KEY_DATA) from whatever panel 
 * button might have called it.
 */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/notice.h>

/*
 * assign "data" to panel items using XV_KEY_DATA ... attach the 
 * message panel item, a prompt string specific for the panel 
 * item's notice_prompt, and a callback function if the user 
 * chooses "yes".
 */
#define MSG_ITEM        10 /* any arbitrary integer */
#define NOTICE_PROMPT   11
#define CALLBACK_FUNC   12

main(argc,argv)
int     argc;
char    *argv[];
{
    Frame       frame;
    Panel       panel;
    Panel_item  msg_item;
    Xv_opaque   my_notify_proc();
    extern int  exit();

    /*
     * Initialize XView, and create frame, panel and buttons.
     */
    xv_init(XV_INIT_ARGS, argc, argv, NULL);
    frame = (Frame)xv_create(XV_NULL, FRAME,
        FRAME_LABEL,            argv[0],
        NULL);
    panel = (Panel)xv_create(frame, PANEL,
        PANEL_LAYOUT,           PANEL_VERTICAL,
        NULL);
    msg_item = (Panel_item)xv_create(panel, PANEL_MESSAGE, NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Quit",
        PANEL_NOTIFY_PROC,      my_notify_proc,
        XV_KEY_DATA,            MSG_ITEM,       msg_item,
        /* 
         * attach a prompt specific for this button used by 
         * notice_prompt() 
         */
        XV_KEY_DATA,            NOTICE_PROMPT,  "Really Quit?",
        /* 
         * a callback function to call if the user answers "yes" 
         * to prompt 
         */
        XV_KEY_DATA,            CALLBACK_FUNC,  exit,
        NULL);
    /*
     * now that the Quit button is under the message item, 
     * layout horizontally
     */
    xv_set(panel, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);
    (void) xv_create(panel, PANEL_BUTTON,
        PANEL_LABEL_STRING,     "Commit...",
        PANEL_NOTIFY_PROC,      my_notify_proc,
        XV_KEY_DATA,            MSG_ITEM,       msg_item,
        /* 
         * attach a prompt specific for this button used by 
         * notice_prompt() 
         */
        XV_KEY_DATA,            NOTICE_PROMPT,  "Update all changes?",
        /* 
         * Note there is no callback func here, but one could be 
         * written 
         */
        NULL);

    window_fit(panel);
    window_fit(frame);
    xv_main_loop(frame);
}

/*
 * my_notify_proc()
 * The notice appears as a result of notice_prompt().
 * The "key data" associated with the panel item is extracted via 
 * xv_get().  The resulting choice is displayed in the panel 
 * message item.
 */
Xv_opaque
my_notify_proc(item, event)
Panel_item  item;
Event      *event;
{
    int          result;
    int        (*func)();
    char        *prompt;
    Panel_item   msg_item;
    Panel        panel;

    func = (int(*)())xv_get(item, XV_KEY_DATA, CALLBACK_FUNC);
    prompt = (char *)xv_get(item, XV_KEY_DATA, NOTICE_PROMPT);
    msg_item = (Panel_item)xv_get(item, XV_KEY_DATA, MSG_ITEM);
    panel = (Panel)xv_get(item, PANEL_PARENT_PANEL);
    /*
     *  Create the notice and get a response.
     */
    result = notice_prompt(panel, NULL,
        NOTICE_MESSAGE_STRINGS,
                prompt,
                "Press YES to confirm",
                "Press NO to cancel",
                NULL,
        NOTICE_BUTTON_YES,      "YES",
        NOTICE_BUTTON_NO,       "NO",
        NULL);

    switch(result) {
        case NOTICE_YES:
            xv_set(msg_item, PANEL_LABEL_STRING, "Confirmed", NULL);
            if (func)
                (*func)();
            break;
        case NOTICE_NO:
            xv_set(msg_item, PANEL_LABEL_STRING, "Cancelled", NULL);
            break;
        case NOTICE_FAILED:
            xv_set(msg_item, PANEL_LABEL_STRING, "unable to pop-up", 
              NULL);
            break;
        default:
            xv_set(msg_item, PANEL_LABEL_STRING, "unknown choice", 
              NULL);
    }
}
