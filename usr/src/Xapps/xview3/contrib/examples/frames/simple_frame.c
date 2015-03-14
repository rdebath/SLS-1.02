#include <xview/xview.h>

main()
{
    Frame frame;
    frame = (Frame)xv_create(NULL, FRAME, NULL);
    xv_main_loop(frame);
}
