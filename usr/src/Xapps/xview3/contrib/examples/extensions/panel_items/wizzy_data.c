#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)wizzy_data.c 1.2 91/09/14";
#endif
#endif

#include <xview/wizzy.h>

extern Xv_pkg xv_panel_item_pkg;

Pkg_private int wizzy_init();
Pkg_private Xv_opaque wizzy_set_avlist();
Pkg_private Xv_opaque wizzy_get_attr();
Pkg_private int wizzy_destroy();

Xv_pkg          xv_panel_wizzy_pkg = {
    "Wizzy Item",
    ATTR_WIZZY,
    sizeof(Xv_panel_wizzy),
    &xv_panel_item_pkg,
    wizzy_init,
    wizzy_set_avlist,
    wizzy_get_attr,
    wizzy_destroy,
    NULL			/* no find proc */
};
