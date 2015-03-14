/* These are the functions that the property editor (editprop.c) and the
   scrollable list (scrolbox.c) share in common.
*/

int sb_textHeight (SUIT_object listWidget);
int sb_mapListEvent (SUIT_object listWidget, point p);
SUIT_viewport sb_listSubViewport (SUIT_object listWidget, int loc, SUIT_viewport drawingVP, double siblingScrollerCurrentValue, int textheight, int margin);
void sb_makeListConsistent (SUIT_object listWidget);
void sb_listInterestCallback (SUIT_object listWidget, char *name, char *type, void *new, void *old);
void sb_genericPaintScrollableBox (SUIT_object listWidget, void (*DrawSingleItem)(SUIT_object, int, SUIT_viewport));
void sb_listScrollerCallback (SUIT_object scrollbar, char *name, char *type, void *new, void *old);
