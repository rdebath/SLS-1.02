/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"


SUIT_object SUIT_createBulletinBoardWithClass (char *name, char *className)
{
    SUIT_object o = SUIT_createObject (name, className);

    SUIT_addDisplayToObject (o, "bulletin board", SUIT_passEventDown, SUIT_paintChildren);
    SUIT_deluxeSetFunctionPointer (o, CALLBACK_FUNCTION, NULL, OBJECT);
    SUIT_deluxeSetBoolean (o, CAN_BE_OPENED, TRUE, CLASS);
    SUIT_makePropertyTemporary (o, CAN_BE_OPENED, CLASS);
    SUIT_deluxeSetBoolean (o, BORDER_RAISED, FALSE, CLASS);
    return o;
}


SUIT_object SUIT_createBulletinBoard (char *name)
{
    return SUIT_createBulletinBoardWithClass (name, "bulletin board");
}
