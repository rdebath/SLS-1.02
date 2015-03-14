const MAXUGLEN = 64;
const NOBODY = -2;
const WILDCARD = -1;
typedef string ugname<MAXUGLEN>;

program UGIDPROG {
    version UGIDVERS {
	int AUTHENTICATE(int) = 1;
	int NAME_UID(ugname) = 2;
	int GROUP_GID(ugname) = 3;
	ugname UID_NAME(int) = 4; 
	ugname GID_GROUP(int) = 5; 
    } = 1;
} = 0x2084e581;
