#define MAXUGLEN 64
#define NOBODY -2
#define WILDCARD -1

typedef char *ugname;
bool_t xdr_ugname();


#define UGIDPROG ((u_long)0x2084e581)
#define UGIDVERS ((u_long)1)
#define AUTHENTICATE ((u_long)1)
extern int *authenticate_1();
#define NAME_UID ((u_long)2)
extern int *name_uid_1();
#define GROUP_GID ((u_long)3)
extern int *group_gid_1();
#define UID_NAME ((u_long)4)
extern ugname *uid_name_1();
#define GID_GROUP ((u_long)5)
extern ugname *gid_group_1();

