#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)ntfy_ctbl.c 1.18 91/09/14";
#endif
#endif

#include <xview_private/ntfy.h>
#include <stdio.h>
#include <signal.h>

/*
 * Add a client into the condition table (ntfy_cndtbl) for the condition it
 * has an interest in.
 */

ntfy_add_to_table(client, condition, type)
    NTFY_CLIENT    *client;
    NTFY_CONDITION *condition;
    int             type;
{
    NTFY_CNDTBL    *cnd_list = ntfy_cndtbl[type];

    NTFY_BEGIN_CRITICAL;
    if (!cnd_list) {
	/* Create the head, which is never used */
	cnd_list = (NTFY_CNDTBL *) xv_malloc(sizeof(NTFY_CNDTBL));
	cnd_list->client = (NTFY_CLIENT *) NULL;
	cnd_list->condition = (NTFY_CONDITION *) NULL;
	cnd_list->next = (NTFY_CNDTBL *) NULL;
	ntfy_cndtbl[type] = cnd_list;

	/*
	 * Create the first clt/cnd in the list, along with ptrs back to the
	 * actual clt and condition
	 */
	cnd_list = (NTFY_CNDTBL *) xv_malloc(sizeof(NTFY_CNDTBL));
	cnd_list->client = client;
	cnd_list->condition = condition;
	cnd_list->next = ntfy_cndtbl[type]->next;
	ntfy_cndtbl[type]->next = cnd_list;
	NTFY_END_CRITICAL;
	return;
    }
    /* See if a particular client already has registered this condition. */
    cnd_list = cnd_list->next;
    while (cnd_list) {
	ntfy_assert(cnd_list->condition->type == condition->type, 25
		    /* Found wrong condition type in condition table */);
	if ((cnd_list->client == client) &&
	    (cnd_list->condition == condition)) {
	    NTFY_END_CRITICAL;
	    return;
	}
	cnd_list = cnd_list->next;
    }

    cnd_list = (NTFY_CNDTBL *) xv_malloc(sizeof(NTFY_CNDTBL));
    cnd_list->client = client;
    cnd_list->condition = condition;
    cnd_list->next = ntfy_cndtbl[type]->next;
    ntfy_cndtbl[type]->next = cnd_list;
    NTFY_END_CRITICAL;
    return;
}

/*
 * Remove a condition interest for a particular client from the condition
 * table.
 */

ntfy_remove_from_table(client, condition)
    NTFY_CLIENT    *client;
    NTFY_CONDITION *condition;
{
    NTFY_CNDTBL    *cnd_list, *last_cnd;

    if ((int) condition->type >= NTFY_LAST_CND)
	return;

    NTFY_BEGIN_CRITICAL;
    cnd_list = last_cnd = ntfy_cndtbl[(int) condition->type];

    ntfy_assert(cnd_list, 26 /* Condition list has a NULL head */);

    cnd_list = cnd_list->next;
    while (cnd_list) {
	ntfy_assert(cnd_list->condition->type == condition->type, 27
		    /* Found wrong condition type in condition table */);
	if ((cnd_list->client == client) &&
	    (cnd_list->condition == condition)) {
	    last_cnd->next = cnd_list->next;
	    free(cnd_list);
	    NTFY_END_CRITICAL;
	    return;
	}
	last_cnd = cnd_list;
	cnd_list = cnd_list->next;
    }
    NTFY_END_CRITICAL;
}

/* VARARGS2 */
pkg_private     NTFY_ENUM
ntfy_new_enum_conditions(cnd_list, enum_func, context)
    NTFY_CNDTBL    *cnd_list;
    NTFY_ENUM_FUNC  enum_func;
    NTFY_ENUM_DATA  context;
{
    if (!cnd_list)
	return (NTFY_ENUM_NEXT);

    cnd_list = cnd_list->next;

#ifdef notdef
    if (cnd_list)
	dump_table(cnd_list->condition->type);
#endif

    while (cnd_list) {
	switch (enum_func(cnd_list->client, cnd_list->condition,
			  context)) {
	  case NTFY_ENUM_SKIP:
	    break;
	  case NTFY_ENUM_TERM:
	    return (NTFY_ENUM_TERM);
	  default:{
	    }
	}
	cnd_list = cnd_list->next;
    }
    return (NTFY_ENUM_NEXT);
}

#define NTFY_BEGIN_PARANOID     ntfy_paranoid_count++
#define NTFY_IN_PARANOID        (ntfy_paranoid_count > 0)
#define NTFY_END_PARANOID       ntfy_paranoid_count--;

/* Variables used in paranoid enumerator */
static NTFY_CONDITION *ntfy_enum_condition;
static NTFY_CONDITION *ntfy_enum_condition_next;
static          ntfy_paranoid_count;

pkg_private     NTFY_ENUM
ntfy_new_paranoid_enum_conditions(cnd_list, enum_func, context)
    NTFY_CNDTBL    *cnd_list;
    NTFY_ENUM_FUNC  enum_func;
    NTFY_ENUM_DATA  context;
{
    extern NTFY_CLIENT *ntfy_enum_client;
    extern NTFY_CLIENT *ntfy_enum_client_next;
    extern u_int    ndet_sigs_managing;
    NTFY_ENUM       return_code = NTFY_ENUM_NEXT;
#ifdef SVR4
    sigset_t oldmask, newmask;
    sigemptyset(&newmask);
    newmask.sigbits[0] = ndet_sigs_managing;  /* assume interesting < 32 */
    sigprocmask(SIG_BLOCK, &newmask , &oldmask);
#else
    int             oldmask = sigblock((int) ndet_sigs_managing);	/* SYSTEM CALL */
#endif SVR4

    /*
     * Blocking signals because async signal sender uses this paranoid
     * enumerator.
     */

    ntfy_assert(!NTFY_IN_PARANOID, 28
		/* More then 1 paranoid using enumerator */);
    NTFY_BEGIN_PARANOID;

    if (!cnd_list)
	goto Done;

    cnd_list = cnd_list->next;

#ifdef notdef
    if (cnd_list)
	dump_table(cnd_list->condition->type);
#endif

    while (cnd_list) {
	ntfy_enum_client = cnd_list->client;
	ntfy_enum_condition = cnd_list->condition;
	switch (enum_func(ntfy_enum_client, ntfy_enum_condition,
			  context)) {
	  case NTFY_ENUM_SKIP:
	    break;
	  case NTFY_ENUM_TERM:
	    return_code = NTFY_ENUM_TERM;
	    goto Done;
	  default:
	    if (ntfy_enum_client == NTFY_CLIENT_NULL)
		goto BreakOut;
	}
	cnd_list = cnd_list->next;
    }
BreakOut:
    {
    }
Done:
    /* Reset global state */
    ntfy_enum_client = ntfy_enum_client_next = NTFY_CLIENT_NULL;
    ntfy_enum_condition = ntfy_enum_condition_next = NTFY_CONDITION_NULL;
    NTFY_END_PARANOID;
#ifdef SVR4
    sigprocmask(SIG_SETMASK, &oldmask, (sigset_t *) 0);
#else
    (void) sigsetmask(oldmask); /* SYSTEM CALL */
#endif SVR4
    return (return_code);
}

#ifdef notdef
dump_table(type)
    int             type;
{
    NTFY_CNDTBL    *cnd_list;
    int             i;

    fprintf(stderr, "\n\n");
    fprintf(stderr, "Searching for type %d\n", type);
    for (i = 0; i < NTFY_LAST_CND; i++) {
	if (ntfy_cndtbl[i]) {
	    cnd_list = ntfy_cndtbl[i]->next;
	    fprintf(stderr, "%d: ", i);
	    while (cnd_list) {
		/*
		 * fprintf (stderr, "%d, ", cnd_list->condition->type);
		 */
		fprintf(stderr, "%d (0x%x [0x%x] 0x%x), ", cnd_list->condition->type, cnd_list->client, cnd_list->client->nclient, cnd_list->condition);
		cnd_list = cnd_list->next;
	    }
	    fprintf(stderr, "\n");
	}
    }
    fprintf(stderr, "\n");
}

#endif
