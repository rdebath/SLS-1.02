#define I_STRING
#include "includes.h"


void do_stats(char *ret, int opt, struct Client *cl) {
  char buff[20];
  extern int stat_cooked_out, stat_uncomp_in, stat_uncomp_out,
  stat_cooked_in, stat_cooked_out;
  int i;
  switch(opt) {
  case -8:
    sprintf(ret, "%d %d", stat_cooked_in, stat_cooked_out);
    break;
  case -7:
    sprintf(ret, "%d", clients_waiting);
    break;
  case -6:
    sprintf(ret, "%d", cl->number);
    break;
  case -5:
    sprintf(ret, "%d", baudrate);
    break;
  case -4:
    sprintf(ret, "%d %d", p_in_num, p_out_num);
    break;
  case -3:
    sprintf(ret,"%d %d", stat_modem_in, stat_modem_out);
    break;
  case -2:			/* Return the compression statistics. */
    sprintf(ret,"%d %d %d %d", stat_comp_in, stat_comp_out,
	    stat_uncomp_in, stat_uncomp_out);
    break;
  case -1:			/* Return a list of all the active clients.*/
    ret[0] = 0;
    for (i = 0; i < MAX_CLIENTS;++i) {
      if (clients[i].fd >= 0) {
	sprintf(buff,"%d ", i);
	strcat(ret, buff);
      }
    }
    break;
  default:
    sprintf(ret, "%d %d %d %d %d %d %s", 
	    clients[opt].fd,
	    clients[opt].priority,
	    clients[opt].type,
	    clients[opt].state,
	    clients[opt].pid,
	    clients[opt].number, 
	    clients[opt].name);
    
  }
}
