#ifndef _theo_yp_h_
#define _theo_yp_h_

#define BINDINGDIR	"/var/yp/binding"

struct ypresp_all {
	bool_t more;
	union {
		struct ypresp_key_val val;
	} ypresp_all_u;
};

#endif
