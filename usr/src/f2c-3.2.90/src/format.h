#define DEF_C_LINE_LENGTH 77
/* actual max will be 79 */

extern int c_output_line_length;	/* max # chars per line in C source
					   code */

char *write_array_decls (/* FILE *, struct Dimblock * */);
void list_init_data (), write_one_init (), write_output_values ();
int do_init_data ();
chainp data_value ();
