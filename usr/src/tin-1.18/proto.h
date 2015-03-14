#if __STDC__
 
/* active.c */
extern void resync_active_file(void);
extern int find_group_index(char *group);
extern int parse_active_line(char *line, long *max, long *min, char *moderated);
extern void read_news_active_file(void);
extern void backup_active(int create);
extern void check_for_any_new_groups(void);
extern int prompt_subscribe_group(char *group);
extern void set_default_attributes(void);
extern void read_attributes_file(void);
extern void write_attributes_file(void);
extern void read_active_times_file(void);
extern void write_active_times_file(void);
extern void load_active_size_info(char *info);
extern int find_active_size_index(char *cur_active_server);
extern void read_motd_file(void);
/* amiga.c */
/* art.c */
extern void find_base(int index);
extern int num_of_arts(void);
extern int valid_artnum(long art);
extern int purge_needed(char *group_path);
extern int index_group(char *group, char *group_path);
extern int read_group(char *group, char *group_path);
extern void make_threads(int rethread);
extern int parse_headers(FILE *fp, struct article_t *h);
extern void write_index_file(char *group);
extern void read_index_file(char *group_name);
extern int find_index_file(char *group);
extern void do_update(void);
extern int artnum_comp(char *p1, char *p2);
extern int subj_comp(char *p1, char *p2);
extern int from_comp(char *p1, char *p2);
extern int date_comp(char *p1, char *p2);
extern void set_article(struct article_t *art);
extern int input_pending(void);
/* curses.c */
extern void setup_screen(void);
extern int InitScreen(void);
extern void ScreenSize(int *num_lines, int *num_columns);
extern void InitWin(void);
extern void EndWin(void);
extern void set_keypad_on(void);
extern void set_keypad_off(void);
extern void ClearScreen(void);
extern void MoveCursor(int row, int col);
extern void CleartoEOLN(void);
extern void CleartoEOS(void);
extern void StartInverse(void);
extern void EndInverse(void);
extern void ToggleInverse(void);
extern int RawState(void);
extern void Raw(int state);
extern int ReadCh(void);
extern int outchar(int c);
/* debug.c */
extern void debug_nntp(char *func, char *line);
extern void debug_nntp_respcode(int respcode);
extern void debug_print_arts(void);
extern void debug_print_header(struct article_t *s);
extern void debug_save_comp(void);
extern void debug_print_comment(char *comment);
extern void debug_print_base(void);
extern void debug_print_active(void);
extern void debug_print_active_hash(void);
/* envarg.c */
extern int count_args(char *s);
extern void envargs(int *Pargc, char ***Pargv, char *envstr);
/* feed.c */
extern void feed_articles(int function, int level, char *prompt, int respnum, char *group_path);
extern int print_file(char *command, int respnum, int count);
extern int get_post_proc_type(int proc_type);
extern int does_article_exist(int function, long artnum, char *path);
/* getline.c */
extern char *getline(char *prompt, int number_only, char *str);
/* group.c */
extern void decr_tagged(int tag);
extern void group_page(char *group);
extern void fix_new_highest(int groupnum);
extern void show_group_page(void);
extern void update_group_page(void);
extern void draw_subject_arrow(void);
extern void erase_subject_arrow(void);
extern void prompt_subject_num(int ch, char *group);
extern void clear_note_area(void);
extern int find_new_pos(int old_top, long old_artnum, int cur_pos);
extern void mark_screen(int level, int screen_row, int screen_col, char *value);
extern void set_subj_from_size(int num_cols);
extern void toggle_subject_from(void);
extern void show_group_title(int clear_title);
/* hashstr.c */
extern char *hash_str(char *s);
extern struct hashnode *add_string(char *s);
extern void hash_init(void);
extern void hash_reclaim(void);
/* help.c */
extern void show_info_page(int type, char *help[], char *title);
extern void display_info_page(void);
extern void show_mini_help(int level);
extern void toggle_mini_help(int level);
/* inews.c */
extern int submit_inews(char *name);
extern void get_host_name(char *host_name);
extern void get_user_info(char *user_name, char *full_name);
extern void get_from_name(char *user_name, char *host_name, char *full_name, char *from_name);
/* init.c */
extern void init_selfinfo(void);
extern void set_tindir(void);
extern int create_mail_save_dirs(void);
/* kill.c */
extern int read_kill_file(void);
extern void write_kill_file(void);
extern int kill_art_menu(char *group_name, int index);
extern int unkill_all_articles(void);
extern int kill_any_articles(int index);
extern int auto_select_articles(int index);
/* lang.c */
/* mail.c */
extern void read_mail_active_file(void);
extern void write_mail_active_file(void);
extern void read_mailgroups_file(void);
extern void read_newsgroups_file(void);
extern void read_groups_descriptions(FILE *fp, FILE *fp_save);
/* main.c */
extern void main(int argc, char *argv[]);
extern void read_cmd_line_options(int argc, char *argv[]);
extern void usage(char *progname);
extern int check_for_any_new_news(int check_any_unread, int start_any_unread);
extern void save_or_mail_new_news(void);
extern void update_index_files(void);
extern void show_intro_page(void);
extern int read_cmd_line_groups(void);
/* memory.c */
extern void init_alloc(void);
extern void expand_art(void);
extern void expand_active(void);
extern void expand_kill(void);
extern void expand_save(void);
extern void expand_spooldirs(void);
extern void expand_active_size(void);
extern void init_screen_array(int allocate);
extern void free_all_arrays(void);
extern void free_art_array(void);
extern void free_attributes_array(void);
extern void free_active_arrays(void);
extern void free_kill_array(void);
extern void free_save_array(void);
extern void free_spooldirs_array(void);
extern void free_active_size_array(void);
extern char *my_malloc(unsigned size);
extern char *my_realloc(char *p, unsigned size);
/* misc.c */
extern void asfail(char *file, int line, char *cond);
extern void copy_fp(FILE *fp_ip, FILE *fp_op, char *prefix);
extern char *get_val(char *env, char *def);
extern int invoke_editor(char *nam);
extern int invoke_ispell(char *nam);
extern void shell_escape(void);
extern void tin_done(int ret);
extern long hash_groupname(char *group);
extern void rename_file(char *old_filename, char *new_filename);
extern char *str_dup(char *str);
extern int invoke_cmd(char *nam);
extern void draw_percent_mark(int cur_num, int max_num);
extern void set_real_uid_gid(void);
extern void set_tin_uid_gid(void);
extern void basename(char *dirname, char *program);
extern void mail_setup(void);
extern int mail_check(void);
extern void parse_from(char *from_line, char *eaddr, char *fname);
extern long my_atol(char *s, int n);
extern int my_stricmp(char *p, char *q);
extern char *eat_re(char *s);
extern long hash_s(char *s);
extern void my_strncpy(char *p, char *q, int n);
extern int untag_all_articles(void);
extern char *str_str(char *text, char *pattern, int patlen);
extern void get_author(int thread, int respnum, char *str);
extern void toggle_inverse_video(void);
extern int get_arrow_key(void);
extern void create_index_lock_file(char *lock_file);
extern int strfquote(char *group, int respnum, char *s, int maxsize, char *format);
extern int strfpath(char *format, char *str, int maxsize, char *homedir, char *maildir, char *savedir, char *group);
extern void get_cwd(char *buf);
extern void make_group_path(char *name, char *path);
extern void cleanup_tmp_files(void);
/* newsrc.c */
extern int auto_subscribe_groups(void);
extern void backup_newsrc(void);
extern void read_newsrc(int sub_only);
extern void write_newsrc(void);
extern void rewrite_newsrc(void);
extern void read_newsrc_line(char *group);
extern void update_newsrc(char *group, int groupnum, int mark_unread);
extern void subscribe(char *group, int ch, int num, int out_seq);
extern void reset_newsrc(void);
extern void delete_group(char *group);
extern int undel_group(void);
extern void mark_group_read(char *group, int groupnum);
extern void parse_seq(char *s);
extern int parse_unread(char *s, int groupnum);
extern int get_line_unread(char *group, int groupnum);
extern void print_seq(FILE *fp, int groupnum);
extern int pos_group_in_newsrc(char *group, int pos);
extern void mark_all_xref_read(char *xref_line);
/* nntplib.c */
extern char *getserverbyfile(char *file);
extern int server_init(char *machine, char *service, int port);
extern int get_tcp_socket(char *machine, char *service, int port);
extern int handle_server_response(int response, char *nntpserver);
extern void put_server(char *string);
extern int get_server(char *string, int size);
extern void close_server(void);
/* open.c */
extern void nntp_open(void);
extern void nntp_close(void);
extern FILE *open_mail_active_fp(char *mode);
extern FILE *open_news_active_fp(void);
extern FILE *open_newgroups_fp(int index);
extern FILE *open_motd_fp(char *motd_file_date);
extern FILE *open_subscription_fp(void);
extern FILE *open_mailgroups_fp(void);
extern FILE *open_newsgroups_fp(void);
extern FILE *open_index_fp(char *group_name);
extern int stat_article(long art, char *group_path);
extern FILE *open_art_fp(char *group_path, long art);
extern FILE *open_header_fp(long art);
extern int base_comp(char *p1, char *p2);
extern void setup_base(char *group, char *group_path);
extern int get_respcode(void);
extern int stuff_nntp(char *fnam);
extern FILE *nntp_to_fp(void);
extern void log_user(void);
extern void authorization(char *server, char *authuser);
extern char *nntp_respcode(int respcode);
/* page.c */
extern int show_page(int respnum, int *threadnum, char *group, char *group_path);
extern void redraw_page(int respnum, char *group);
extern void show_note_page(int respnum, char *group);
extern void show_first_header(int respnum, char *group);
extern void show_cont_header(int respnum);
extern int art_open(long art, char *group_path);
extern void art_close(void);
extern int prompt_response(int ch, int respnum);
extern void yank_to_addr(char *orig, char *addr);
extern int show_last_page(void);
extern int match_header(char *buf, char *pat, char *body, int len);
/* parsedate.y */
extern int GetTimeInfo(TIMEINFO *Now);
extern time_t parsedate(char *p, TIMEINFO *now);
/* post.c */
extern int user_posted_messages(void);
extern void update_art_posted_file(char *group, int action, char *subj);
extern int post_header_ok(char *article);
extern void quick_post_article(void);
extern int post_article(char *group, int *posted);
extern int post_response(char *group, int respnum, int copy_text);
extern int mail_to_someone(int respnum, char *address, int mail_to_poster, int confirm_to_mail, int *mailed_ok);
extern int mail_bug_report(void);
extern int mail_to_author(char *group, int respnum, int copy_text);
extern void find_mail_header(int header, char *file, char *value);
extern int delete_article(char *group, int respnum);
extern int crosspost_article(char *group, int respnum);
extern int submit_file(char *name);
extern void insert_x_headers(char *infile);
extern void find_reply_to_addr(int respnum, char *from_addr);
extern void reread_active_file_after_posting(void);
/* prompt.c */
extern int prompt_num(int ch, char *prompt);
extern int prompt_string(char *prompt, char *buf);
extern int prompt_menu_string(int line, int col, char *var);
extern int prompt_yn(int line, char *prompt, int prompt_ch);
extern void prompt_on_off(int row, int col, int *var, char *help_text, char *prompt_text);
extern void continue_prompt(void);
/* rcfile.c */
extern int read_rcfile(void);
extern void write_rcfile(void);
extern int change_rcfile(char *group, int kill_at_once);
extern void show_rcfile_menu(void);
extern void expand_rel_abs_pathname(int line, int col, char *str);
extern void show_menu_help(char *help_message);
extern int match_boolean(char *line, char *pat, int *dst);
extern int match_number(char *line, char *pat, int *dst);
extern int match_string(char *line, char *pat, char *dst, int dstlen);
extern void quote_dash_to_space(char *s);
extern char *quote_space_to_dash(char *s);
/* save.c */
extern int check_start_save_any_news(int check_start_save);
extern int save_art_to_file(int respnum, int index, int mailbox, char *filename);
extern int save_thread_to_file(int is_mailbox, char *group_path);
extern int save_regex_arts(int is_mailbox, char *group_path);
extern int append_to_existing_file(int i);
extern int create_path(char *path);
extern int create_sub_dir(int i);
extern void add_to_save_list(int index, struct article_t *article, int is_mailbox, int archive_save, char *path);
extern void sort_save_list(void);
extern int save_comp(char *p1, char *p2);
extern char *save_filename(int i);
extern char *get_first_savefile(void);
extern char *get_last_savefile(void);
extern int post_process_files(int proc_type_ch);
extern void post_process_uud(int pp);
extern void post_process_sh(void);
extern char *get_archive_file(char *dir, char *ext);
extern void delete_processed_files(void);
extern void print_art_seperator_line(FILE *fp, int mailbox);
/* screen.c */
extern void info_message(char *str);
extern void wait_message(char *str);
extern void error_message(char *template, char *str);
extern void perror_message(char *template, char *str);
extern void clear_message(void);
extern void center_line(int line, int inverse, char *str);
extern void draw_arrow(int line);
extern void erase_arrow(int line);
extern void show_title(char *title);
extern void ring_bell(void);
/* search.c */
extern int search_author(int index, int current_art, int forward);
extern void search_group(int forward);
extern void search_subject(int forward, char *group);
extern int search_article(int forward);
extern void make_lower(char *s, char *t);
/* select.c */
extern void selection_index(int start_groupnum);
extern void group_selection_page(void);
extern int prompt_group_num(int ch);
extern void erase_group_arrow(void);
extern void draw_group_arrow(void);
extern void yank_active_file(void);
extern int choose_new_group(void);
extern int add_group(char *s, int get_unread);
extern int reposition_group(char *group, int default_num);
extern void catchup_group(int goto_next_unread_group);
extern void next_unread_group(int enter_group);
extern void set_groupname_len(int all_groups);
extern void toggle_my_groups(int only_unread_groups, char *group);
extern void goto_next_group_on_screen(void);
extern void strip_line(char *line, int len);
/* sigfile.c */
extern void add_signature(FILE *fp, int flag);
extern FILE *open_random_sig(char *sigdir);
extern int thrashdir(char *sigdir);
/* signal.c */
extern sigtype_t (*sigdisp(int sig, sigtype_t (*func)()))();
extern void set_signal_handlers(void);
extern void set_alarm_signal(void);
extern void set_alarm_clock_on(void);
extern void set_alarm_clock_off(void);
extern void signal_handler(int sig);
extern int set_win_size(int *num_lines, int *num_cols);
extern void set_signals_art(void);
extern void set_signals_group(void);
extern void set_signals_help(void);
extern void set_signals_page(void);
extern void set_signals_select(void);
extern void set_signals_spooldir(void);
extern void set_signals_thread(void);
extern void art_suspend(int sig);
extern void main_suspend(int sig);
extern void select_suspend(int sig);
extern void spooldir_suspend(int sig);
extern void group_suspend(int sig);
extern void help_suspend(int sig);
extern void page_suspend(int sig);
extern void thread_suspend(int sig);
extern void rcfile_suspend(int sig);
extern void art_resize(int sig);
extern void main_resize(int sig);
extern void select_resize(int sig);
extern void spooldir_resize(int sig);
extern void group_resize(int sig);
extern void help_resize(int sig);
extern void page_resize(int sig);
extern void thread_resize(int sig);
/* spooldir.c */
extern int spooldir_index(void);
extern void show_spooldir_page(void);
extern int prompt_spooldir_num(int ch);
extern void erase_spooldir_arrow(void);
extern void draw_spooldir_arrow(void);
extern int load_spooldirs(void);
extern void get_spooldir(void);
extern int set_spooldir(char *name);
/* strftime.c */
extern size_t my_strftime(char *s, size_t maxsize, char *format, struct tm *timeptr);
/* thread.c */
extern int show_thread(int respnum, char *group, char *group_path);
extern void show_thread_page(void);
extern void update_thread_page(void);
extern void draw_thread_arrow(void);
extern void erase_thread_arrow(void);
extern int prompt_thread_num(int ch);
extern int new_responses(int thread);
extern int which_thread(int n);
extern int which_response(int n);
extern int num_of_responses(int n);
extern int stat_thread(int n, struct art_stat_t *sbuf);
extern int next_response(int n);
extern int next_thread(int n);
extern int prev_response(int n);
extern int choose_response(int i, int n);
extern int next_unread(int n);
extern int prev_unread(int n);
/* wildmat.c */
extern int wildmat(char *text, char *p);
 
#else
 
/* active.c */
extern void resync_active_file(/*void*/);
extern int find_group_index(/*char *group*/);
extern int parse_active_line(/*char *line, long *max, long *min, char *moderated*/);
extern void read_news_active_file(/*void*/);
extern void backup_active(/*int create*/);
extern void check_for_any_new_groups(/*void*/);
extern int prompt_subscribe_group(/*char *group*/);
extern void set_default_attributes(/*void*/);
extern void read_attributes_file(/*void*/);
extern void write_attributes_file(/*void*/);
extern void read_active_times_file(/*void*/);
extern void write_active_times_file(/*void*/);
extern void load_active_size_info(/*char *info*/);
extern int find_active_size_index(/*char *cur_active_server*/);
extern void read_motd_file(/*void*/);
/* amiga.c */
/* art.c */
extern void find_base(/*int index*/);
extern int num_of_arts(/*void*/);
extern int valid_artnum(/*long art*/);
extern int purge_needed(/*char *group_path*/);
extern int index_group(/*char *group, char *group_path*/);
extern int read_group(/*char *group, char *group_path*/);
extern void make_threads(/*int rethread*/);
extern int parse_headers(/*FILE *fp, struct article_t *h*/);
extern void write_index_file(/*char *group*/);
extern void read_index_file(/*char *group_name*/);
extern int find_index_file(/*char *group*/);
extern void do_update(/*void*/);
extern int artnum_comp(/*char *p1, char *p2*/);
extern int subj_comp(/*char *p1, char *p2*/);
extern int from_comp(/*char *p1, char *p2*/);
extern int date_comp(/*char *p1, char *p2*/);
extern void set_article(/*struct article_t *art*/);
extern int input_pending(/*void*/);
/* curses.c */
extern void setup_screen(/*void*/);
extern int InitScreen(/*void*/);
extern void ScreenSize(/*int *num_lines, int *num_columns*/);
extern void InitWin(/*void*/);
extern void EndWin(/*void*/);
extern void set_keypad_on(/*void*/);
extern void set_keypad_off(/*void*/);
extern void ClearScreen(/*void*/);
extern void MoveCursor(/*int row, int col*/);
extern void CleartoEOLN(/*void*/);
extern void CleartoEOS(/*void*/);
extern void StartInverse(/*void*/);
extern void EndInverse(/*void*/);
extern void ToggleInverse(/*void*/);
extern int RawState(/*void*/);
extern void Raw(/*int state*/);
extern int ReadCh(/*void*/);
extern int outchar(/*int c*/);
/* debug.c */
extern void debug_nntp(/*char *func, char *line*/);
extern void debug_nntp_respcode(/*int respcode*/);
extern void debug_print_arts(/*void*/);
extern void debug_print_header(/*struct article_t *s*/);
extern void debug_save_comp(/*void*/);
extern void debug_print_comment(/*char *comment*/);
extern void debug_print_base(/*void*/);
extern void debug_print_active(/*void*/);
extern void debug_print_active_hash(/*void*/);
/* envarg.c */
extern int count_args(/*char *s*/);
extern void envargs(/*int *Pargc, char ***Pargv, char *envstr*/);
/* feed.c */
extern void feed_articles(/*int function, int level, char *prompt, int respnum, char *group_path*/);
extern int print_file(/*char *command, int respnum, int count*/);
extern int get_post_proc_type(/*int proc_type*/);
extern int does_article_exist(/*int function, long artnum, char *path*/);
/* getline.c */
extern char *getline(/*char *prompt, int number_only, char *str*/);
/* group.c */
extern void decr_tagged(/*int tag*/);
extern void group_page(/*char *group*/);
extern void fix_new_highest(/*int groupnum*/);
extern void show_group_page(/*void*/);
extern void update_group_page(/*void*/);
extern void draw_subject_arrow(/*void*/);
extern void erase_subject_arrow(/*void*/);
extern void prompt_subject_num(/*int ch, char *group*/);
extern void clear_note_area(/*void*/);
extern int find_new_pos(/*int old_top, long old_artnum, int cur_pos*/);
extern void mark_screen(/*int level, int screen_row, int screen_col, char *value*/);
extern void set_subj_from_size(/*int num_cols*/);
extern void toggle_subject_from(/*void*/);
extern void show_group_title(/*int clear_title*/);
/* hashstr.c */
extern char *hash_str(/*char *s*/);
extern struct hashnode *add_string(/*char *s*/);
extern void hash_init(/*void*/);
extern void hash_reclaim(/*void*/);
/* help.c */
extern void show_info_page(/*int type, char *help[], char *title*/);
extern void display_info_page(/*void*/);
extern void show_mini_help(/*int level*/);
extern void toggle_mini_help(/*int level*/);
/* inews.c */
extern int submit_inews(/*char *name*/);
extern void get_host_name(/*char *host_name*/);
extern void get_user_info(/*char *user_name, char *full_name*/);
extern void get_from_name(/*char *user_name, char *host_name, char *full_name, char *from_name*/);
/* init.c */
extern void init_selfinfo(/*void*/);
extern void set_tindir(/*void*/);
extern int create_mail_save_dirs(/*void*/);
/* kill.c */
extern int read_kill_file(/*void*/);
extern void write_kill_file(/*void*/);
extern int kill_art_menu(/*char *group_name, int index*/);
extern int unkill_all_articles(/*void*/);
extern int kill_any_articles(/*int index*/);
extern int auto_select_articles(/*int index*/);
/* lang.c */
/* mail.c */
extern void read_mail_active_file(/*void*/);
extern void write_mail_active_file(/*void*/);
extern void read_mailgroups_file(/*void*/);
extern void read_newsgroups_file(/*void*/);
extern void read_groups_descriptions(/*FILE *fp, FILE *fp_save*/);
/* main.c */
extern void main(/*int argc, char *argv[]*/);
extern void read_cmd_line_options(/*int argc, char *argv[]*/);
extern void usage(/*char *progname*/);
extern int check_for_any_new_news(/*int check_any_unread, int start_any_unread*/);
extern void save_or_mail_new_news(/*void*/);
extern void update_index_files(/*void*/);
extern void show_intro_page(/*void*/);
extern int read_cmd_line_groups(/*void*/);
/* memory.c */
extern void init_alloc(/*void*/);
extern void expand_art(/*void*/);
extern void expand_active(/*void*/);
extern void expand_kill(/*void*/);
extern void expand_save(/*void*/);
extern void expand_spooldirs(/*void*/);
extern void expand_active_size(/*void*/);
extern void init_screen_array(/*int allocate*/);
extern void free_all_arrays(/*void*/);
extern void free_art_array(/*void*/);
extern void free_attributes_array(/*void*/);
extern void free_active_arrays(/*void*/);
extern void free_kill_array(/*void*/);
extern void free_save_array(/*void*/);
extern void free_spooldirs_array(/*void*/);
extern void free_active_size_array(/*void*/);
extern char *my_malloc(/*unsigned size*/);
extern char *my_realloc(/*char *p, unsigned size*/);
/* misc.c */
extern void asfail(/*char *file, int line, char *cond*/);
extern void copy_fp(/*FILE *fp_ip, FILE *fp_op, char *prefix*/);
extern char *get_val(/*char *env, char *def*/);
extern int invoke_editor(/*char *nam*/);
extern int invoke_ispell(/*char *nam*/);
extern void shell_escape(/*void*/);
extern void tin_done(/*int ret*/);
extern long hash_groupname(/*char *group*/);
extern void rename_file(/*char *old_filename, char *new_filename*/);
extern char *str_dup(/*char *str*/);
extern int invoke_cmd(/*char *nam*/);
extern void draw_percent_mark(/*int cur_num, int max_num*/);
extern void set_real_uid_gid(/*void*/);
extern void set_tin_uid_gid(/*void*/);
extern void basename(/*char *dirname, char *program*/);
extern void mail_setup(/*void*/);
extern int mail_check(/*void*/);
extern void parse_from(/*char *from_line, char *eaddr, char *fname*/);
extern long my_atol(/*char *s, int n*/);
extern int my_stricmp(/*char *p, char *q*/);
extern char *eat_re(/*char *s*/);
extern long hash_s(/*char *s*/);
extern void my_strncpy(/*char *p, char *q, int n*/);
extern int untag_all_articles(/*void*/);
extern char *str_str(/*char *text, char *pattern, int patlen*/);
extern void get_author(/*int thread, int respnum, char *str*/);
extern void toggle_inverse_video(/*void*/);
extern int get_arrow_key(/*void*/);
extern void create_index_lock_file(/*char *lock_file*/);
extern int strfquote(/*char *group, int respnum, char *s, int maxsize, char *format*/);
extern int strfpath(/*char *format, char *str, int maxsize, char *homedir, char *maildir, char *savedir, char *group*/);
extern void get_cwd(/*char *buf*/);
extern void make_group_path(/*char *name, char *path*/);
extern void cleanup_tmp_files(/*void*/);
/* newsrc.c */
extern int auto_subscribe_groups(/*void*/);
extern void backup_newsrc(/*void*/);
extern void read_newsrc(/*int sub_only*/);
extern void write_newsrc(/*void*/);
extern void rewrite_newsrc(/*void*/);
extern void read_newsrc_line(/*char *group*/);
extern void update_newsrc(/*char *group, int groupnum, int mark_unread*/);
extern void subscribe(/*char *group, int ch, int num, int out_seq*/);
extern void reset_newsrc(/*void*/);
extern void delete_group(/*char *group*/);
extern int undel_group(/*void*/);
extern void mark_group_read(/*char *group, int groupnum*/);
extern void parse_seq(/*char *s*/);
extern int parse_unread(/*char *s, int groupnum*/);
extern int get_line_unread(/*char *group, int groupnum*/);
extern void print_seq(/*FILE *fp, int groupnum*/);
extern int pos_group_in_newsrc(/*char *group, int pos*/);
extern void mark_all_xref_read(/*char *xref_line*/);
/* nntplib.c */
extern char *getserverbyfile(/*char *file*/);
extern int server_init(/*char *machine, char *service, int port*/);
extern int get_tcp_socket(/*char *machine, char *service, int port*/);
extern int handle_server_response(/*int response, char *nntpserver*/);
extern void put_server(/*char *string*/);
extern int get_server(/*char *string, int size*/);
extern void close_server(/*void*/);
/* open.c */
extern void nntp_open(/*void*/);
extern void nntp_close(/*void*/);
extern FILE *open_mail_active_fp(/*char *mode*/);
extern FILE *open_news_active_fp(/*void*/);
extern FILE *open_newgroups_fp(/*int index*/);
extern FILE *open_motd_fp(/*char *motd_file_date*/);
extern FILE *open_subscription_fp(/*void*/);
extern FILE *open_mailgroups_fp(/*void*/);
extern FILE *open_newsgroups_fp(/*void*/);
extern FILE *open_index_fp(/*char *group_name*/);
extern int stat_article(/*long art, char *group_path*/);
extern FILE *open_art_fp(/*char *group_path, long art*/);
extern FILE *open_header_fp(/*long art*/);
extern int base_comp(/*char *p1, char *p2*/);
extern void setup_base(/*char *group, char *group_path*/);
extern int get_respcode(/*void*/);
extern int stuff_nntp(/*char *fnam*/);
extern FILE *nntp_to_fp(/*void*/);
extern void log_user(/*void*/);
extern void authorization(/*char *server, char *authuser*/);
extern char *nntp_respcode(/*int respcode*/);
/* page.c */
extern int show_page(/*int respnum, int *threadnum, char *group, char *group_path*/);
extern void redraw_page(/*int respnum, char *group*/);
extern void show_note_page(/*int respnum, char *group*/);
extern void show_first_header(/*int respnum, char *group*/);
extern void show_cont_header(/*int respnum*/);
extern int art_open(/*long art, char *group_path*/);
extern void art_close(/*void*/);
extern int prompt_response(/*int ch, int respnum*/);
extern void yank_to_addr(/*char *orig, char *addr*/);
extern int show_last_page(/*void*/);
extern int match_header(/*char *buf, char *pat, char *body, int len*/);
/* parsedate.y */
extern int GetTimeInfo(/*TIMEINFO *Now*/);
extern time_t parsedate(/*char *p, TIMEINFO *now*/);
/* post.c */
extern int user_posted_messages(/*void*/);
extern void update_art_posted_file(/*char *group, int action, char *subj*/);
extern int post_header_ok(/*char *article*/);
extern void quick_post_article(/*void*/);
extern int post_article(/*char *group, int *posted*/);
extern int post_response(/*char *group, int respnum, int copy_text*/);
extern int mail_to_someone(/*int respnum, char *address, int mail_to_poster, int confirm_to_mail, int *mailed_ok*/);
extern int mail_bug_report(/*void*/);
extern int mail_to_author(/*char *group, int respnum, int copy_text*/);
extern void find_mail_header(/*int header, char *file, char *value*/);
extern int delete_article(/*char *group, int respnum*/);
extern int crosspost_article(/*char *group, int respnum*/);
extern int submit_file(/*char *name*/);
extern void insert_x_headers(/*char *infile*/);
extern void find_reply_to_addr(/*int respnum, char *from_addr*/);
extern void reread_active_file_after_posting(/*void*/);
/* prompt.c */
extern int prompt_num(/*int ch, char *prompt*/);
extern int prompt_string(/*char *prompt, char *buf*/);
extern int prompt_menu_string(/*int line, int col, char *var*/);
extern int prompt_yn(/*int line, char *prompt, int prompt_ch*/);
extern void prompt_on_off(/*int row, int col, int *var, char *help_text, char *prompt_text*/);
extern void continue_prompt(/*void*/);
/* rcfile.c */
extern int read_rcfile(/*void*/);
extern void write_rcfile(/*void*/);
extern int change_rcfile(/*char *group, int kill_at_once*/);
extern void show_rcfile_menu(/*void*/);
extern void expand_rel_abs_pathname(/*int line, int col, char *str*/);
extern void show_menu_help(/*char *help_message*/);
extern int match_boolean(/*char *line, char *pat, int *dst*/);
extern int match_number(/*char *line, char *pat, int *dst*/);
extern int match_string(/*char *line, char *pat, char *dst, int dstlen*/);
extern void quote_dash_to_space(/*char *s*/);
extern char *quote_space_to_dash(/*char *s*/);
/* save.c */
extern int check_start_save_any_news(/*int check_start_save*/);
extern int save_art_to_file(/*int respnum, int index, int mailbox, char *filename*/);
extern int save_thread_to_file(/*int is_mailbox, char *group_path*/);
extern int save_regex_arts(/*int is_mailbox, char *group_path*/);
extern int append_to_existing_file(/*int i*/);
extern int create_path(/*char *path*/);
extern int create_sub_dir(/*int i*/);
extern void add_to_save_list(/*int index, struct article_t *article, int is_mailbox, int archive_save, char *path*/);
extern void sort_save_list(/*void*/);
extern int save_comp(/*char *p1, char *p2*/);
extern char *save_filename(/*int i*/);
extern char *get_first_savefile(/*void*/);
extern char *get_last_savefile(/*void*/);
extern int post_process_files(/*int proc_type_ch*/);
extern void post_process_uud(/*int pp*/);
extern void post_process_sh(/*void*/);
extern char *get_archive_file(/*char *dir, char *ext*/);
extern void delete_processed_files(/*void*/);
extern void print_art_seperator_line(/*FILE *fp, int mailbox*/);
/* screen.c */
extern void info_message(/*char *str*/);
extern void wait_message(/*char *str*/);
extern void error_message(/*char *template, char *str*/);
extern void perror_message(/*char *template, char *str*/);
extern void clear_message(/*void*/);
extern void center_line(/*int line, int inverse, char *str*/);
extern void draw_arrow(/*int line*/);
extern void erase_arrow(/*int line*/);
extern void show_title(/*char *title*/);
extern void ring_bell(/*void*/);
/* search.c */
extern int search_author(/*int index, int current_art, int forward*/);
extern void search_group(/*int forward*/);
extern void search_subject(/*int forward, char *group*/);
extern int search_article(/*int forward*/);
extern void make_lower(/*char *s, char *t*/);
/* select.c */
extern void selection_index(/*int start_groupnum*/);
extern void group_selection_page(/*void*/);
extern int prompt_group_num(/*int ch*/);
extern void erase_group_arrow(/*void*/);
extern void draw_group_arrow(/*void*/);
extern void yank_active_file(/*void*/);
extern int choose_new_group(/*void*/);
extern int add_group(/*char *s, int get_unread*/);
extern int reposition_group(/*char *group, int default_num*/);
extern void catchup_group(/*int goto_next_unread_group*/);
extern void next_unread_group(/*int enter_group*/);
extern void set_groupname_len(/*int all_groups*/);
extern void toggle_my_groups(/*int only_unread_groups, char *group*/);
extern void goto_next_group_on_screen(/*void*/);
extern void strip_line(/*char *line, int len*/);
/* sigfile.c */
extern void add_signature(/*FILE *fp, int flag*/);
extern FILE *open_random_sig(/*char *sigdir*/);
extern int thrashdir(/*char *sigdir*/);
/* signal.c */
extern sigtype_t (*sigdisp(/*int sig, sigtype_t (*func)()*/))();
extern void set_signal_handlers(/*void*/);
extern void set_alarm_signal(/*void*/);
extern void set_alarm_clock_on(/*void*/);
extern void set_alarm_clock_off(/*void*/);
extern void signal_handler(/*int sig*/);
extern int set_win_size(/*int *num_lines, int *num_cols*/);
extern void set_signals_art(/*void*/);
extern void set_signals_group(/*void*/);
extern void set_signals_help(/*void*/);
extern void set_signals_page(/*void*/);
extern void set_signals_select(/*void*/);
extern void set_signals_spooldir(/*void*/);
extern void set_signals_thread(/*void*/);
extern void art_suspend(/*int sig*/);
extern void main_suspend(/*int sig*/);
extern void select_suspend(/*int sig*/);
extern void spooldir_suspend(/*int sig*/);
extern void group_suspend(/*int sig*/);
extern void help_suspend(/*int sig*/);
extern void page_suspend(/*int sig*/);
extern void thread_suspend(/*int sig*/);
extern void rcfile_suspend(/*int sig*/);
extern void art_resize(/*int sig*/);
extern void main_resize(/*int sig*/);
extern void select_resize(/*int sig*/);
extern void spooldir_resize(/*int sig*/);
extern void group_resize(/*int sig*/);
extern void help_resize(/*int sig*/);
extern void page_resize(/*int sig*/);
extern void thread_resize(/*int sig*/);
/* spooldir.c */
extern int spooldir_index(/*void*/);
extern void show_spooldir_page(/*void*/);
extern int prompt_spooldir_num(/*int ch*/);
extern void erase_spooldir_arrow(/*void*/);
extern void draw_spooldir_arrow(/*void*/);
extern int load_spooldirs(/*void*/);
extern void get_spooldir(/*void*/);
extern int set_spooldir(/*char *name*/);
/* strftime.c */
extern size_t my_strftime(/*char *s, size_t maxsize, char *format, struct tm *timeptr*/);
/* thread.c */
extern int show_thread(/*int respnum, char *group, char *group_path*/);
extern void show_thread_page(/*void*/);
extern void update_thread_page(/*void*/);
extern void draw_thread_arrow(/*void*/);
extern void erase_thread_arrow(/*void*/);
extern int prompt_thread_num(/*int ch*/);
extern int new_responses(/*int thread*/);
extern int which_thread(/*int n*/);
extern int which_response(/*int n*/);
extern int num_of_responses(/*int n*/);
extern int stat_thread(/*int n, struct art_stat_t *sbuf*/);
extern int next_response(/*int n*/);
extern int next_thread(/*int n*/);
extern int prev_response(/*int n*/);
extern int choose_response(/*int i, int n*/);
extern int next_unread(/*int n*/);
extern int prev_unread(/*int n*/);
/* wildmat.c */
extern int wildmat(/*char *text, char *p*/);
 
#endif
