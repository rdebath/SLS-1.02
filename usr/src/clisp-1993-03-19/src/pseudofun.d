# Liste aller Pseudofunktionen
# Bruno Haible 3.2.1993

# Der Macro PSEUDOFUN deklariert eine Pseudofunktion.
# PSEUDOFUN(fun)
# > fun: C-Funktion

# Expander für die Deklaration der Tabelle:
  #define PSEUDOFUN_A(fun)  Pseudofun pseudo_##fun;

# Expander für die Initialisierung der Tabelle:
  #define PSEUDOFUN_B(fun)  (Pseudofun)(&fun),

# Welcher Expander benutzt wird, muß vom Hauptfile aus eingestellt werden.

PSEUDOFUN(rd_by_dummy) PSEUDOFUN(wr_by_dummy) PSEUDOFUN(rd_ch_dummy) PSEUDOFUN(wr_ch_dummy)
PSEUDOFUNSS(wr_ss_dummy) PSEUDOFUNSS(wr_ss_dummy_nogc)
#ifdef HANDLES
PSEUDOFUN(rd_ch_handle) PSEUDOFUN(wr_ch_handle) PSEUDOFUNSS(wr_ss_handle) PSEUDOFUN(rd_by_handle) PSEUDOFUN(wr_by_handle)
#endif
#ifdef KEYBOARD
PSEUDOFUN(rd_ch_keyboard)
#endif
#ifdef ATARI
PSEUDOFUN(wr_ch_terminal) PSEUDOFUN(rd_ch_terminal)
#endif
#if (defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(AMIGAOS))
PSEUDOFUN(wr_ch_terminal1) PSEUDOFUN(rd_ch_terminal1) PSEUDOFUNSS(wr_ss_terminal1)
#ifdef MSDOS
PSEUDOFUN(wr_ch_terminal2) PSEUDOFUN(rd_ch_terminal2) PSEUDOFUNSS(wr_ss_terminal2)
#endif
#ifdef GNU_READLINE
PSEUDOFUN(wr_ch_terminal3) PSEUDOFUN(rd_ch_terminal3) PSEUDOFUNSS(wr_ss_terminal3)
#endif
#endif
#ifdef WINDOWS
PSEUDOFUN(wr_ch_window)
#endif
PSEUDOFUN(rd_ch_sch_file) PSEUDOFUN(wr_ch_sch_file) PSEUDOFUNSS(wr_ss_sch_file)
PSEUDOFUN(rd_ch_ch_file) PSEUDOFUN(wr_ch_ch_file)
PSEUDOFUN(rd_by_iau_file) PSEUDOFUN(wr_by_iau_file)
PSEUDOFUN(rd_by_ias_file) PSEUDOFUN(wr_by_ias_file)
PSEUDOFUN(rd_by_ibu_file) PSEUDOFUN(wr_by_ibu_file)
PSEUDOFUN(rd_by_ibs_file) PSEUDOFUN(wr_by_ibs_file)
PSEUDOFUN(rd_by_icu_file) PSEUDOFUN(wr_by_icu_file)
PSEUDOFUN(rd_by_ics_file) PSEUDOFUN(wr_by_ics_file)
PSEUDOFUN(rd_by_synonym) PSEUDOFUN(wr_by_synonym) PSEUDOFUN(rd_ch_synonym) PSEUDOFUN(wr_ch_synonym) PSEUDOFUNSS(wr_ss_synonym)
PSEUDOFUN(wr_by_broad) PSEUDOFUN(wr_ch_broad) PSEUDOFUNSS(wr_ss_broad)
PSEUDOFUN(rd_by_concat) PSEUDOFUN(rd_ch_concat)
PSEUDOFUN(rd_by_twoway) PSEUDOFUN(wr_by_twoway) PSEUDOFUN(rd_ch_twoway) PSEUDOFUN(wr_ch_twoway) PSEUDOFUNSS(wr_ss_twoway)
PSEUDOFUN(rd_by_echo) PSEUDOFUN(rd_ch_echo)
PSEUDOFUN(rd_ch_str_in)
PSEUDOFUN(wr_ch_str_out) PSEUDOFUNSS(wr_ss_str_out)
PSEUDOFUN(wr_ch_str_push)
PSEUDOFUN(wr_ch_pphelp) PSEUDOFUNSS(wr_ss_pphelp)
PSEUDOFUN(rd_ch_buff_in)
PSEUDOFUN(wr_ch_buff_out)
#ifdef PRINTER
PSEUDOFUN(wr_ch_printer)
#endif
#ifdef PIPES
PSEUDOFUN(rd_ch_pipe_in)
PSEUDOFUN(wr_ch_pipe_out) PSEUDOFUNSS(wr_ss_pipe_out)
#endif
#ifdef SOCKETS
PSEUDOFUN(rd_ch_socket) PSEUDOFUN(wr_ch_socket) PSEUDOFUNSS(wr_ss_socket) PSEUDOFUN(rd_by_socket) PSEUDOFUN(wr_by_socket)
#endif

