#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>

time_t time();

#include "vier.h"
#include "xvier.h"

int rows, columns, vnum;
int row_col, row_1_col, row_2_col;
int *brett, *weiss, *schwarz, **freip, **doublesp;
int frei[MAXRC], reihenfolge[MAXRC], doubles[MAXRC];
int (*pu)[4];
int *_p_h_, **pp;
struct oldv *stack, *sp, **zugstack;
int bewertung = 0;

int zugstackp = 0, level = 2;

#define WEISS    1
#define SCHWARZ  2
#define W_WINS   4
#define S_WINS   8
#define USELESS 16

char read_char(ch)
char *ch;
{
  if (read(0, ch, 1) < 1) {
    perror("xvier_prog read failed");
    exit(1);
  }
  return *ch;
}

void write_char(ch)
char ch;
{
  if (write(1, &ch, 1) < 1) {
    perror("xvier_prog write failed");
    exit(1);
  }
}

void w_test(pos)
int pos;
{
  register int *p, j;
  int oben;

  /* leere Position suchen */
  for (p = pu[pos]; brett[*p] & (WEISS | SCHWARZ); p++);
  if (brett[j = *p] & (W_WINS | USELESS))
    return;
  sp -> pos = brett + j;
  sp++ -> value = brett[j];
  brett[j] |= W_WINS;
  if (*doublesp[j] == SCHWARZ &&
      j > *freip[j] &&
      j < row_1_col &&
      !(brett[j+columns] & USELESS)) {
    sp -> pos = doublesp[j];
    sp++ -> value = SCHWARZ;
    *doublesp[j] = 0;
  }
  if (!*doublesp[j] &&
      ((j > *freip[j] + columns &&
	(brett[(oben = j) - columns] & W_WINS)) ||
       (j > *freip[j] && j < row_1_col &&
	(brett[oben = j + columns] & W_WINS)))) {
    register int k;

    for (k = *freip[j]; k < oben; k += columns)
      if (brett[k] & S_WINS)
	goto no_double;
    sp -> pos = doublesp[j];
    sp++ -> value = 0;
    *doublesp[j] = WEISS;
  }
 no_double:
  if (j < row_1_col &&
      ((brett[oben = j] & S_WINS) ||
       (j > *freip[j] && (brett[j - columns] & W_WINS)) ||
       (j < row_2_col &&
	(brett[oben = j + columns] & W_WINS))))
    for (j = oben + columns;
	 j < row_col && !(brett[j] & USELESS);
	 j += columns) {
      register int h1, h2;

      /* Vierer darueber sind nutzlos */
      sp -> pos = brett + j;
      sp++ -> value = brett[j];
      brett[j] |= USELESS;
      p = pp[j];
      while ((h1 = *(p++)) >= 0) {
	if ((h2 = weiss[h1]) >= 0) {
	  sp -> pos = &weiss[h1];
	  sp++ -> value = h2;
	  weiss[h1] = -1;
	  bewertung -= h2;
	}
	if ((h2 = schwarz[h1]) >= 0) {
	  sp -> pos = &schwarz[h1];
	  sp++ -> value = h2;
	  schwarz[h1] = -1;
	  bewertung += h2;
	}
      }
    }
}
 
void s_test(pos)
int pos;
{
  register int *p, j;
  int oben;

  /* leere Position suchen */
  for (p = pu[pos]; brett[*p] & (WEISS | SCHWARZ); p++);
  if (brett[j = *p] & (S_WINS | USELESS))
    return;
  sp -> pos = brett + j;
  sp++ -> value = brett[j];
  brett[j] |= S_WINS;
  if (*doublesp[j] == WEISS &&
      j > *freip[j] &&
      j < row_1_col &&
      !(brett[j+columns] & USELESS)) {
    sp -> pos = doublesp[j];
    sp++ -> value = WEISS;
    *doublesp[j] = 0;
  }
  if (!*doublesp[j] &&
      ((j > *freip[j] + columns &&
	(brett[(oben = j) - columns] & S_WINS)) ||
       (j > *freip[j] && j < row_1_col &&
	(brett[oben = j + columns] & S_WINS)))) {
    register int k;

    for (k = *freip[j]; k < oben; k += columns)
      if (brett[k] & W_WINS)
	goto no_double;
    sp -> pos = doublesp[j];
    sp++ -> value = 0;
    *doublesp[j] = SCHWARZ;
  }
 no_double:
  if (j < row_1_col &&
      ((brett[oben = j] & W_WINS) ||
       (j > *freip[j] && (brett[j - columns] & S_WINS)) ||
       (j < row_2_col &&
	(brett[oben = j + columns] & S_WINS))))
    for (j = oben + columns;
	 j < row_col && !(brett[j] & USELESS);
	 j += columns) {
      register int h1, h2;

      /* Vierer darueber sind nutzlos */
      sp -> pos = brett + j;
      sp++ -> value = brett[j];
      brett[j] |= USELESS;
      p = pp[j];
      while ((h1 = *(p++)) >= 0) {
	if ((h2 = weiss[h1]) >= 0) {
	  sp -> pos = &weiss[h1];
	  sp++ -> value = h2;
	  weiss[h1] = -1;
	  bewertung -= h2;
	}
	if ((h2 = schwarz[h1]) >= 0) {
	  sp -> pos = &schwarz[h1];
	  sp++ -> value = h2;
	  schwarz[h1] = -1;
	  bewertung += h2;
	}
      }
    }
}

int w_zugzwang(remain)
int remain;
{
  register int i, pos, poslev = 7, p_s_p = 0;
  int p_stack[MAXRC];

  for (i = 0; i < columns; i++) {
    register int f = frei[i];

    if (f < row_col) {
      if (brett[f] & W_WINS)
	return 8 + 8 * remain;
      if (brett[f] & S_WINS) {
	pos = i;
	poslev = 1;
      } else if (poslev > 2) {
	if (doubles[i] == WEISS) {
	  pos = i;
	  poslev = 2;
	} else if (poslev > 3) {
	  if (f >= row_1_col ||
	      ((brett[f + columns] & (W_WINS | S_WINS)) == 0 &&
	       !doubles[i])) {
	    pos = i;
	    poslev = 3;
	  } else if (poslev > 4) {
	    if ((brett[f + columns] & (W_WINS | S_WINS)) == 0) {
	      pos = i;
	      poslev = 4;
	    } else if ((brett[f + columns] & S_WINS) == 0) {
	      p_stack[p_s_p++] = pos = i;
	      poslev = 5;
	    } else if (poslev > 6) {
	      pos = i;
	      poslev = 6;
	    }
	  }
	}
      }
    }
  }
  if (poslev == 7)
    return 0;
  if (poslev == 5 && p_s_p > 1) {
    int m;

    frei[p_stack[0]] += columns;
    m = s_zugzwang(remain-1);
    frei[p_stack[0]] -= columns;
    for (i = 1; i < p_s_p; i++) {
      register int tmp;

      frei[p_stack[i]] += columns;
      if ((tmp = s_zugzwang(remain-1)) > m)
	m = tmp;
      frei[p_stack[i]] -= columns;
    }
    return m;
  } else {
    frei[pos] += columns;
    i = s_zugzwang(remain-1);
    frei[pos] -= columns;
    return i;
  }
}

int s_zugzwang(remain)
int remain;
{
  register int i, pos, poslev = 7, p_s_p = 0;
  int p_stack[MAXRC];

  for (i = 0; i < columns; i++) {
    register int f = frei[i];

    if (f < row_col) {
      if (brett[f] & S_WINS)
	return -8 - 8 * remain;
      if (brett[f] & W_WINS) {
	pos = i;
	poslev = 1;
      } else if (poslev > 2) {
	if (doubles[i] == SCHWARZ) {
	  pos = i;
	  poslev = 2;
	} else if (poslev > 3) {
	  if (f >= row_1_col ||
	      ((brett[f + columns] & (W_WINS | S_WINS)) == 0 &&
	       !doubles[i])) {
	    pos = i;
	    poslev = 3;
	  } else if (poslev > 4) {
	    if ((brett[f + columns] & (W_WINS | S_WINS)) == 0) {
	      pos = i;
	      poslev = 4;
	    } else if ((brett[f + columns] & W_WINS) == 0) {
	      p_stack[p_s_p++] = pos = i;
	      poslev = 5;
	    } else if (poslev > 6) {
	      pos = i;
	      poslev = 6;
	    }
	  }
	}
      }
    }
  }
  if (poslev == 7)
    return 0;
  if (poslev == 5 && p_s_p > 1) {
    int m;

    frei[p_stack[0]] += columns;
    m = w_zugzwang(remain-1);
    frei[p_stack[0]] -= columns;
    for (i = 1; i < p_s_p; i++) {
      register int tmp;

      frei[p_stack[i]] += columns;
      if ((tmp = w_zugzwang(remain-1)) < m)
	m = tmp;
      frei[p_stack[i]] -= columns;
    }
    return m;
  } else {
    frei[pos] += columns;
    i = w_zugzwang(remain-1);
    frei[pos] -= columns;
    return i;
  }
}

int comp_weiss(pos, lev, limit)
int pos, lev, limit;
{
  register int  h1, h2, i, j, *p;
  int   *frp, wert;
  struct oldv *sold;

  if (brett[pos] & W_WINS)
    return 50000 - lev;
  /* Zug fuer Weiss ausfuehren */
  sold = sp;
  sp -> pos = frp = freip[pos];
  sp++ -> value = *frp;
  sp -> pos = brett + pos;
  sp++ -> value = brett[pos];
  sp -> pos = &bewertung;
  sp++ -> value = bewertung;
  *frp += columns;
  brett[pos] |= WEISS;
  p = pp[pos];
  while ((h1 = *(p++)) >= 0) {
    if ((h2 = weiss[h1]) >= 0) {
      sp -> pos = &weiss[h1];
      sp++ -> value = h2;
      weiss[h1]++;
      bewertung++;
      if (h2 == 3)
	w_test (h1);
    }
    if ((h2 = schwarz[h1]) >= 0) {
      sp -> pos = &schwarz[h1];
      sp++ -> value = h2;
      schwarz[h1] = -1;
      bewertung += h2;
    }
  }
  h1 = -1;
  for (i = 0; i < columns; i++) {
    register int f = frei[i];

    if (f < row_col) {
      if (brett[f] & S_WINS) {
	wert = -49999 + lev;
	goto end;
      }
      if (brett[f] & W_WINS)
	h1 = f;
    }
  }
  if (h1 >= 0) {
    level++;
    wert = comp_schwarz(h1, lev + 1, 100000);
    level--;
  } else if (lev >= level)
    wert = bewertung + s_zugzwang(40 - lev - zugstackp);
  else {
    register int    zw;

    wert = 100000;
    for (i = 0; i < columns; i++) {
      j = frei[reihenfolge[i]];
      if (j < row_col)
	if ((zw = comp_schwarz (j, lev + 1, wert)) < wert)
	  if ((wert = zw) <= limit)
	    break;
      /* Schwarz wird wohl den fuer ihn guenstigsten Zug auswaehlen */
    }
    if (wert == 100000)
      wert = 0;		/* unentschieden */
  }
 end:
  while (sp != sold) {
    sp--;
    *(sp -> pos) = sp -> value;
  }
  return (wert);
}

int comp_schwarz(pos, lev, limit)
int pos, lev, limit;
{
  register int  h1, h2, i, j, *p;
  int   *frp, wert;
  struct oldv *sold;

  if (brett[pos] & S_WINS)
    return -50000 + lev;
  /* Zug fuer Schwarz ausfuehren */
  sold = sp;
  sp -> pos = frp = freip[pos];
  sp++ -> value = *frp;
  sp -> pos = brett + pos;
  sp++ -> value = brett[pos];
  sp -> pos = &bewertung;
  sp++ -> value = bewertung;
  *frp += columns;
  brett[pos] |= SCHWARZ;
  p = pp[pos];
  while ((h1 = *(p++)) >= 0) {
    if ((h2 = schwarz[h1]) >= 0) {
      sp -> pos = &schwarz[h1];
      sp++ -> value = h2;
      schwarz[h1]++;
      bewertung--;
      if (h2 == 3)
	s_test (h1);
    }
    if ((h2 = weiss[h1]) >= 0) {
      sp -> pos = &weiss[h1];
      sp++ -> value = h2;
      weiss[h1] = -1;
      bewertung -= h2;
    }
  }
  h1 = -1;
  for (i = 0; i < columns; i++) {
    register int f = frei[i];

    if (f < row_col) {
      if (brett[f] & W_WINS) {
	wert = 49999 - lev;
	goto end;
      }
      if (brett[f] & S_WINS)
	h1 = f;
    }
  }
  if (h1 >= 0) {
    level++;
    wert = comp_weiss(h1, lev + 1, -100000);
    level--;
  } else if (lev >= level)
    wert = bewertung + w_zugzwang(40 - lev - zugstackp);
  else {
    register int    zw;

    wert = -100000;
    for (i = 0; i < columns; i++) {
      j = frei[reihenfolge[i]];
      if (j < row_col)
	if ((zw = comp_weiss (j, lev + 1, wert)) > wert)
	  if ((wert = zw) >= limit)
	    break;
    }
    if (wert == -100000)
      wert = 0;		/* unentschieden */
  }
 end:
  while (sp != sold) {
    sp--;
    *(sp -> pos) = sp -> value;
  }
  return (wert);
}

int main(argc, argv)
int argc;
char **argv;
{
  int  i, j, h1, zj, *p, wert, zi;
  char ch, buf[10];
  int  same[MAXRC], same_n;

  if (argc != 3 ||
      (rows = atoi(argv[1])) < 4 || rows > MAXRC ||
      (columns = atoi(argv[2])) < 4 || columns > MAXRC) {
    fprintf(stderr, "Usage: %s <rows> <columns>\n", *argv);
    exit(1);
  }
  vierinit();
  sprintf(buf, "%dR%dC", rows, columns);
  for (i = 0; buf[i] != '\0'; i++)
    write_char(buf[i]);
  srand((int) time(NULL));
  while (1) {
  mensch:
    read_char(&ch);
    switch (ch) {
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      write_char(ch);
      level = ch - '0' + 2;
      break;
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
      if (ch - 'a' >= columns ||
	  (j = frei[ch - 'a']) >= row_col) {
	write_char('x');
	break;
      }
      if (brett[j] & W_WINS) {
	write_char('w');
	while (read_char(&ch) != 'u')
	  switch (ch) {
	  case '0': case '1': case '2': case '3': case '4':
	  case '5': case '6': case '7': case '8': case '9':
	    write_char(ch);
	    level = ch - '0' + 2;
	    break;
	  case 'n':
	    goto newgame;
	  default:
	    write_char('x');
	    break;
	  }
	write_char('v');
	break;
      }
      zugstack[zugstackp++] = sp;
      sp -> pos = brett + j;
      sp++ -> value = brett[j];
      sp -> pos = frei + ch - 'a';
      sp++ -> value = frei[ch - 'a'];
      sp -> pos = &bewertung;
      sp++ -> value = bewertung;
      brett[j] |= WEISS;
      frei[ch - 'a'] += columns;
      p = pp[j];
      while ((h1 = *(p++)) >= 0) {
	if ((weiss[h1]) >= 0) {
	  sp -> pos = weiss + h1;
	  sp++ -> value = weiss[h1];
	  weiss[h1]++;
	  bewertung++;
	  if (weiss[h1] == 4)
	    w_test(h1);
	}
	if ((schwarz[h1]) >= 0) {
	  sp -> pos = schwarz + h1;
	  sp++ -> value = schwarz[h1];
	  bewertung += schwarz[h1];
	  schwarz[h1] = -1;
	}
      }
    computer:
      same_n = 0;
      for (i = 0; i < columns; i++) {
	int f = frei[i];
	
	if (f < row_col) {
	  if (brett[f] & S_WINS) {
	    zi = i;
	    goto calculate_end;
	  }
	  if (brett[f] & W_WINS)
	    same[same_n++] = i;
	}
      }
      if (same_n == 0) {
	wert = 100000;
	for (i = 0; i < columns; i++) {
	  register int zw;
	  
	  if ((j = frei[reihenfolge[i]]) < row_col)
	    if ((zw = comp_schwarz (j, 0, wert + 1)) < wert) {
	      wert = zw;
	      same_n = 1;
	      same[0] = reihenfolge[i];
	    } else if (zw == wert)
	      same[same_n++] = reihenfolge[i];
	}
      }
      if (same_n == 0) {
	write_char('z');
	while (read_char(&ch) != 'u')
	  switch (ch) {
	  case '0': case '1': case '2': case '3': case '4':
	  case '5': case '6': case '7': case '8': case '9':
	    write_char(ch);
	    level = ch - '0' + 2;
	    break;
	  case 'n':
	    goto newgame;
	  default:
	    write_char('x');
	    break;
	  }
	write_char('v');
	zugstackp--;
	while (sp != zugstack[zugstackp]) {
	  sp--;
	  *(sp -> pos) = sp -> value;
	}
	break;
      }
      if (same_n == 1)
	zi = same[0];
      else
	zi = same[rand() % same_n];
    calculate_end:
      zj = frei[zi];
      if (brett[zj] & S_WINS) {
	write_char('A' + zi);
	while (read_char(&ch) != 'u')
	  switch (ch) {
	  case '0': case '1': case '2': case '3': case '4':
	  case '5': case '6': case '7': case '8': case '9':
	    write_char(ch);
	    level = ch - '0' + 2;
	    break;
	  case 'n':
	    goto newgame;
	  default:
	    write_char('x');
	    break;
	  }
	write_char('u');
	zugstackp--;
	while (sp != zugstack[zugstackp]) {
	  sp--;
	  *(sp -> pos) = sp -> value;
	}
	break;
      }
      zugstack[zugstackp++] = sp;
      sp -> pos = brett + zj;
      sp++ -> value = brett[zj];
      sp -> pos = frei + zi;
      sp++ -> value = zj;
      sp -> pos = &bewertung;
      sp++ -> value = bewertung;
      brett[zj] |= SCHWARZ;
      frei[zi] += columns;
      p = pp[zj];
      while ((h1 = *(p++)) >= 0) {
	if ((schwarz[h1]) >= 0) {
	  sp -> pos = schwarz + h1;
	  sp++ -> value = schwarz[h1];
	  schwarz[h1]++;
	  bewertung--;
	  if (schwarz[h1] == 4)
	    s_test(h1);
	}
	if ((weiss[h1]) >= 0) {
	  sp -> pos = weiss + h1;
	  sp++ -> value = weiss[h1];
	  bewertung -= weiss[h1];
	  weiss[h1] = -1;
	}
      }
      if (frei[zi] < row_col && (brett[frei[zi]] & USELESS)) {
	/* user didn't recognize a double */
	for (i = frei[zi]; i < row_col; i+= columns) {
	  int w_wins = 0, s_wins = 0;

	  sp -> pos = brett + i;
	  sp++ -> value = brett[i];
	  brett[i] = 0;
	  p = pp[i];
	  while ((h1 = *(p++)) >= 0) {
	    int wval = 1, sval = 1;

	    for (j = 0; j < 4; j++) {
	      if (brett[pu[h1][j]] & USELESS) {
		wval = -1;
		sval = -1;
	      } else if (brett[pu[h1][j]] & WEISS) {
		if (wval > 0)
		  wval++;
	      } else if (brett[pu[h1][j]] & SCHWARZ) {
		if (sval > 0)
		  sval++;
	      }
	    }
	    if (wval > 0) {
	      bewertung += wval;
	      sp -> pos = weiss + h1;
	      sp++ -> value = weiss[h1];
	      weiss[h1] = wval;
	      if (wval == 4)
		w_wins = 1;
	    }
	    if (sval > 0) {
	      bewertung -= sval;
	      sp -> pos = schwarz + h1;
	      sp++ -> value = schwarz[h1];
	      schwarz[h1] = sval;
	      if (sval == 4)
		s_wins = 1;
	    }
	  }
	  if (w_wins && s_wins) {
	    brett[i] |= (W_WINS | S_WINS);
	    if (i > frei[zi] + columns) {
	      if (brett[i - columns] & W_WINS) {
		if (*doublesp[i] != WEISS) {
		  sp -> pos = doublesp[i];
		  sp++ -> value = *doublesp[i];
		  *doublesp[i] = WEISS;
		}
	      } else if (brett[i - columns] & S_WINS) {
		if (*doublesp[i] != SCHWARZ) {
		  sp -> pos = doublesp[i];
		  sp++ -> value = *doublesp[i];
		  *doublesp[i] = SCHWARZ;
		}
	      }
	    }
	    break;
	  } else if (w_wins) {
	    brett[i] |= W_WINS;
	    if (i > frei[zi] + columns && (brett[i - columns] & W_WINS)) {
	      if (*doublesp[i] != WEISS) {
		sp -> pos = doublesp[i];
		sp++ -> value = *doublesp[i];
		*doublesp[i] = WEISS;
	      }
	      break;
	    }
	  } else if (s_wins) {
	    brett[i] |= S_WINS;
	    if (i > frei[zi] + columns  && (brett[i - columns] & S_WINS)) {
	      if (*doublesp[i] != SCHWARZ) {
		sp -> pos = doublesp[i];
		sp++ -> value = *doublesp[i];
		*doublesp[i] = SCHWARZ;
	      }
	      break;
	    }
	  }
	}
      }
      for (i = 0; i < columns; i++)
	if (frei[i] < row_col) {
	  write_char('a' + zi);
	  goto mensch;
	}
      /* unentschieden */
      write_char('N' + zi);
      while (read_char(&ch) != 'u')
	switch (ch) {
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	  write_char(ch);
	  level = ch - '0' + 2;
	  break;
	case 'n':
	  goto newgame;
	default:
	  write_char('x');
	  break;
	}
      write_char('u');
      zugstackp -= 2;
      while (sp != zugstack[zugstackp]) {
	sp--;
	*(sp -> pos) = sp -> value;
      }
      break;
    case 'n':
    newgame:
      write_char('n');
      zugstackp = 0;
      while (sp != stack) {
	sp--;
	*(sp -> pos) = sp -> value;
      }
      break;
    case 'u':
      if (zugstackp < 2)
	goto newgame;
      write_char('u');
      zugstackp -= 2;
      while (sp != zugstack[zugstackp]) {
	sp--;
	*(sp -> pos) = sp -> value;
      }
      break;
    case 's':
      if (zugstackp == 0)
	goto computer;
      /* else fall through */
    default:
      write_char('x');
      break;
    }
  }
}
