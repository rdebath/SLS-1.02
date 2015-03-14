/* Konversionsprogramm IBMPC-Zeichensatz -> SUN4-Zeichensatz */
/* Bruno Haible 15.1.1992 */

#include <stdio.h>
#ifdef __EMX__
#include <fcntl.h>
#endif

main ()
{ static int tabelle[256];
  /* Tabelle initialisieren: */
  int ibmpc, sun4;
#define IBMPC(x) ibmpc=x;
#define SUN4(y) sun4=y;
#define _ tabelle[ibmpc]=sun4;
  { int i;
    for (i=0;i<128;i++) { SUN4(i) IBMPC(i) _ }
  }
  SUN4(164) IBMPC(15) _ /*  */
  SUN4(182) IBMPC(20) _ /* ¶ */
  SUN4(167) IBMPC(21) _ /* § */
  SUN4(199) IBMPC(128) _ /* Ç */
  SUN4(252) IBMPC(129) _ /* ü */
  SUN4(233) IBMPC(130) _ /* é */
  SUN4(226) IBMPC(131) _ /* â */
  SUN4(228) IBMPC(132) _ /* ä */
  SUN4(224) IBMPC(133) _ /* à */
  SUN4(229) IBMPC(134) _ /* å */
  SUN4(231) IBMPC(135) _ /* ç */
  SUN4(234) IBMPC(136) _ /* ê */
  SUN4(235) IBMPC(137) _ /* ë */
  SUN4(232) IBMPC(138) _ /* è */
  SUN4(239) IBMPC(139) _ /* ï */
  SUN4(238) IBMPC(140) _ /* î */
  SUN4(236) IBMPC(141) _ /* ì */
  SUN4(196) IBMPC(142) _ /* Ä */
  SUN4(197) IBMPC(143) _ /* Å */
  SUN4(201) IBMPC(144) _ /* É */
  SUN4(230) IBMPC(145) _ /* æ */
  SUN4(198) IBMPC(146) _ /* Æ */
  SUN4(244) IBMPC(147) _ /* ô */
  SUN4(246) IBMPC(148) _ /* ö */
  SUN4(242) IBMPC(149) _ /* ò */
  SUN4(251) IBMPC(150) _ /* û */
  SUN4(249) IBMPC(151) _ /* ù */
  SUN4(255) IBMPC(152) _ /* ÿ */
  SUN4(214) IBMPC(153) _ /* Ö */
  SUN4(220) IBMPC(154) _ /* Ü */
  SUN4(162) IBMPC(155) _ /* ¢ */
  SUN4(163) IBMPC(156) _ /* £ */
  SUN4(165) IBMPC(157) _ /* ¥ */
  SUN4(-1) IBMPC(158) _ /* Pt */
  SUN4(-1) IBMPC(159) _ /*  */
  SUN4(225) IBMPC(160) _ /* á */
  SUN4(237) IBMPC(161) _ /* í */
  SUN4(243) IBMPC(162) _ /* ó */
  SUN4(250) IBMPC(163) _ /* ú */
  SUN4(241) IBMPC(164) _ /* ñ */
  SUN4(209) IBMPC(165) _ /* Ñ */
  SUN4(170) IBMPC(166) _ /* ª */
  SUN4(186) IBMPC(167) _ /* º */
  SUN4(191) IBMPC(168) _ /* ¿ */
  SUN4(-1) IBMPC(169) _ /*  */
  SUN4(172) IBMPC(170) _ /* ¬ */
  SUN4(189) IBMPC(171) _ /* ½ */
  SUN4(188) IBMPC(172) _ /* ¼ */
  SUN4(161) IBMPC(173) _ /* ¡ */
  SUN4(171) IBMPC(174) _ /* « */
  SUN4(187) IBMPC(175) _ /* » */
  { int i;
    for (i=176;i<224;i++) { SUN4(-1) IBMPC(i) _ }
  }
  SUN4(-1) IBMPC(224) _ /*  */
  SUN4(223) IBMPC(225) _ /* ß */
  SUN4(-1) IBMPC(226) _ /*  */
  SUN4(-1) IBMPC(227) _ /*  */
  SUN4(-1) IBMPC(228) _ /*  */
  SUN4(-1) IBMPC(229) _ /*  */
  SUN4(181) IBMPC(230) _ /* µ */
  SUN4(-1) IBMPC(231) _ /*  */
  SUN4(-1) IBMPC(232) _ /*  */
  SUN4(-1) IBMPC(233) _ /*  */
  SUN4(-1) IBMPC(234) _ /*  */
  SUN4(-1) IBMPC(235) _ /*  */
  SUN4(-1) IBMPC(236) _ /*  */
  SUN4(-1) IBMPC(237) _ /*  */
  SUN4(-1) IBMPC(238) _ /*  */
  SUN4(-1) IBMPC(239) _ /*  */
  SUN4(-1) IBMPC(240) _ /*  */
  SUN4(177) IBMPC(241) _ /* ± */
  SUN4(-1) IBMPC(242) _ /*  */
  SUN4(-1) IBMPC(243) _ /*  */
  SUN4(-1) IBMPC(244) _ /*  */
  SUN4(-1) IBMPC(245) _ /*  */
  SUN4(247) IBMPC(246) _ /* ÷ */
  SUN4(-1) IBMPC(247) _ /*  */
  SUN4(176) IBMPC(248) _ /* ° */
  SUN4(-1) IBMPC(249) _ /*  */
  SUN4(183) IBMPC(250) _ /*  */
  SUN4(-1) IBMPC(251) _ /*  */
  SUN4(-1) IBMPC(252) _ /*  */
  SUN4(178) IBMPC(253) _ /* ² */
  SUN4(-1) IBMPC(254) _ /*  */
  SUN4(-1) IBMPC(255) _ /*  */
#undef _
#undef SUN4
#undef IBMPC
#ifdef __EMX__
  setmode(0,O_TEXT);
  setmode(1,O_BINARY);
#endif
  { int fehler = 0;
    int c;
    while (!((c = getchar()) == EOF))
      { c = tabelle[c];
        if (c < 0) { fehler++; } else putchar(c);
      }
    if (!(fehler == 0))
      { fprintf(stderr,"%d illegal characters\n",fehler); return 1; }
      else
      { return 0; }
} }
