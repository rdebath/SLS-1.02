/* Konversionsprogramm SUN4-Zeichensatz -> IBMPC-Zeichensatz */
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
#define _ tabelle[sun4]=ibmpc;
  { int i;
    for (i=0;i<32;i++) { SUN4(i) IBMPC(i) _ }
  }
  { int i;
    for (i=32;i<128;i++) { SUN4(i) IBMPC(i) _ }
  }
  { int i;
    for (i=0;i<32;i++) { SUN4(128+i) IBMPC(i) _ }
  }
  SUN4(15) IBMPC(-1) _
  SUN4(20) IBMPC(-1) _
  SUN4(21) IBMPC(-1) _
  SUN4(143) IBMPC(-1) _
  SUN4(148) IBMPC(-1) _
  SUN4(149) IBMPC(-1) _
  SUN4(160) IBMPC(32) _ /*   */
  SUN4(161) IBMPC(173) _ /* ¡ */
  SUN4(162) IBMPC(155) _ /* ¢ */
  SUN4(163) IBMPC(156) _ /* £ */
  SUN4(164) IBMPC(15) _ /*  */
  SUN4(165) IBMPC(157) _ /* ¥ */
  SUN4(166) IBMPC(124) _ /* | */
  SUN4(167) IBMPC(21) _ /* § */
  SUN4(168) IBMPC(34) _ /* ¨ */
  SUN4(169) IBMPC(-1) _ /* © */
  SUN4(170) IBMPC(166) _ /* ª */
  SUN4(171) IBMPC(174) _ /* « */
  SUN4(172) IBMPC(170) _ /* ¬ */
  SUN4(173) IBMPC(45) _ /* - */
  SUN4(174) IBMPC(-1) _ /* ® */
  SUN4(175) IBMPC(-1) _ /* ¯ */
  SUN4(176) IBMPC(248) _ /* ° */
  SUN4(177) IBMPC(241) _ /* ± */
  SUN4(178) IBMPC(253) _ /* ² */
  SUN4(179) IBMPC(-1) _ /* ³ */
  SUN4(180) IBMPC(39) _ /* ´ */
  SUN4(181) IBMPC(230) _ /* µ */
  SUN4(182) IBMPC(20) _ /* ¶ */
  SUN4(183) IBMPC(250) _ /*  */
  SUN4(184) IBMPC(44) _ /* , */
  SUN4(185) IBMPC(-1) _ /*  */
  SUN4(186) IBMPC(167) _ /* º */
  SUN4(187) IBMPC(175) _ /* » */
  SUN4(188) IBMPC(172) _ /* ¼ */
  SUN4(189) IBMPC(171) _ /* ½ */
  SUN4(190) IBMPC(-1) _ /*  */
  SUN4(191) IBMPC(168) _ /* ¿ */
  SUN4(192) IBMPC(65) _ /* À */
  SUN4(193) IBMPC(-1) _ /*  */
  SUN4(194) IBMPC(-1) _ /*  */
  SUN4(195) IBMPC(-1) _ /* Ã */
  SUN4(196) IBMPC(142) _ /* Ä */
  SUN4(197) IBMPC(143) _ /* Å */
  SUN4(198) IBMPC(146) _ /* Æ */
  SUN4(199) IBMPC(128) _ /* Ç */
  SUN4(200) IBMPC(-1) _ /*  */
  SUN4(201) IBMPC(144) _ /* É */
  SUN4(202) IBMPC(-1) _ /*  */
  SUN4(203) IBMPC(-1) _ /*  */
  SUN4(204) IBMPC(-1) _ /*  */
  SUN4(205) IBMPC(-1) _ /*  */
  SUN4(206) IBMPC(-1) _ /*  */
  SUN4(207) IBMPC(-1) _ /*  */
  SUN4(208) IBMPC(-1) _ /*  */
  SUN4(209) IBMPC(165) _ /* Ñ */
  SUN4(210) IBMPC(-1) _ /*  */
  SUN4(211) IBMPC(-1) _ /*  */
  SUN4(212) IBMPC(-1) _ /*  */
  SUN4(213) IBMPC(-1) _ /* Õ */
  SUN4(214) IBMPC(153) _ /* Ö */
  SUN4(215) IBMPC(-1) _ /*  */
  SUN4(216) IBMPC(-1) _ /* Ø */
  SUN4(217) IBMPC(-1) _ /*  */
  SUN4(218) IBMPC(-1) _ /*  */
  SUN4(219) IBMPC(-1) _ /*  */
  SUN4(220) IBMPC(154) _ /* Ü */
  SUN4(221) IBMPC(-1) _ /*  */
  SUN4(222) IBMPC(-1) _ /*  */
  SUN4(223) IBMPC(225) _ /* ß */
  SUN4(224) IBMPC(133) _ /* à */
  SUN4(225) IBMPC(160) _ /* á */
  SUN4(226) IBMPC(131) _ /* â */
  SUN4(227) IBMPC(-1) _ /* ã */
  SUN4(228) IBMPC(132) _ /* ä */
  SUN4(229) IBMPC(134) _ /* å */
  SUN4(230) IBMPC(145) _ /* æ */
  SUN4(231) IBMPC(135) _ /* ç */
  SUN4(232) IBMPC(138) _ /* è */
  SUN4(233) IBMPC(130) _ /* é */
  SUN4(234) IBMPC(136) _ /* ê */
  SUN4(235) IBMPC(137) _ /* ë */
  SUN4(236) IBMPC(141) _ /* ì */
  SUN4(237) IBMPC(161) _ /* í */
  SUN4(238) IBMPC(140) _ /* î */
  SUN4(239) IBMPC(139) _ /* ï */
  SUN4(240) IBMPC(-1) _ /*  */
  SUN4(241) IBMPC(164) _ /* ñ */
  SUN4(242) IBMPC(149) _ /* ò */
  SUN4(243) IBMPC(162) _ /* ó */
  SUN4(244) IBMPC(147) _ /* ô */
  SUN4(245) IBMPC(-1) _ /* õ */
  SUN4(246) IBMPC(148) _ /* ö */
  SUN4(247) IBMPC(246) _ /* ÷ */
  SUN4(248) IBMPC(-1) _ /* ø */
  SUN4(249) IBMPC(151) _ /* ù */
  SUN4(250) IBMPC(163) _ /* ú */
  SUN4(251) IBMPC(150) _ /* û */
  SUN4(252) IBMPC(129) _ /* ü */
  SUN4(253) IBMPC(-1) _ /*  */
  SUN4(254) IBMPC(-1) _ /*  */
  SUN4(255) IBMPC(152) _ /* ÿ */
#undef _
#undef SUN4
#undef IBMPC
#ifdef __EMX__
  setmode(0,O_BINARY);
  setmode(1,O_TEXT);
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
