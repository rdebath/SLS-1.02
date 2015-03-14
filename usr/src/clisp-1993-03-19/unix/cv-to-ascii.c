/* Konversionsprogramm ISO-Latin1-Zeichensatz -> ASCII-Zeichensatz */
/* Bruno Haible 11.12.1992 */

#include <stdio.h>

main ()
{ static int tabelle[256];
  /* Tabelle initialisieren: */
  int iso;
  long ascii;
#define ISO(x) iso=x;
#define ASCII(y) ascii=y;
#define ASCII2(y1,y2) ascii=(y2<<8)|y1;
#define ASCII3(y1,y2,y3) ascii=(y3<<16)|(y2<<8)|y1;
#define _ tabelle[iso]=ascii;
  { int i;
    for (i=0;i<128;i++) { ISO(i) ASCII(i) _ }
  }
  { int i;
    for (i=0;i<32;i++) { ISO(128+i) ASCII(i) _ }
  }
  ISO(160) ASCII(' ') _ /*   */
  ISO(161) ASCII('!') _ /* ¡ */
  ISO(162) ASCII('c') _ /* ¢ */
  ISO(163) ASCII2('l','b') _ /* £ */
  ISO(164) ASCII(0) _ /* ¤ */
  ISO(165) ASCII3('y','e','n') _ /* ¥ */
  ISO(166) ASCII('|') _ /* ¦ */
  ISO(167) ASCII2('S','S') _ /* § */
  ISO(168) ASCII('\"') _ /* ¨ */
  ISO(169) ASCII3('(','c',')') _ /* © */
  ISO(170) ASCII('a') _ /* ª */
  ISO(171) ASCII2('<','<') _ /* « */
  ISO(172) ASCII3('n','o','t') _ /* ¬ */
  ISO(173) ASCII('-') _ /* ­ */
  ISO(174) ASCII3('(','R',')') _ /* ® */
  ISO(175) ASCII(0) _ /* ¯ */
  ISO(176) ASCII2('^','0') _ /* ° */
  ISO(177) ASCII2('+','-') _ /* ± */
  ISO(178) ASCII2('^','2') _ /* ² */
  ISO(179) ASCII2('^','3') _ /* ³ */
  ISO(180) ASCII('\'') _ /* ´ */
  ISO(181) ASCII('u') _ /* µ */
  ISO(182) ASCII('P') _ /* ¶ */
  ISO(183) ASCII('.') _ /* · */
  ISO(184) ASCII(',') _ /* ¸ */
  ISO(185) ASCII2('^','1') _ /* ¹ */
  ISO(186) ASCII('o') _ /* º */
  ISO(187) ASCII2('>','>') _ /* » */
  ISO(188) ASCII3('1','/','4') _ /* ¼ */
  ISO(189) ASCII3('1','/','2') _ /* ½ */
  ISO(190) ASCII3('3','/','4') _ /* ¾ */
  ISO(191) ASCII('?') _ /* ¿ */
  ISO(192) ASCII2('`','A') _ /* À */
  ISO(193) ASCII2('\'','A') _ /* Á */
  ISO(194) ASCII2('^','A') _ /* Â */
  ISO(195) ASCII2('~','A') _ /* Ã */
  ISO(196) ASCII2('A','e') _ /* Ä */
  ISO(197) ASCII('A') _ /* Å */
  ISO(198) ASCII2('A','E') _ /* Æ */
  ISO(199) ASCII('C') _ /* Ç */
  ISO(200) ASCII2('`','E') _ /* È */
  ISO(201) ASCII2('\'','E') _ /* É */
  ISO(202) ASCII2('^','E') _ /* Ê */
  ISO(203) ASCII2('\"','E') _ /* Ë */
  ISO(204) ASCII2('`','I') _ /* Ì */
  ISO(205) ASCII2('\'','I') _ /* Í */
  ISO(206) ASCII2('^','I') _ /* Î */
  ISO(207) ASCII2('\"','I') _ /* Ï */
  ISO(208) ASCII('D') _ /* Ð */
  ISO(209) ASCII2('~','N') _ /* Ñ */
  ISO(210) ASCII2('`','O') _ /* Ò */
  ISO(211) ASCII2('\'','O') _ /* Ó */
  ISO(212) ASCII2('^','O') _ /* Ô */
  ISO(213) ASCII2('~','O') _ /* Õ */
  ISO(214) ASCII2('O','e') _ /* Ö */
  ISO(215) ASCII('x') _ /* × */
  ISO(216) ASCII('O') _ /* Ø */
  ISO(217) ASCII2('`','U') _ /* Ù */
  ISO(218) ASCII2('\'','U') _ /* Ú */
  ISO(219) ASCII2('^','U') _ /* Û */
  ISO(220) ASCII2('U','e') _ /* Ü */
  ISO(221) ASCII2('\'','Y') _ /* Ý */
  ISO(222) ASCII(0) _ /* Þ */
  ISO(223) ASCII2('s','s') _ /* ß */
  ISO(224) ASCII2('`','a') _ /* à */
  ISO(225) ASCII2('\'','a') _ /* á */
  ISO(226) ASCII2('^','a') _ /* â */
  ISO(227) ASCII2('~','a') _ /* ã */
  ISO(228) ASCII2('a','e') _ /* ä */
  ISO(229) ASCII('a') _ /* å */
  ISO(230) ASCII2('a','e') _ /* æ */
  ISO(231) ASCII('c') _ /* ç */
  ISO(232) ASCII2('`','e') _ /* è */
  ISO(233) ASCII2('\'','e') _ /* é */
  ISO(234) ASCII2('^','e') _ /* ê */
  ISO(235) ASCII2('\"','e') _ /* ë */
  ISO(236) ASCII2('`','i') _ /* ì */
  ISO(237) ASCII2('\'','i') _ /* í */
  ISO(238) ASCII2('^','i') _ /* î */
  ISO(239) ASCII2('\"','i') _ /* ï */
  ISO(240) ASCII('d') _ /* ð */
  ISO(241) ASCII2('~','n') _ /* ñ */
  ISO(242) ASCII2('`','o') _ /* ò */
  ISO(243) ASCII2('\'','o') _ /* ó */
  ISO(244) ASCII2('^','o') _ /* ô */
  ISO(245) ASCII2('~','o') _ /* õ */
  ISO(246) ASCII2('o','e') _ /* ö */
  ISO(247) ASCII(':') _ /* ÷ */
  ISO(248) ASCII('o') _ /* ø */
  ISO(249) ASCII2('`','u') _ /* ù */
  ISO(250) ASCII2('\'','u') _ /* ú */
  ISO(251) ASCII2('^','u') _ /* û */
  ISO(252) ASCII2('u','e') _ /* ü */
  ISO(253) ASCII2('\'','y') _ /* ý */
  ISO(254) ASCII(0) _ /* þ */
  ISO(255) ASCII2('\"','y') _ /* ÿ */
#undef _
#undef ASCII3
#undef ASCII2
#undef ASCII
#undef ISO
  { int fehler = 0;
    int c;
    while (!((c = getchar()) == EOF))
      { long cx = tabelle[c];
        if (cx == 0)
          { fehler++; }
          else
          { do { putchar(cx & 0xFF); cx = cx>>8; } while (!(cx == 0)); }
      }
    if (!(fehler == 0))
      { fprintf(stderr,"%d illegal characters\n",fehler); return 1; }
      else
      { return 0; }
} }
