# Funktionen für Characters und Strings für CLISP
# Bruno Haible 7.12.1992

#include "lispbibl.c"


# Character-Umwandlungstabellen:
#if defined(ATARI_CHS)
 # Darin sind eingetragen die bijektiven Klein<-->Groß-Umwandlungen
 #  Klein 61 ... 7A 87 81 82 84 85 86 91 94 A4 B0 B1 B3 B4 C0
 #  Groß  41 ... 5A 80 9A 90 8E B6 8F 92 99 A5 B7 B8 B2 B5 C1
 #  Beide aA ... zZ çÇ üÜ éÉ äÄ àÀ åÅ æÆ öÖ ñÑ ãÃ õÕ øØ oe ij
 # Die Nur Klein-->Groß-Umwandlungen
 #  Klein 83 88 89 8A 8B 8C 8D 93 95 96 97 98 9F A0 A1 A2 A3 A6 A7
 #  Groß  41 45 45 45 49 49 49 4F 4F 55 55 59 46 41 49 4F 55 41 4F
 #  Beide âA êE ëE èE ïI îI ìI ôO òO ûU ùU ÿY fF áA íI óO úU ªA ºO
 # mußten wegen der Forderung nach Rückkonvertierbarkeit (CLTL S. 241)
 # weggelassen werden.
#elif defined(ISOLATIN_CHS)
 # Darin sind eingetragen die bijektiven Klein<-->Groß-Umwandlungen
 #  Klein 61 ... 7A E0 ... F6 F8 ... FE
 #  Groß  41 ... 5A C0 ... D6 D8 ... DE
 #  Beide aA ... zZ àÀ ... öÖ øØ ... th
#elif defined(HPROMAN8_CHS)
 # Darin sind eingetragen die bijektiven Klein<-->Groß-Umwandlungen
 #  Klein 61 ... 7A C4 C5 D5 C6 C7 B2 C0 C1 D1 C2 C3 C8 C9 D9 CA CB
 #  Groß  41 ... 5A E0 DC E5 E7 ED B1 A2 A4 A6 DF AE A1 A3 E6 E8 AD
 #  Was   aA ... zZ a´ e´ i´ o´ u´ y´ a^ e^ i^ o^ u^ a` e` i` o` u`
 #  Klein CC CD DD CE CF EF E2 B7 EA D4 D7 D6 B5 EC E4 F1
 #  Groß  D8 A5 A7 DA DB EE E1 B6 E9 D0 D3 D2 B4 EB E3 F0
 #  Was   äÄ ë  ï  öÖ üÜ y" ãÃ ñÑ õÕ åÅ ae øØ çÇ sv -D th
#elif defined(IBMPC_CHS)
 # Darin sind eingetragen die bijektiven Klein<-->Groß-Umwandlungen
 #  Klein 61 ... 7A 87 81 82 84 86 91 94 A4
 #  Groß  41 ... 5A 80 9A 90 8E 8F 92 99 A5
 #  Beide aA ... zZ çÇ üÜ éÉ äÄ åÅ æÆ öÖ ñÑ
#else # defined(ASCII_CHS)
 # Darin sind eingetragen die bijektiven Klein<-->Groß-Umwandlungen
 #  Klein 61 ... 7A
 #  Groß  41 ... 5A
 #  Beide aA ... zZ
#endif

# Wandelt Byte ch in einen Großbuchstaben
# up_case(ch)
  global uintB up_case (uintB ch);
  global uintB up_case(ch)
    var reg1 uintB ch;
    { # Tabelle für Umwandlung in Großbuchstaben:
      local uintB up_case_table[char_code_limit] =
        #if defined(ATARI_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x9A,0x90,0x83,0x8E,0xB6,0x8F,0x80,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x92,0x92,0x93,0x99,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA5,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB7,0xB8,0xB2,0xB2,0xB5,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC1,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
            0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #elif defined(ISOLATIN_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xF7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xFF,
          };
        #elif defined(HPROMAN8_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB4,0xB6,0xB6,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xA2,0xA4,0xDF,0xAE,0xE0,0xDC,0xE7,0xB2,0xA1,0xA3,0xE8,0xAD,0xD8,0xA5,0xDA,0xDB,
            0xD0,0xA6,0xD2,0xD3,0xD0,0xE5,0xD2,0xD3,0xD8,0xE6,0xDA,0xDB,0xDC,0xA7,0xDE,0xDF,
            0xE0,0xE1,0xE1,0xE3,0xE3,0xE5,0xE6,0xE7,0xE8,0xE9,0xE9,0xEB,0xEB,0xED,0xEE,0xEE,
            0xF0,0xF0,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #elif defined(IBMPC_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x9A,0x90,0x83,0x8E,0x85,0x8F,0x80,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x92,0x92,0x93,0x99,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA5,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
            0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #else # Standard-Ascii-Umwandlungstabelle: Nur a..z --> A..Z
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
            0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
            0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #endif
      return up_case_table[ch];
    }

# Wandelt Byte ch in einen Kleinbuchstaben
# down_case(ch)
  global uintB down_case (uintB ch);
  global uintB down_case(ch)
    var reg1 uintB ch;
    { # Tabelle für Umwandlung in Kleinbuchstaben:
      local uintB down_case_table[char_code_limit] =
        #if defined(ATARI_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x87,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x84,0x86,
            0x82,0x91,0x91,0x93,0x94,0x95,0x96,0x97,0x98,0x94,0x81,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA4,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB3,0xB3,0xB4,0xB4,0x85,0xB0,0xB1,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC0,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
            0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          }; 
        #elif defined(ISOLATIN_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xD7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xDF,
            0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #elif defined(HPROMAN8_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xC8,0xC0,0xC9,0xC1,0xCD,0xD1,0xDD,0xA8,0xA9,0xAA,0xAB,0xAC,0xCB,0xC3,0xAF,
            0xB0,0xB2,0xB2,0xB3,0xB5,0xB5,0xB7,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD4,0xD1,0xD6,0xD7,0xD4,0xD5,0xD6,0xD7,0xCC,0xD9,0xCE,0xCF,0xC5,0xDD,0xDE,0xC2,
            0xC4,0xE2,0xE2,0xE4,0xE4,0xD5,0xD9,0xC6,0xCA,0xEA,0xEA,0xEC,0xEC,0xC7,0xEF,0xEF,
            0xF1,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #elif defined(IBMPC_CHS)
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x87,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x84,0x86,
            0x82,0x91,0x91,0x93,0x94,0x95,0x96,0x97,0x98,0x94,0x81,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA4,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
            0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #else # Standard-Ascii-Umwandlungstabelle: Nur A..Z --> a..z
          { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
            0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
            0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
            0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
            0x40,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x5B,0x5C,0x5D,0x5E,0x5F,
            0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
            0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F,
            0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
            0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
            0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
            0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
            0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
            0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
            0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
            0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
          };
        #endif
      return down_case_table[ch];
    }

# UP: Stellt fest, ob ein Character alphabetisch ist.
# alphap(ch)
# > ch: Character-Code
# < ergebnis: TRUE falls alphabetisch, FALSE sonst.
# Alphabetische Characters sind die mit einem Code c, mit
# $41 <= c <= $5A oder $61 <= c <= $7A
#if defined(ATARI_CHS)
# oder $80 <= c <= $9A oder $9E <= c <= $A7
# oder $B0 <= c <= $B8 oder $C0 <= c <= $C1.
#elif defined(ISOLATIN_CHS)
# oder $C0 <= c außer c=$D7,$F7.
#elif defined(HPROMAN8_CHS)
# oder $A1 <= c <= $A7 oder $AD <= c <= $AE oder $B1 <= c <= $B7 außer c=$B3
# oder $C0 <= c <= $F1.
#elif defined(IBMPC_CHS)
# oder $80 <= c <= $9A oder $9F <= c <= $A7.
#endif
# Darin sind (siehe CLTL S. 236 oben) aller Uppercase- und alle Lowercase-
# Characters enthalten.
  local boolean alphap (uintB ch);
  local boolean alphap(ch)
    var reg1 uintB ch;
    { if (ch < 0x41) goto no; if (ch <= 0x5A) goto yes;
      if (ch < 0x61) goto no; if (ch <= 0x7A) goto yes;
      #if defined(ATARI_CHS)
      if (ch < 0x80) goto no; if (ch <= 0x9A) goto yes;
      if (ch < 0x9E) goto no; if (ch <= 0xA7) goto yes;
      if (ch < 0xB0) goto no; if (ch <= 0xB8) goto yes;
      if (ch < 0xC0) goto no; if (ch <= 0xC1) goto yes;
      #elif defined(ISOLATIN_CHS)
      if (ch < 0xC0) goto no;
      if ((ch == 0xD7) || (ch == 0xF7)) goto no; else goto yes;
      #elif defined(HPROMAN8_CHS)
      if (ch < 0xA1) goto no;
      if (ch > 0xF1) goto no; if (ch >= 0xC0) goto yes;
      if (ch <= 0xA7) goto yes;
      if (ch < 0xB1)
        { if (ch < 0xAD) goto no; if (ch <= 0xAE) goto yes; goto no; }
        else
        { if (ch > 0xB7) goto no; if (ch == 0xB3) goto no; else goto yes; }
      #elif defined(IBMPC_CHS)
      if (ch < 0x80) goto no; if (ch <= 0x9A) goto yes;
      if (ch < 0x9F) goto no; if (ch <= 0xA7) goto yes;
      #endif
      no: return FALSE;
      yes: return TRUE;
    }

# Stellt fest, ob ein Character alphanumerisch ist.
# alphanumericp(ch)
# > ch: Character-Code
# < ergebnis: TRUE falls alphanumerisch, FALSE sonst.
# Alphanumerische Characters sind die alphabetischen und die Ziffern.
  global boolean alphanumericp (uintB ch);
  global boolean alphanumericp(ch)
    var reg2 uintB ch;
    { if (('0' <= ch) && (ch <= '9'))
        return TRUE; # '0' <= ch <= '9' ist alphanumerisch
        else
        return alphap(ch);
    }

# Stellt fest, ob ein Character ein Graphic-Character ("druckend") ist.
# graphic_char_p(ch)
# > ch: Character-Code
# < ergebnis: TRUE falls druckend, FALSE sonst.
# Graphic-Characters sind die mit einem Code c, mit
#if defined(ATARI_CHS)
#       $20 <= c < $100 oder c in {1,..,4}u{14,..,25}u{28,..,31}.
#elif defined(ISOLATIN_CHS) || defined(HPROMAN8_CHS)
#       $20 <= c <= $7E oder $A0 <= c < $100.
#elif defined(IBMPC_CHS)
#       $20 <= c < $100 oder c in {1,..,6}u{14,..,25}u{28,..,31}.
#       [c=11 und c=12 werden zwar auch druckend ausgegeben, aber c=12
#        ist unser #\Page, und c=11 streichen wir aus Gleichberechtigungs-
#        gründen.]
#else # defined(ASCII_CHS)
#       $20 <= c <= $7E.
#endif
  global boolean graphic_char_p (uintB ch);
  global boolean graphic_char_p(ch)
    var reg1 uintB ch;
    {
      #if defined(ATARI_CHS)
      if (ch >= ' ') goto yes; # >= ' ' -> ja
      # 0 <= ch < 32.
      # Bit ch aus der 32-Bit-Zahl %11110011111111111100000000011110 holen:
      if (0xF3FFC01EUL & bit(ch)) goto yes; else goto no;
      #elif defined(ISOLATIN_CHS) || defined(HPROMAN8_CHS)
      if ((('~' >= ch) && (ch >= ' ')) || (ch >= 0xA0)) goto yes; else goto no;
      #elif defined(IBMPC_CHS)
      if (ch >= ' ') goto yes; # >= ' ' -> ja
      # 0 <= ch < 32.
      # Bit ch aus der 32-Bit-Zahl %11110011111111111100000001111110 holen:
      if (0xF3FFC07EUL & bit(ch)) goto yes; else goto no;
      #else # defined(ASCII_CHS)
      if (ch >= ' ') goto yes; else goto no;
      #endif
      no: return FALSE;
      yes: return TRUE;
    }

# UP: verfolgt einen String.
# unpack_string(string,&len)
# > object string: ein String.
# < uintL len: Anzahl der Zeichen des Strings.
# < uintB* ergebnis: Anfangsadresse der Bytes
  global uintB* unpack_string (object string, uintL* len);
  global uintB* unpack_string(string,len)
    var reg1 object string;
    var reg2 uintL* len;
    { if (simple_string_p(string))
        { *len = TheSstring(string)->length;
          return &TheSstring(string)->data[0];
        }
        else
        # String, aber kein Simple-String => Displacement verfolgen
        { # Länge bestimmen (wie in vector_length in ARRAY.D):
          var reg3 uintL size;
          { var reg2 Array addr = TheArray(string);
            var reg3 uintL offset = offsetof(array_,dims);
            if (addr->flags & bit(arrayflags_dispoffset_bit))
              offset += sizeof(uintL);
            # Bei addr+offset fangen die Dimensionen an.
            if (addr->flags & bit(arrayflags_fillp_bit)) # evtl. Fillpointer
              offset += sizeof(uintL);
            size = *(uintL*)pointerplus(addr,offset);
          }
          *len = size;
          # Displacement verfolgen:
          { var uintL index = 0;
            var reg3 object datenvektor = array1_displace_check(string,size,&index);
            return &TheSstring(datenvektor)->data[index];
        } }
    }

# UP: vergleicht zwei Strings auf Gleichheit
# string_gleich(string1,string2)
# > string1: String
# > string2: simple-string
# < ergebnis: /=0, wenn gleich
  global boolean string_gleich (object string1, object string2);
  global boolean string_gleich(string1,string2)
    var reg4 object string1;
    var reg5 object string2;
    { var uintL len1;
      var reg1 uintB* ptr1;
      var reg2 uintB* ptr2;
      ptr1 = unpack_string(string1,&len1);
      # Ab ptr1 kommen genau len1 Zeichen.
      # Längenvergleich:
      if (!(len1 == TheSstring(string2)->length)) goto no;
      ptr2 = &TheSstring(string2)->data[0];
      # Ab ptr2 kommen genau (ebenfalls) len1 Zeichen.
      # Die len1 Zeichen vergleichen:
      { var reg3 uintL count;
        dotimesL(count,len1, { if (!(*ptr1++ == *ptr2++)) goto no; } );
      }
      return TRUE;
      no: return FALSE;
    }

# UP: vergleicht zwei Strings auf Gleichheit, case-insensitive
# string_equal(string1,string2)
# > string1: String
# > string2: simple-string
# < ergebnis: /=0, wenn gleich
  global boolean string_equal (object string1, object string2);
  global boolean string_equal(string1,string2)
    var reg4 object string1;
    var reg5 object string2;
    { var uintL len1;
      var reg1 uintB* ptr1;
      var reg2 uintB* ptr2;
      ptr1 = unpack_string(string1,&len1);
      # Ab ptr1 kommen genau len1 Zeichen.
      # Längenvergleich:
      if (!(len1 == TheSstring(string2)->length)) goto no;
      ptr2 = &TheSstring(string2)->data[0];
      # Ab ptr2 kommen genau (ebenfalls) len1 Zeichen.
      # Die len1 Zeichen vergleichen:
      { var reg3 uintL count;
        dotimesL(count,len1, { if (!(up_case(*ptr1++) == up_case(*ptr2++))) goto no; } );
      }
      return TRUE;
      no: return FALSE;
    }

# UP: kopiert einen String und macht dabei einen Simple-String draus.
# copy_string(string)
# > string: String
# < ergebnis: Simple-String mit denselben Zeichen
# kann GC auslösen
  global object copy_string (object string);
  global object copy_string(string)
    var reg5 object string;
    { pushSTACK(string); # String retten
     {var reg3 uintL len = vector_length(string); # Länge berechnen
      var reg4 object new_string = allocate_string(len);
      # new_string = neuer Simple-String mit vorgegebener Länge len
      string = popSTACK(); # String zurück
      if (!(len==0))
        { var local uintL len_; # nochmals die Länge, unbenutzt
          var reg1 uintB* ptr1 = unpack_string(string,&len_);
          var reg2 uintB* ptr2 = &TheSstring(new_string)->data[0];
          # Kopierschleife: Kopiere len Bytes von ptr1[] nach ptr2[]:
          dotimespL(len,len, { *ptr2++ = *ptr1++; } );
        }
      return new_string;
    }}

# UP: wandelt einen String in einen Simple-String um.
# coerce_ss(obj)
# > obj: Lisp-Objekt, sollte ein String sein.
# < ergebnis: Simple-String mit denselben Zeichen
# kann GC auslösen
  global object coerce_ss (object obj);
  global object coerce_ss(obj)
    var reg1 object obj;
    { switch (typecode(obj))
        { case_sstring:
            # Simple-String, unverändert zurück
            return obj;
          case_ostring:
            # sonstiger String, kopieren
            return copy_string(obj);
          default:
            pushSTACK(obj);
            fehler(
                   DEUTSCH ? "Das ist kein String: ~" :
                   ENGLISH ? "This is not a string: ~" :
                   FRANCAIS ? "Ceci n'est pas une chaîne : ~" :
                   ""
                  );
    }   }

# UP: Konversion eines Objekts zu einem Character
# coerce_char(obj)
# > obj: Lisp-Objekt
# < ergebnis: Character oder NIL
  global object coerce_char (object obj);
  global object coerce_char(obj)
    var reg1 object obj;
    { if (charp(obj))
        return obj; # Character unverändert zurück
        else
        if (symbolp(obj))
          { # obj ist ein Symbol
            obj = TheSymbol(obj)->pname; goto string;
          }
          else
          if (stringp(obj))
            { string: # obj ist ein String
              { var uintL len;
                var reg1 uintB* ptr = unpack_string(obj,&len);
                # ab ptr kommen len Characters
                if (len==1) return code_char(ptr[0]);
            } }
            else
            if (posfixnump(obj))
              { var reg1 uintL code = posfixnum_to_L(obj);
                if (code < char_int_limit)
                  # obj ist ein Fixnum >=0, < char_int_limit
                  return int_char(code);
              }
      # war nichts von allem -> nicht in Character umwandelbar
      return NIL; # NIL als Ergebnis
    }

# Character-Namen:
# Nur die Characters mit Font 0 und Bits 0 haben Namen. Unter diesen
# sind alle non-graphic String-Chars und das Space.
# Vom Reader wird allerdings auch die Syntax #\A für das Character A (usw.
# für alle Characters) und die Syntax #\Code231 für das Character mit dem
# Code 231 (dezimal) akzeptiert, dies für alle Characters aus Font 0.

# Tabelle der Character-Namen:
# in CONSTOBJ.D definiert,
  #ifdef ATARI_CHARNAMES
    #define charname_table_length  15  # Länge der Tabelle
    #define charname_table_extra   25  # zusätzlich
    #define charname_table  ((object*)(&object_tab.charname_0)) # Tabelle fängt mit charname_0 an
  #endif
  #ifdef AMIGA_CHARNAMES
    #define charname_table_length  43  # Länge der Tabelle
    #define charname_table_extra    0  # zusätzlich
    #define charname_table  ((object*)(&object_tab.charname_0)) # Tabelle fängt mit charname_0 an
  #endif
  #ifdef MSDOS_CHARNAMES
    #define charname_table_length  13  # Länge der Tabelle
    #define charname_table_extra   24  # zusätzlich
    #define charname_table  ((object*)(&object_tab.charname_0)) # Tabelle fängt mit charname_0 an
  #endif
  #ifdef UNIX_CHARNAMES
    #define charname_table_length  46  # Länge der Tabelle
    #define charname_table_extra   22  # zusätzlich
    #define charname_table  ((object*)(&object_tab.charname_0bis)) # Tabelle fängt mit charname_0bis an
  #endif
# Tabelle der Codes zu diesen Namen:
  local uintB charname_table_codes [charname_table_length+charname_table_extra]
    #ifdef ATARI_CHARNAMES
      = { 0,5,6,BEL,BS,TAB,NL,11,PG,CR,26,ESC,' ',RUBOUT,LF,
          CR,16,17,18,19,20,22,23,24,25,28,29,127,
          'A','B','C','D','E','F','G','H','I','J','K','L',
        };
    #endif
    #ifdef AMIGA_CHARNAMES
      = { 0,1,2,3,4,5,6,BEL,BS,TAB,NL,11,PG,CR,14,15,16,17,18,19,20,21,22,
          23,24,25,26,ESC,28,29,30,31,' ',7,8,RUBOUT,9,LF,10,12,13,27,155,
        };
    #endif
    #ifdef MSDOS_CHARNAMES
      = { 0,BEL,BS,TAB,NL,11,PG,CR,26,ESC,' ',RUBOUT,LF,
          CR,16,17,18,19,20,22,23,24,25,29,127,
          'A','B','C','D','E','F','G','H','I','J','K','L',
        };
    #endif
    #ifdef UNIX_CHARNAMES
      = { 0,7,BS,TAB,NL,LF,PG,CR,27,32,RUBOUT,127,
          0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
          20,21,22,23,24,25,26,27,28,29,30,31,32,127,
          16,17,18,19,20,21,22,23,24,25,
          'A','B','C','D','E','F','G','H','I','J','K','L',
        };
    #endif
# Zum Namen charname_table[i] gehört der Code charname_table_codes[i]
# (für 0 <= i < charname_table_length).

# UP: Liefert den Namen eines Zeichens.
# char_name(code)
# > uintB code: Ascii-Code eines Zeichens
# < ergebnis: Simple-String (Name dieses Zeichens) oder NIL
  global object char_name (uintB code);
  global object char_name(code)
    var reg1 uintB code;
    { var reg4 uintB* codes_ptr = &charname_table_codes[0];
      var reg3 object* strings_ptr = &charname_table[0];
      var reg2 uintC count;
      dotimesC(count,charname_table_length,
        { if (code == *codes_ptr++) goto found; # code mit charname_table_codes[i] vergleichen
          strings_ptr++;
        });
      # nicht gefunden
      return NIL;
      found: # gefunden
        return *strings_ptr; # String charname_table[i] aus der Tabelle holen
    }

# UP: Bestimmt das Character mit einem gegebenen Namen
# name_char(string)
# > string: String
# < ergebnis: Character mit diesem Namen, oder NIL falls keins existiert
  global object name_char (object string);
  global object name_char(string)
    var reg3 object string;
    { var reg4 uintB* codes_ptr = &charname_table_codes[0];
      var reg3 object* strings_ptr = &charname_table[0];
      var reg2 uintC count;
      dotimesC(count,charname_table_length,
        { if (string_equal(string,*strings_ptr++)) goto found; # string mit charname_table[i] vergleichen
          codes_ptr++;
        });
      dotimesC(count,charname_table_extra,
        { if (string_equal(string,*strings_ptr++)) goto found_extra; # string mit charname_table[i] vergleichen
          codes_ptr++;
        });
      # kein Character mit diesem Namen gefunden
      return NIL;
      found: # gefunden
        return code_char(*codes_ptr); # Code charname_table_codes[i] aus der Tabelle holen
      found_extra: # gefunden unter den Extra-Namen
        return int_char((cint)(*codes_ptr << char_code_shift_c) | char_hyper_c); # hier mit Hyper-Bit
    }

# Fehlermeldung, falls ein Argument kein Character ist:
# fehler_char(arg)
# > arg: Das fehlerhafte Argument
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_char (object arg);
  local nonreturning void fehler_char(arg)
    var reg1 object arg;
    { pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Argument ~ ist kein Character." :
             ENGLISH ? "~: argument ~ is not a character" :
             FRANCAIS ? "~: L'argument ~ n'est pas un caractère." :
             ""
            );
    }

LISPFUNN(standard_char_p,1) # (STANDARD-CHAR-P char), CLTL S. 234
# (standard-char-p char) ==
#   (or (char= char #\Newline) (char<= #\Space char #\~))
# Standard-Chars sind die mit einem Code c, mit
#       $20 <= c <= $7E oder c = NL.
  { var reg2 object arg = popSTACK(); # Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    { var reg1 cint ch = char_int(arg);
      if ((('~' >= ch) && (ch >= ' ')) || (ch == NL))
        { value1 = T; mv_count=1; }
        else
        { value1 = NIL; mv_count=1; }
  } }

LISPFUNN(graphic_char_p,1) # (GRAPHIC-CHAR-P char), CLTL S. 234
  { var reg2 object arg = popSTACK(); # Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    { var reg1 cint ch = char_int(arg);
      if (ch >= char_code_limit) goto no; # kein String-Char -> nein
      if (graphic_char_p(ch)) goto yes; else goto no;
    }
    yes: value1 = T; mv_count=1; return;
    no: value1 = NIL; mv_count=1; return;
  }

LISPFUNN(string_char_p,1) # (STRING-CHAR-P char), CLTL S. 235
# String-Chars sind die mit einem Code c, mit 0 <= c < $100.
  { var reg2 object arg = popSTACK(); # Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    { var reg1 cint ch = char_int(arg);
      if (ch >= char_code_limit) goto no;
      goto yes;
    }
    yes: value1 = T; mv_count=1; return;
    no: value1 = NIL; mv_count=1; return;
  }

LISPFUNN(alpha_char_p,1) # (ALPHA-CHAR-P char), CLTL S. 235
# Nur String-Chars sind alphabetisch, auf sie wird ALPHAP angewandt
  { var reg2 object arg = popSTACK(); # Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    { var reg1 cint ch = char_int(arg);
      if (ch >= char_code_limit) goto no; # kein String-Char -> nein
      if (alphap(ch)) goto yes; else goto no;
    }
    yes: value1 = T; mv_count=1; return;
    no: value1 = NIL; mv_count=1; return;
  }

LISPFUNN(upper_case_p,1) # (UPPER-CASE-P char), CLTL S. 235
# Upper-case-Characters sind die mit einem Code c mit 0 <= c < $100, die
# von (downcase char) verschieden sind.
  { var reg2 object arg = popSTACK(); # Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    { var reg1 cint ch = char_int(arg);
      if (ch >= char_code_limit) goto no; # kein String-Char -> nein
      if (!(down_case(ch)==ch)) goto yes; else goto no;
    }
    yes: value1 = T; mv_count=1; return;
    no: value1 = NIL; mv_count=1; return;
  }

LISPFUNN(lower_case_p,1) # (LOWER-CASE-P char), CLTL S. 235
# Lower-case-Characters sind die mit einem Code c mit 0 <= c < $100, die
# von (upcase char) verschieden sind.
  { var reg2 object arg = popSTACK(); # Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    { var reg1 cint ch = char_int(arg);
      if (ch >= char_code_limit) goto no; # kein String-Char -> nein
      if (!(up_case(ch)==ch)) goto yes; else goto no;
    }
    yes: value1 = T; mv_count=1; return;
    no: value1 = NIL; mv_count=1; return;
  }

LISPFUNN(both_case_p,1) # (BOTH-CASE-P char), CLTL S. 235
# (both-case-p char) == (or (upper-case-p char) (lower-case-p char))
# Both-case-Characters sind die mit einem Code c mit 0 <= c < $100, bei denen
# (downcase char) und (upcase char) verschieden sind.
  { var reg2 object arg = popSTACK(); # Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    { var reg1 cint ch = char_int(arg);
      if (ch >= char_code_limit) goto no; # kein String-Char -> nein
      if (!(down_case(ch)==up_case(ch))) goto yes; else goto no;
    }
    yes: value1 = T; mv_count=1; return;
    no: value1 = NIL; mv_count=1; return;
  }

# UP: Uberprüft ein optionales Radix-Argument
# test_radix_arg()
# > STACK_0: Argument, Default ist 10
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Radix, ein Integer >=2, <=36
# erhöht STACK um 1
  local uintWL test_radix_arg (void);
  local uintWL test_radix_arg()
    { var reg1 object arg = popSTACK(); # Argument
      if (eq(arg,unbound)) { return 10; }
      if (posfixnump(arg))
        { var reg2 uintL radix = posfixnum_to_L(arg);
          if ((2 <= radix) && (radix <= 36)) return radix;
        }
      # Fehler.
      pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Als Zahlsystembasis sind nur Integers zwischen 2 und 36 zulässig, nicht ~." :
             ENGLISH ? "~: the radix must be an integer between 2 and 36, not ~" :
             FRANCAIS ? "~: Seuls les entiers compris entre 2 et 36 sont possible comme base et non ~." :
             ""
            );
    }

LISPFUN(digit_char_p,1,1,norest,nokey,0,NIL)
# (DIGIT-CHAR-P char [radix]), CLTL S. 236
# Methode:
# Test, ob radix ein Integer >=2 und <=36 ist.
# char muß ein String-Char <= 'z' sein, sonst NIL als Ergebnis.
# Falls radix<=10: c muß >= '0' und < '0'+radix sein, sonst NIL.
# Falls radix>=10: c muß >= '0' und <= '9' oder
#                  (upcase c) muß >= 'A' und < 'A'-10+radix sein, sonst NIL.
  { var reg1 uintWL radix = test_radix_arg(); # Zahlbasis, >=2, <=36
    var reg2 object arg = popSTACK(); # Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    { var reg1 cint ch = char_int(arg);
      if (ch > 'z') goto no; # kein String-Char oder zu groß -> nein
      if (ch >= 'a') { ch -= 'a'-'A'; } # Character >='a',<='z' in Großbuchstaben wandeln
      # Nun ist $00 <= ch <= $60.
      if (ch < '0') goto no;
      # $30 <= ch <= $60 in Zahlwert umwandeln:
      if (ch <= '9') { ch = ch - '0'; }
      else if (ch >= 'A') { ch = ch - 'A' + 10; }
      else goto no;
      # Nun ist ch der Zahlwert der Ziffer, >=0, <=41.
      if (ch >= radix) goto no; # nur gültig, falls 0 <= ch < radix.
      # Wert als Fixnum zurück:
      value1 = fixnum(ch); mv_count=1; return;
    }
    no: value1 = NIL; mv_count=1; return;
  }

LISPFUNN(alphanumericp,1) # (ALPHANUMERICP char), CLTL S. 236
# Alphanumerische Characters sind die Ziffern '0',...,'9' und die
# alphabetischen Characters.
  { var reg2 object arg = popSTACK(); # Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    { var reg1 cint ch = char_int(arg);
      if (ch >= char_code_limit) goto no; # kein String-Char -> nein
      if (alphanumericp(ch)) goto yes; else goto no;
    }
    yes: value1 = T; mv_count=1; return;
    no: value1 = NIL; mv_count=1; return;
  }

# Zeichenvergleichsfunktionen:
# Die Vergleiche CHAR=,... vergleichen das gesamte oint (oder äquivalent,
# nur das cint, aber inclusive Font und Bits).
# Die Vergleiche CHAR-EQUAL,... ignorieren Font und Bits, wandeln die
# Ascii-Codes in Großbuchstaben um und vergleichen diese.

# UP: Testet, ob alle argcount+1 Argumente unterhalb von args_pointer
# Characters sind. Wenn nein, Error.
# > argcount: Argumentezahl-1
# > args_pointer: Pointer über die Argumente
# > subr_self: Aufrufer (ein SUBR)
  local void test_char_args (uintC argcount, object* args_pointer);
  local void test_char_args(argcount,args_pointer)
    var reg2 uintC argcount;
    var reg1 object* args_pointer;
    { dotimespC(argcount,argcount+1,
        { var reg3 object arg = NEXT(args_pointer); # nächstes Argument
          if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
        });
    }

# UP: Testet, ob alle argcount+1 Argumente unterhalb von args_pointer
# Characters sind. Wenn nein, Error. Streicht von ihnen Bits und Font
# und wandelt sie in Großbuchstaben um.
# > argcount: Argumentezahl-1
# > args_pointer: Pointer über die Argumente
# > subr_self: Aufrufer (ein SUBR)
  local void test_char_args_upcase (uintC argcount, object* args_pointer);
  local void test_char_args_upcase(argcount,args_pointer)
    var reg2 uintC argcount;
    var reg1 object* args_pointer;
    { dotimespC(argcount,argcount+1,
        { var reg3 object* argptr = &NEXT(args_pointer);
          var reg3 object arg = *argptr; # nächstes Argument
          if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
          *argptr = code_char(up_case(char_code(arg))); # durch Großbuchstaben ersetzen
        });
    }

# UP: (CHAR= char {char}) bei überprüften Argumenten
  local Values char_gleich (uintC argcount, object* args_pointer);
  local Values char_gleich (argcount,args_pointer)
    var reg2 uintC argcount;
    var reg1 object* args_pointer;
    # Methode:
    # n+1 Argumente Arg[0..n].
    # x:=Arg[n].
    # for i:=n-1 to 0 step -1 do ( if Arg[i]/=x then return(NIL) ), return(T).
    { var reg3 object x = popSTACK(); # letztes Argument nehmen
      dotimesC(argcount,argcount, { if (!eq(popSTACK(),x)) goto no; } );
      yes: value1 = T; goto ok;
      no: value1 = NIL; goto ok;
      ok: mv_count=1; set_args_end_pointer(args_pointer);
    }

# UP: (CHAR/= char {char}) bei überprüften Argumenten
  local Values char_ungleich (uintC argcount, object* args_pointer);
  local Values char_ungleich (argcount,args_pointer)
    var reg6 uintC argcount;
    var reg5 object* args_pointer;
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for j:=n-1 to 0 step -1 do
    #   x:=Arg[j+1], for i:=j to 0 step -1 do
    #                   if Arg[i]=x then return(NIL),
    # return(T).
    { var reg4 object* arg_j_ptr = args_end_pointer;
      var reg3 uintC j = argcount;
      until (j==0)
        { var reg2 object x = BEFORE(arg_j_ptr); # nächst-letztes Argument
          # mit allen Argumenten davor vergleichen:
          var reg1 object* arg_i_ptr = arg_j_ptr;
          var reg1 uintC i;
          dotimespC(i,j, { if (eq(BEFORE(arg_i_ptr),x)) goto no; } );
          j--;
        }
      yes: value1 = T; goto ok;
      no: value1 = NIL; goto ok;
      ok: mv_count=1; set_args_end_pointer(args_pointer);
    }

# UP: (CHAR< char {char}) bei überprüften Argumenten
  local Values char_kleiner (uintC argcount, object* args_pointer);
  local Values char_kleiner (argcount,args_pointer)
    var reg3 uintC argcount;
    var reg2 object* args_pointer;
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for i:=n to 1 step -1 do
    #    x:=Arg[i], if x char<= Arg[i-1] then return(NIL),
    # return(T).
    { dotimesC(argcount,argcount,
        { var reg1 object x = popSTACK();
          if ((oint)x <= (oint)STACK_0) goto no;
        });
      yes: value1 = T; goto ok;
      no: value1 = NIL; goto ok;
      ok: mv_count=1; set_args_end_pointer(args_pointer);
    }

# UP: (CHAR> char {char}) bei überprüften Argumenten
  local Values char_groesser (uintC argcount, object* args_pointer);
  local Values char_groesser (argcount,args_pointer)
    var reg3 uintC argcount;
    var reg2 object* args_pointer;
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for i:=n to 1 step -1 do
    #    x:=Arg[i], if x char>= Arg[i-1] then return(NIL),
    # return(T).
    { dotimesC(argcount,argcount,
        { var reg1 object x = popSTACK();
          if ((oint)x >= (oint)STACK_0) goto no;
        });
      yes: value1 = T; goto ok;
      no: value1 = NIL; goto ok;
      ok: mv_count=1; set_args_end_pointer(args_pointer);
    }

# UP: (CHAR<= char {char}) bei überprüften Argumenten
  local Values char_klgleich (uintC argcount, object* args_pointer);
  local Values char_klgleich (argcount,args_pointer)
    var reg3 uintC argcount;
    var reg2 object* args_pointer;
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for i:=n to 1 step -1 do
    #    x:=Arg[i], if x char< Arg[i-1] then return(NIL),
    # return(T).
    { dotimesC(argcount,argcount,
        { var reg1 object x = popSTACK();
          if ((oint)x < (oint)STACK_0) goto no;
        });
      yes: value1 = T; goto ok;
      no: value1 = NIL; goto ok;
      ok: mv_count=1; set_args_end_pointer(args_pointer);
    }

# UP: (CHAR>= char {char}) bei überprüften Argumenten
  local Values char_grgleich (uintC argcount, object* args_pointer);
  local Values char_grgleich (argcount,args_pointer)
    var reg3 uintC argcount;
    var reg2 object* args_pointer;
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for i:=n to 1 step -1 do
    #    x:=Arg[i], if x char> Arg[i-1] then return(NIL),
    # return(T).
    { dotimesC(argcount,argcount,
        { var reg1 object x = popSTACK();
          if ((oint)x > (oint)STACK_0) goto no;
        });
      yes: value1 = T; goto ok;
      no: value1 = NIL; goto ok;
      ok: mv_count=1; set_args_end_pointer(args_pointer);
    }

LISPFUN(char_gleich,1,0,rest,nokey,0,NIL) # (CHAR= char {char}), CLTL S. 237
  { var reg2 object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args(argcount,args_pointer);
    return_Values char_gleich(argcount,args_pointer);
  }

LISPFUN(char_ungleich,1,0,rest,nokey,0,NIL) # (CHAR/= char {char}), CLTL S. 237
  { var reg2 object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args(argcount,args_pointer);
    return_Values char_ungleich(argcount,args_pointer);
  }

LISPFUN(char_kleiner,1,0,rest,nokey,0,NIL) # (CHAR< char {char}), CLTL S. 237
  { var reg2 object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args(argcount,args_pointer);
    return_Values char_kleiner(argcount,args_pointer);
  }

LISPFUN(char_groesser,1,0,rest,nokey,0,NIL) # (CHAR> char {char}), CLTL S. 237
  { var reg2 object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args(argcount,args_pointer);
    return_Values char_groesser(argcount,args_pointer);
  }

LISPFUN(char_klgleich,1,0,rest,nokey,0,NIL) # (CHAR<= char {char}), CLTL S. 237
  { var reg2 object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args(argcount,args_pointer);
    return_Values char_klgleich(argcount,args_pointer);
  }

LISPFUN(char_grgleich,1,0,rest,nokey,0,NIL) # (CHAR>= char {char}), CLTL S. 237
  { var reg2 object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args(argcount,args_pointer);
    return_Values char_grgleich(argcount,args_pointer);
  }

LISPFUN(char_equal,1,0,rest,nokey,0,NIL) # (CHAR-EQUAL char {char}), CLTL S. 239
  { var reg2 object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args_upcase(argcount,args_pointer);
    return_Values char_gleich(argcount,args_pointer);
  }

LISPFUN(char_not_equal,1,0,rest,nokey,0,NIL) # (CHAR-NOT-EQUAL char {char}), CLTL S. 239
  { var reg2 object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args_upcase(argcount,args_pointer);
    return_Values char_ungleich(argcount,args_pointer);
  }

LISPFUN(char_lessp,1,0,rest,nokey,0,NIL) # (CHAR-LESSP char {char}), CLTL S. 239
  { var reg2 object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args_upcase(argcount,args_pointer);
    return_Values char_kleiner(argcount,args_pointer);
  }

LISPFUN(char_greaterp,1,0,rest,nokey,0,NIL) # (CHAR-GREATERP char {char}), CLTL S. 239
  { var reg2 object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args_upcase(argcount,args_pointer);
    return_Values char_groesser(argcount,args_pointer);
  }

LISPFUN(char_not_greaterp,1,0,rest,nokey,0,NIL) # (CHAR-NOT-GREATERP char {char}), CLTL S. 239
  { var reg2 object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args_upcase(argcount,args_pointer);
    return_Values char_klgleich(argcount,args_pointer);
  }

LISPFUN(char_not_lessp,1,0,rest,nokey,0,NIL) # (CHAR-NOT-LESSP char {char}), CLTL S. 239
  { var reg2 object* args_pointer = rest_args_pointer STACKop 1;
    test_char_args_upcase(argcount,args_pointer);
    return_Values char_grgleich(argcount,args_pointer);
  }

LISPFUNN(char_code,1) # (CHAR-CODE char), CLTL S. 239
  { var reg1 object arg = popSTACK(); # Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    value1 = fixnum(char_code(arg)); # Ascii-Code als Fixnum
    mv_count=1;
  }

LISPFUNN(char_bits,1) # (CHAR-BITS char), CLTL S. 240
  { var reg1 object arg = popSTACK(); # Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    value1 = fixnum(((char_int(arg) & char_bits_mask_c) >> char_bits_shift_c));
    mv_count=1;
  }

LISPFUNN(char_font,1) # (CHAR-FONT char), CLTL S. 240
  { var reg1 object arg = popSTACK(); # Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    value1 = fixnum(((char_int(arg) & char_font_mask_c) >> char_font_shift_c));
    mv_count=1;
  }

# UP: Überprüft ein optionales Font-Argument
# > STACK_0: Argument, Default ist 0
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Font, ein Integer
# erhöht STACK um 1
  local object test_font_arg (void);
  local object test_font_arg()
    { var reg1 object arg = popSTACK(); # font-Argument
      if (eq(arg,unbound)) { return Fixnum_0; } # 0 als Default
      if (integerp(arg)) { return arg; }
      # arg ist kein Integer.
      pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Font-Argument muß ein Integer sein, nicht ~." :
             ENGLISH ? "~: the font argument should be an integer, not ~" :
             FRANCAIS ? "~: L'argument fonte doit être un entier et non ~." :
             ""
            );
    }

# UP: Überprüft ein optionales Bits-Argument
# > STACK_0: Argument, Default ist 0
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Bits, ein Integer
# erhöht STACK um 1
  local object test_bits_arg (void);
  local object test_bits_arg()
    { var reg1 object arg = popSTACK(); # bits-Argument
      if (eq(arg,unbound)) { return Fixnum_0; } # 0 als Default
      if (integerp(arg)) { return arg; }
      # arg ist kein Integer.
      pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Bits-Argument muß ein Integer sein, nicht ~." :
             ENGLISH ? "~: the bits argument should be an integer, not ~" :
             FRANCAIS ? "~: L'argument bits doit être un entier et non ~." :
             ""
            );
    }

LISPFUN(code_char,1,2,norest,nokey,0,NIL)
# (CODE-CHAR code [bits] [font]), CLTL S. 240
  { var reg5 object fontobj = test_font_arg(); # Font-Argument, ein Integer
    var reg6 object bitsobj = test_bits_arg(); # Bits-Argument, ein Integer
    var reg4 object codeobj = popSTACK(); # code-Argument
    if (!integerp(codeobj))
      { # code-Argument ist kein Integer.
        pushSTACK(codeobj); pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: Code-Argument muß ein Integer sein, nicht ~." :
               ENGLISH ? "~: the code argument should be an integer, not ~" :
               FRANCAIS ? "~: L'argument code doit être un entier et non ~." :
               ""
              );
      }
    # codeobj ist jetzt ein Integer.
    { var reg3 uintL font;
      var reg2 uintL bits;
      var reg1 uintL code;
      # Teste, ob  0 <= font < char_font_limit
      #       und  0 <= bits < char_bits_limit
      #       und  0 <= code < char_code_limit :
      if ( (posfixnump(fontobj)) && ((font = posfixnum_to_L(fontobj)) < char_font_limit)
        && (posfixnump(bitsobj)) && ((bits = posfixnum_to_L(bitsobj)) < char_bits_limit)
        && (posfixnump(codeobj)) && ((code = posfixnum_to_L(codeobj)) < char_code_limit)
         )
        { # Bastle neues Character:
          value1 = int_char( (font << char_font_shift_c) |
                             (bits << char_bits_shift_c) |
                             (code << char_code_shift_c) );
          mv_count=1;
        }
        else
        { value1 = NIL; mv_count=1; } # sonst Wert NIL
  } }

LISPFUN(make_char,1,2,norest,nokey,0,NIL)
# (MAKE-CHAR char [bits] [font]), CLTL S. 240
  { var reg5 object fontobj = test_font_arg(); # Font-Argument, ein Integer
    var reg6 object bitsobj = test_bits_arg(); # Bits-Argument, ein Integer
    var reg4 object charobj = popSTACK(); # char-Argument
    if (!(charp(charobj))) fehler_char(charobj);
    { var reg3 uintL font;
      var reg2 uintL bits;
      # Teste, ob  0 <= font < char_font_limit
      #       und  0 <= bits < char_bits_limit :
      if ( (posfixnump(fontobj)) && ((font = posfixnum_to_L(fontobj)) < char_font_limit)
        && (posfixnump(bitsobj)) && ((bits = posfixnum_to_L(bitsobj)) < char_bits_limit)
         )
        { # Bastle neues Character:
          value1 = int_char( (font << char_font_shift_c) |
                             (bits << char_bits_shift_c) |
                             (char_code(charobj) << char_code_shift_c) );
          mv_count=1;
        }
        else
        { value1 = NIL; mv_count=1; } # sonst Wert NIL
  } }

LISPFUNN(character,1) # (CHARACTER object), CLTL S. 241
  { var reg1 object try = coerce_char(STACK_0); # Argument in Character umwandeln
    if (nullp(try)) # erfolglos?
      { # Argument noch in STACK_0
        pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: ~ kann nicht in ein Character umgewandelt werden." :
               ENGLISH ? "~: cannot coerce ~ to a character" :
               FRANCAIS ? "~: ~ ne peut pas être transformé en caractère." :
               ""
              );
      }
      else
      { value1 = try; mv_count=1; skipSTACK(1); }
  }

LISPFUNN(char_upcase,1) # (CHAR-UPCASE char), CLTL S. 241
  { var reg2 object arg = popSTACK(); # char-Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    { var reg1 cint ch = char_int(arg);
      value1 =
        ( (ch >= char_code_limit)
          ? arg # kein String-Char, also Font oder Bits /=0 -> tut sich nichts
          : int_char(up_case(ch)) # sonst in Großbuchstaben umwandeln
        );
      mv_count=1;
  } }

LISPFUNN(char_downcase,1) # (CHAR-DOWNCASE char), CLTL S. 241
  { var reg2 object arg = popSTACK(); # char-Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    { var reg1 cint ch = char_int(arg);
      value1 =
        ( (ch >= char_code_limit)
          ? arg # kein String-Char, also Font oder Bits /=0 -> tut sich nichts
          : int_char(down_case(ch)) # sonst in Kleinbuchstaben umwandeln
        );
      mv_count=1;
  } }

LISPFUN(digit_char,1,2,norest,nokey,0,NIL)
# (DIGIT-CHAR weight [radix] [font]), CLTL S. 241
  # Methode:
  # Alles müssen Integers sein, radix zwischen 2 und 36.
  # Falls font=0 und 0 <= weight < radix, konstruiere
  #     ein String-Char aus '0',...,'9','A',...,'Z' mit Wert weight.
  # Sonst Wert NIL. (Denn Characters mit font/=0 erfüllen nicht DIGIT-CHAR-P.)
  { var reg4 object font = test_font_arg(); # Font-Argument, ein Integer
    var reg3 uintWL radix = test_radix_arg(); # radix-Argument, >=2, <=36
    var reg2 object weightobj = popSTACK(); # weight-Argument
    if (!integerp(weightobj))
      { # weight-Argument ist kein Integer.
        pushSTACK(weightobj); pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: Weight-Argument muß ein Integer sein, nicht ~." :
               ENGLISH ? "~: the weight argument should be an integer, not ~" :
               FRANCAIS ? "~: L'argument poids doit être un entier et non ~." :
               ""
              );
      }
    # weightobj ist jetzt ein Integer.
    # Teste, ob font=0 und 0<=weight<radix, sonst NIL:
    { var reg1 uintL weight;
      if ((eq(font,Fixnum_0))
          && (posfixnump(weightobj))
          && ((weight = posfixnum_to_L(weightobj)) < radix)
         )
        { weight = weight + '0'; # in Ziffer umwandeln
          if (weight > '9') { weight += 'A'-'0'-10; } # oder Buchstaben draus machen
          value1 = code_char(weight); # String-Char basteln (font ist ja =0)
          mv_count=1;
        }
        else
        { value1 = NIL; mv_count=1; }
  } }

LISPFUNN(char_int,1) # (CHAR-INT char), CLTL S. 242
  { var reg1 object arg = popSTACK(); # char-Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    value1 = fixnum(char_int(arg)); mv_count=1;
  }

LISPFUNN(int_char,1) # (INT-CHAR integer), CLTL S. 242
  { var reg2 object arg = popSTACK(); # integer-Argument
    if (integerp(arg))
      { # bei 0 <= arg < char_int_limit in Character umwandeln, sonst NIL
        var reg1 uintL i;
        if ((posfixnump(arg)) && ((i = posfixnum_to_L(arg)) < char_int_limit))
          { value1 = int_char(i); mv_count=1; }
          else
          { value1 = NIL; mv_count=1; }
      }
      else
      { # arg kein Integer -> Fehler:
        pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: Argument muß ein Integer sein, nicht ~." :
               ENGLISH ? "~: argument should be an integer, not ~" :
               FRANCAIS ? "~: L'argument doit être un entier et non ~." :
               ""
              );
      }
  }

LISPFUNN(char_name,1) # (CHAR-NAME char), CLTL S. 242
  { var reg1 object arg = popSTACK(); # char-Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    { var reg1 cint ch = char_int(arg);
      value1 =
        ( (ch >= char_code_limit)
          ? NIL # Characters mit Bits oder Font /=0 haben keinen Namen
          : char_name(ch)
        );
      mv_count=1;
  } }

# UP: Überprüft ein Bitname-Argument
# Das Argument muß eines der Keywords :CONTROL, :META, :SUPER, :HYPER oder
# einer der Werte der Konstanten CHAR-CONTROL-BIT = 1, CHAR-META-BIT = 2,
# CHAR-SUPER-BIT = 4, CHAR-HYPER-BIT = 8 sein.
# test_bitname_arg()
# > STACK_0: Argument
# > subr_self: Aufrufer
# < ergebnis: Maske fürs Bit (genau 1 Bit gesetzt)
# erhöht STACK um 1
  local cint test_bitname_arg (void);
  local cint test_bitname_arg()
    { var reg5 object arg = popSTACK(); # Argument
      var reg1 object* bitnamekwptr = &object_tab.bitnamekw_0; # Pointer in Bitnamen-Tabelle
      var reg2 uintL intval = 1; # Bitname als Integer-Wert
      var reg4 cint bitmask = bit(char_bits_shift_c); # Bit als cint-Maske
      var reg3 uintC count;
      dotimesC(count,char_bits_len_c,
        { # Hier ist für i=0,...,char_bits_len_c-1:
          # bitnamekwptr = &object_tab.bitnamekw_i,
          # intval = 2^i, bitmask = bit(char_bits_shift_c + i).
          if (eq(arg,*bitnamekwptr++) # ist arg das Bitnamen-Keyword Nummer i
              || eq(arg,fixnum(intval)) # oder das Fixnum 2^i
             )
            goto found; # ja -> fertig
          intval = intval << 1;
          bitmask = bitmask << 1;
        });
      # Bitname nicht gefunden -> Fehler:
      pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Als Bit-Name sind nur :CONTROL, :META, :SUPER, :HYPER zugelassen, nicht ~." :
             ENGLISH ? "~: the only bit names are :CONTROL, :META, :SUPER, :HYPER, not ~" :
             FRANCAIS ? "~: Les seuls noms bits permis sont :CONTROL, :META, :SUPER et :HYPER et non ~." :
             ""
            );
      found: return bitmask;
    }

LISPFUNN(char_bit,2) # (CHAR-BIT char name), CLTL S. 243
  { var reg2 cint bitmask = test_bitname_arg(); # name als Bitmaske
    var reg1 object arg = popSTACK(); # char-Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
    # entsprechendes Bit herausgreifen:
    if ((char_int(arg) & bitmask)==0) goto no; else goto yes;
    yes: value1 = T; mv_count=1; return;
    no: value1 = NIL; mv_count=1; return;
  }

LISPFUNN(set_char_bit,3) # (SET-CHAR-BIT char name newvalue), CLTL S. 244
  { var reg4 object newvalue = popSTACK();
    var reg2 cint bitmask = test_bitname_arg(); # name als Bitmaske
    var reg1 object arg = popSTACK(); # char-Argument
    if (!(charp(arg))) fehler_char(arg); # muß ein Character sein
   {var reg3 cint ch = char_int(arg);
    # entsprechendes Bit setzen oder löschen:
    if (nullp(newvalue)) { ch = ch & ~bitmask; } else { ch = ch | bitmask; }
    value1 = int_char(ch); mv_count=1;
  }}


# Fehlermeldung, falls ein Argument kein String ist:
# > obj: Das fehlerhafte Argument
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_string (object obj);
  local nonreturning void fehler_string(obj)
    var reg1 object obj;
    { pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Argument ~ ist kein String." :
             ENGLISH ? "~: argument ~ is not a string" :
             FRANCAIS ? "~: L'argument ~ n'est pas une chaîne." :
             ""
            );
    }

# Fehlermeldung, falls ein Argument kein String ist:
# > obj: Das fehlerhafte Argument
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_sstring (object obj);
  local nonreturning void fehler_sstring(obj)
    var reg1 object obj;
    { pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Argument ~ ist kein Simple-String." :
             ENGLISH ? "~: argument ~ is not a simple string" :
             FRANCAIS ? "~: L'argument ~ n'est pas de type SIMPLE-STRING." :
             ""
            );
    }

# Macro: Überprüft ein Index-Argument
# test_index(woher,wohin_zuweisung,def,default,vergleich,grenze,ucname,lcname)
# woher : expression, woher der Index (als object) kommt.
# wohin_zuweisung : weist das Ergebnis (als uintL) zu.
# def : 0 wenn nicht auf Defaultwerte zu testen ist,
#       1 wenn bei unbound der Default eingesetzt wird,
#       2 wenn bei unbound oder NIL der Default eingesetzt wird.
# default : expression, die als Defaultwert in diesem Falle dient.
# grenze : obere Grenze
# vergleich : Vergleich mit der oberen Grenze
# ucname,lcname : Zusätzliche Identifikation des Index in Groß- bzw. Kleinbuchstaben
  #define test_index(woher,wohin_zuweisung,def,default,vergleich,grenze,ucname,lcname)  \
    { var reg1 object index = woher; # Index-Argument                           \
      if (def && ((eq(index,unbound)) || ((def==2) && (eq(index,NIL)))))        \
        { wohin_zuweisung default; }                                            \
        else                                                                    \
        { # muß ein Integer sein:                                               \
          if (!integerp(index))                                                 \
            { pushSTACK(index); pushSTACK(TheSubr(subr_self)->name);            \
              fehler(def==2                                                     \
                     ? (DEUTSCH ? "~: " ucname "Index muß NIL oder ein Integer sein, nicht ~." : \
                        ENGLISH ? "~: " lcname "index should be NIL or an integer, not ~" : \
                        FRANCAIS ? "~: L'index " lcname " doit être NIL ou un entier et non ~." : \
                        ""                                                      \
                       )                                                        \
                     : (DEUTSCH ? "~: " ucname "Index muß ein Integer sein, nicht ~." : \
                        ENGLISH ? "~: " lcname "index should be an integer, not ~" : \
                        FRANCAIS ? "~: L'index " lcname " doit être un entier et non ~." : \
                        ""                                                      \
                       )                                                        \
                    );                                                          \
            }                                                                   \
          # index ist ein Integer.                                              \
          if (!(positivep(index)))                                              \
            { pushSTACK(index); pushSTACK(TheSubr(subr_self)->name);            \
              fehler(                                                           \
                     DEUTSCH ? "~: " ucname "Index muß >=0 sein, nicht ~." :    \
                     ENGLISH ? "~: " lcname "index should not be negative: ~" : \
                     FRANCAIS ? "~: L'index doit être positif ou zéro et non ~." : \
                     ""                                                         \
                    );                                                          \
            }                                                                   \
          # index ist >=0.                                                      \
          if (!((posfixnump(index)) &&                                          \
                ((wohin_zuweisung posfixnum_to_L(index)) vergleich grenze)      \
             ) )                                                                \
            { pushSTACK(index); pushSTACK(TheSubr(subr_self)->name);            \
              if (0 vergleich 0)                                                \
                # "<= grenze" - Vergleich nicht erfüllt                         \
                fehler(                                                         \
                       DEUTSCH ? "~: " ucname "Index ~ darf die Stringlänge nicht überschreiten." : \
                       ENGLISH ? "~: " lcname "index ~ should not be greater than the length of the string" : \
                       FRANCAIS ? "~: L'index " ucname " ~ ne peut pas être plus grand que la longueur de la chaîne." : \
                       ""                                                       \
                      );                                                        \
                else                                                            \
                # "< grenze" - Vergleich nicht erfüllt                          \
                fehler(                                                         \
                       DEUTSCH ? "~: " ucname "Index ~ muß kleiner als die Stringlänge sein." : \
                       ENGLISH ? "~: " lcname "index ~ should be less than the length of the string" : \
                       FRANCAIS ? "~: L'index " ucname " ~ doit être plus petit que la longueur de la chaîne." : \
                       ""                                                       \
                      );                                                        \
            }                                                                   \
    }   }

# UP: Überprüft ein Index-Argument für Stringfunktionen
# > STACK_0: Argument
# > charptr: Ab hier kommen die Characters des Strings
# > len: Länge des Strings (< array-total-size-limit)
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Pointer auf das angesprochene Character
  local uintB* test_index_arg (uintB* charptr, uintL len);
  local uintB* test_index_arg(charptr,len)
    var reg3 uintB* charptr;
    var reg2 uintL len;
    { var reg4 uintL i;
      # i := Index STACK_0, kein Defaultwert nötig, muß <len sein:
      test_index(STACK_0,i=,0,0,<,len,"","");
      return &charptr[i];
    }

LISPFUNN(char,2) # (CHAR string index), CLTL S. 300
  { var reg3 object string = STACK_1; # string-Argument
    if (!(stringp(string))) fehler_string(string); # muß ein String sein
   {var uintL len;
    var reg2 uintB* charptr = unpack_string(string,&len); # zu den Characters vorrücken
    charptr = test_index_arg(charptr,len); # zum vom Index angesprochenen Element gehen
    value1 = code_char(*charptr); mv_count=1; # Character herausgreifen
    skipSTACK(2);
  }}

LISPFUNN(schar,2) # (SCHAR string integer), CLTL S. 300
  { var reg2 object string = STACK_1; # string-Argument
    if (!(simple_string_p(string))) fehler_sstring(string); # muß ein Simple-String sein
    # zum vom Index angesprochenen Element gehen
   {var reg1 uintB* charptr = test_index_arg(&TheSstring(string)->data[0],TheSstring(string)->length);
    value1 = code_char(*charptr); mv_count=1; # Character herausgreifen
    skipSTACK(2);
  }}

# UP: Überprüft ein in einen String einzusetzendes Character
# test_newchar_arg()
# > STACK_0: Argument
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Argument als String-Char
# erhöht STACK um 1
  local object test_newchar_arg (void);
  local object test_newchar_arg()
    { var reg1 object arg = popSTACK(); # Argument
      if (string_char_p(arg))
        return arg;
        else
        { pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: Argument muß ein String-Char sein, nicht ~." :
                 ENGLISH ? "~: argument should be a string-char, not ~" :
                 FRANCAIS ? "~: L'argument doit être de type STRING-CHAR et non ~." :
                 ""
                );
        }
    }

LISPFUNN(store_char,3) # (SYSTEM::STORE-CHAR string index newchar)
                       # = (SETF (CHAR string index) newchar), CLTL S. 300
  { var reg4 object newchar = test_newchar_arg(); # newchar-Argument
    var reg3 object string = STACK_1; # string-Argument
    if (!(stringp(string))) fehler_string(string); # muß ein String sein
   {var uintL len;
    var reg2 uintB* charptr = unpack_string(string,&len); # zu den Characters vorrücken
    charptr = test_index_arg(charptr,len); # zum vom Index angesprochenen Element gehen
    *charptr = char_code(newchar); # Character eintragen
    value1 = newchar; mv_count=1;
    skipSTACK(2);
  }}

LISPFUNN(store_schar,3) # (SYSTEM::STORE-SCHAR simple-string index newchar)
                        # = (SETF (SCHAR simple-string index) newchar), CLTL S. 300
  { var reg4 object newchar = test_newchar_arg(); # newchar-Argument
    var reg2 object string = STACK_1; # string-Argument
    if (!(simple_string_p(string))) fehler_sstring(string); # muß ein Simple-String sein
    # zum vom Index angesprochenen Element gehen
   {var reg1 uintB* charptr = test_index_arg(&TheSstring(string)->data[0],TheSstring(string)->length);
    *charptr = char_code(newchar); # Character eintragen
    value1 = newchar; mv_count=1;
    skipSTACK(2);
  }}

# UP: Überprüft die Grenzen für ein String-Argument
# test_string_limits(&string,&start,&len)
# > STACK_2: String-Argument
# > STACK_1: optionales :start-Argument
# > STACK_0: optionales :end-Argument
# > subr_self: Aufrufer (ein SUBR)
# < object string: String
# < uintL start: Wert des :start-Arguments
# < uintL len: Anzahl der angesprochenen Characters
# < uintB* ergebnis: Ab hier kommen die angesprochenen Characters
# erhöht STACK um 3
  global uintB* test_string_limits (object* string_, uintL* start_, uintL* len_);
  global uintB* test_string_limits(string_,start_,len_)
    var reg4 object* string_;
    var reg5 uintL* start_;
    var reg6 uintL* len_;
    { var reg3 uintB* charptr;
      var uintL len;
      var reg1 uintL start;
      var reg2 uintL end;
      # String-Argument überprüfen:
      { var reg1 object string = STACK_2;
        if (!(stringp(string))) fehler_string(string);
        charptr = unpack_string(string,&len);
        *string_ = string; # String herausgeben
      }
      # Nun ist len die Länge (<2^oint_addr_len), und ab charptr kommen die Zeichen.
      # :START-Argument überprüfen:
        # start := Index STACK_1, Defaultwert 0, muß <=len sein:
        test_index(STACK_1,start=,1,0,<=,len,":START-",":start-");
      # start ist jetzt der Wert des :START-Arguments.
      # :END-Argument überprüfen:
        # end := Index STACK_0, Defaultwert len, muß <=len sein:
        test_index(STACK_0,end=,2,len,<=,len,":END-",":end-");
      # end ist jetzt der Wert des :END-Arguments.
      # Vergleiche :START und :END Argumente:
      if (!(start <= end))
        { pushSTACK(STACK_0); # :END-Index
          pushSTACK(STACK_2); # :START-Index
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: :START-Index ~ darf den :END-Index ~ nicht überschreiten." :
                 ENGLISH ? "~: :start-index ~ must not be greater than :end-index ~" :
                 FRANCAIS ? "~: L'index :START ~ ne doit pas être supérieur à l'index :END ~." :
                 ""
                );
        }
      skipSTACK(3);
      # Ergebnisse herausgeben:
      *start_ = start; *len_ = end-start; return &charptr[start];
    }

# UP: Überprüft ein String/Symbol/Character-Argument
# > obj: Argument
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Argument als String
# kann GC auslösen
  local object test_stringsymchar_arg (object obj);
  local object test_stringsymchar_arg(obj)
    var reg1 object obj;
    { if (stringp(obj)) return obj; # String: unverändert zurück
      if (symbolp(obj)) return TheSymbol(obj)->pname; # Symbol: Printnamen verwenden
      if (string_char_p(obj)) # String-Char: einelementigen String daraus machen:
        { var reg1 object new_string = allocate_string(1);
          TheSstring(new_string)->data[0] = char_code(obj);
          return new_string;
        }
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Argument muß ein String, Symbol oder String-Char sein, nicht ~." :
             ENGLISH ? "~: argument ~ should be a string, a symbol or a string-char" :
             FRANCAIS ? "~: L'argument ~ doit être de type STRING, SYMBOL ou STRING-CHAR et non ~." :
             ""
            );
    }

# UP: Überprüft die Grenzen für 1 String/Symbol-Argument und kopiert es
# test_1_stringsym_limits(&string,&len)
# > STACK_2: String/Symbol-Argument
# > STACK_1: optionales :start-Argument
# > STACK_0: optionales :end-Argument
# > subr_self: Aufrufer (ein SUBR)
# < object string: Kopie des Strings
# < uintL len: Anzahl der angesprochenen Characters
# < uintB* ergebnis: Ab hier kommen die angesprochenen Characters
# erhöht STACK um 3
# kann GC auslösen
  local uintB* test_1_stringsym_limits (object* string_, uintL* len_);
  local uintB* test_1_stringsym_limits(string_,len_)
    var reg5 object* string_;
    var reg6 uintL* len_;
    { var reg4 object string;
      var reg3 uintL len;
      var reg1 uintL start;
      var reg2 uintL end;
      # String/Symbol-Argument überprüfen:
      string = test_stringsymchar_arg(STACK_2);
      len = vector_length(string);
      # Nun ist len die Länge (<2^oint_addr_len).
      # :START-Argument überprüfen:
        # start := Index STACK_1, Defaultwert 0, muß <=len sein:
        test_index(STACK_1,start=,1,0,<=,len,":START-",":start-");
      # start ist jetzt der Wert des :START-Arguments.
      # :END-Argument überprüfen:
        # end := Index STACK_0, Defaultwert len, muß <=len sein:
        test_index(STACK_0,end=,2,len,<=,len,":END-",":end-");
      # end ist jetzt der Wert des :END-Arguments.
      # Vergleiche :START und :END Argumente:
      if (!(start <= end))
        { pushSTACK(STACK_0); # :END-Index
          pushSTACK(STACK_2); # :START-Index
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: :START-Index ~ darf den :END-Index ~ nicht überschreiten." :
                 ENGLISH ? "~: :start-index ~ must not be greater than :end-index ~" :
                 FRANCAIS ? "~: L'index :START ~ ne doit pas être supérieur à l'index :END ~." :
                 ""
                );
        }
      skipSTACK(3);
      # String kopieren und Ergebnisse herausgeben:
      *string_ = string = copy_string(string); # String kopieren
      *len_ = end-start; return &TheSstring(string)->data[start];
    }

# UP: Überprüft die Grenzen für 2 String/Symbol-Argumente
# test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2)
# > STACK_5: String/Symbol-Argument1
# > STACK_4: String/Symbol-Argument2
# > STACK_3: optionales :start1-Argument
# > STACK_2: optionales :end1-Argument
# > STACK_1: optionales :start2-Argument
# > STACK_0: optionales :end2-Argument
# > subr_self: Aufrufer (ein SUBR)
# < uintB* charptr1: Ab hier kommen die angesprochenen Characters im String1
# < uintL len1: Anzahl der angesprochenen Characters im String1
# < uintB* charptr2: Ab hier kommen die angesprochenen Characters im String2
# < uintL len2: Anzahl der angesprochenen Characters im String2
# < ergebnis: Wert des :start2-Arguments
# erhöht STACK um 6
  local uintL test_2_stringsym_limits (uintB** charptr1_, uintL* len1_, uintB** charptr2_, uintL* len2_);
  local uintL test_2_stringsym_limits(charptr1_,len1_,charptr2_,len2_)
    var reg4 uintB** charptr1_;
    var reg5 uintL* len1_;
    var reg4 uintB** charptr2_;
    var reg5 uintL* len2_;
    { var uintL len1;
      var uintL len2;
      { # String/Symbol-Argument1 überprüfen:
        var reg1 object string1 = test_stringsymchar_arg(STACK_5);
        pushSTACK(string1); # string1 retten
        # String/Symbol-Argument2 überprüfen:
       {var reg2 object string2 = test_stringsymchar_arg(STACK_(4+1));
        *charptr2_ = unpack_string(string2,&len2);
        # Nun ist len2 die Länge (<2^oint_addr_len) von string2, und ab charptr2 kommen die Zeichen.
        string1 = popSTACK(); # string1 zurück
        *charptr1_ = unpack_string(string1,&len1);
        # Nun ist len1 die Länge (<2^oint_addr_len) von string1, und ab charptr1 kommen die Zeichen.
      }}
      # :START1 und :END1 überprüfen:
      { var reg3 uintL start1;
        var reg2 uintL end1;
        # :START1-Argument überprüfen:
          # start1 := Index STACK_3, Defaultwert 0, muß <=len1 sein:
          test_index(STACK_3,start1=,1,0,<=,len1,":START1-",":start1-");
        # start1 ist jetzt der Wert des :START1-Arguments.
        # :END1-Argument überprüfen:
          # end1 := Index STACK_2, Defaultwert len1, muß <=len1 sein:
          test_index(STACK_2,end1=,2,len1,<=,len1,":END1-",":end1-");
        # end1 ist jetzt der Wert des :END1-Arguments.
        # Vergleiche :START1 und :END1 Argumente:
        if (!(start1 <= end1))
          { pushSTACK(STACK_2); # :END1-Index
            pushSTACK(STACK_4); # :START1-Index
            pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~: :START1-Index ~ darf den :END1-Index ~ nicht überschreiten." :
                   ENGLISH ? "~: :start1-index ~ must not be greater than :end1-index ~" :
                   FRANCAIS ? "~: L'index :START1 ~ ne doit pas être supérieur à l'index :END1 ~." :
                   ""
                  );
          }
        # Ergebnisse zu string1 herausgeben:
        *charptr1_ += start1; *len1_ = end1-start1;
      }
      # :START2 und :END2 überprüfen:
      { var reg3 uintL start2;
        var reg2 uintL end2;
        # :START2-Argument überprüfen:
          # start2 := Index STACK_1, Defaultwert 0, muß <=len2 sein:
          test_index(STACK_1,start2=,1,0,<=,len2,":START2-",":start2-");
        # start2 ist jetzt der Wert des :START2-Arguments.
        # :END2-Argument überprüfen:
          # end2 := Index STACK_0, Defaultwert len2, muß <=len2 sein:
          test_index(STACK_0,end2=,2,len2,<=,len2,":END2-",":end2-");
        # end2 ist jetzt der Wert des :END2-Arguments.
        # Vergleiche :START2 und :END2 Argumente:
        if (!(start2 <= end2))
          { pushSTACK(STACK_0); # :END2-Index
            pushSTACK(STACK_2); # :START2-Index
            pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~: :START2-Index ~ darf den :END2-Index ~ nicht überschreiten." :
                   ENGLISH ? "~: :start2-index ~ must not be greater than :end2-index ~" :
                   FRANCAIS ? "~: L'index :START2 ~ ne doit pas être supérieur à l'index :END2 ~." :
                   ""
                  );
          }
        # Ergebnisse zu string2 herausgeben:
        *charptr2_ += start2; *len2_ = end2-start2;
        # Fertig.
        skipSTACK(6);
        return start2;
    } }

# UP: vergleicht zwei gleichlange Strings auf Gleichheit
# > charptr1: Ab hier kommen die angesprochenen Characters im String1
# > charptr2: Ab hier kommen die angesprochenen Characters im String2
# > len: Anzahl der angesprochenen Characters in String1 und in String2
# < ergebnis: TRUE falls gleich, FALSE sonst.
  local boolean string_eqcomp (uintB* charptr1, uintB* charptr2, uintL len);
  local boolean string_eqcomp(charptr1,charptr2,len)
    var reg1 uintB* charptr1;
    var reg2 uintB* charptr2;
    var reg3 uintL len;
    { dotimesL(len,len, { if (!(*charptr1++ == *charptr2++)) goto no; } );
      return TRUE;
      no: return FALSE;
    }

# UP: vergleicht zwei Strings
# > charptr1: Ab hier kommen die angesprochenen Characters im String1
# > len1: Anzahl der angesprochenen Characters im String1
# > charptr2: Ab hier kommen die angesprochenen Characters im String2
# > len2: Anzahl der angesprochenen Characters im String2
# < ergebnis: 0 falls gleich,
#             -1 falls String1 echt vor String2 kommt,
#             +1 falls String1 echt nach String2 kommt.
  local signean string_comp (uintB* charptr1, uintL len1, uintB* charptr2, uintL len2);
  local signean string_comp(charptr1,len1,charptr2,len2)
    var reg2 uintB* charptr1;
    var reg3 uintL len1;
    var reg2 uintB* charptr2;
    var reg3 uintL len2;
    { loop
        { # einer der Strings zu Ende ?
          if (len1==0) goto string1_end;
          if (len2==0) goto string2_end;
          # nächste Characters vergleichen:
          if (!(*charptr1++ == *charptr2++)) break;
          # beide Zähler erniedrigen:
          len1--; len2--;
        }
      # zwei verschiedene Characters gefunden
      if (*--charptr1 < *--charptr2)
        return signean_minus; # String1 < String2
        else
        return signean_plus; # String1 > String2
      string1_end: # String1 zu Ende
        if (len2==0)
          return signean_null; # String1 = String2
          else
          return signean_minus; # String1 ist echtes Anfangsstück von String2
      string2_end: # String2 zu Ende, String1 noch nicht
        return signean_plus; # String2 ist echtes Anfangsstück von String1
    }

LISPFUN(string_gleich,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING= string1 string2 :start1 :end1 :start2 :end2), CLTL S. 300
  { var uintB* charptr1;
    var uintL len1;
    var uintB* charptr2;
    var uintL len2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2);
    # vergleichen:
    value1 = (((len1==len2) && string_eqcomp(charptr1,charptr2,len1)) ? T : NIL);
    mv_count=1;
  }

LISPFUN(string_ungleich,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING/= string1 string2 :start1 :end1 :start2 :end2), CLTL S. 301
  { var uintB* charptr1;
    var uintL len1;
    var uintB* charptr2;
    var uintL len2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2);
    # vergleichen:
    value1 = (((len1==len2) && string_eqcomp(charptr1,charptr2,len1)) ? NIL : T);
    mv_count=1;
  }

LISPFUN(string_kleiner,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING< string1 string2 :start1 :end1 :start2 :end2), CLTL S. 301
  { var uintB* charptr1;
    var uintL len1;
    var uintB* charptr2;
    var uintL len2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2);
    # vergleichen:
    value1 = (string_comp(charptr1,len1,charptr2,len2)<0 ? T : NIL);
    mv_count=1;
  }

LISPFUN(string_groesser,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING> string1 string2 :start1 :end1 :start2 :end2), CLTL S. 301
  { var uintB* charptr1;
    var uintL len1;
    var uintB* charptr2;
    var uintL len2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2);
    # vergleichen:
    value1 = (string_comp(charptr1,len1,charptr2,len2)>0 ? T : NIL);
    mv_count=1;
  }

LISPFUN(string_klgleich,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING<= string1 string2 :start1 :end1 :start2 :end2), CLTL S. 301
  { var uintB* charptr1;
    var uintL len1;
    var uintB* charptr2;
    var uintL len2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2);
    # vergleichen:
    value1 = (string_comp(charptr1,len1,charptr2,len2)<=0 ? T : NIL);
    mv_count=1;
  }

LISPFUN(string_grgleich,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING>= string1 string2 :start1 :end1 :start2 :end2), CLTL S. 301
  { var uintB* charptr1;
    var uintL len1;
    var uintB* charptr2;
    var uintL len2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2);
    # vergleichen:
    value1 = (string_comp(charptr1,len1,charptr2,len2)>=0 ? T : NIL);
    mv_count=1;
  }

# UP: vergleicht zwei gleichlange Strings auf Gleichheit, case-insensitive
# > charptr1: Ab hier kommen die angesprochenen Characters im String1
# > charptr2: Ab hier kommen die angesprochenen Characters im String2
# > len: Anzahl der angesprochenen Characters in String1 und in String2
# < ergebnis: TRUE falls gleich, FALSE sonst.
  local boolean string_eqcomp_ci (uintB* charptr1, uintB* charptr2, uintL len);
  local boolean string_eqcomp_ci(charptr1,charptr2,len)
    var reg1 uintB* charptr1;
    var reg2 uintB* charptr2;
    var reg3 uintL len;
    { dotimesL(len,len,
        { if (!(up_case(*charptr1++) == up_case(*charptr2++))) goto no; }
        );
      return TRUE;
      no: return FALSE;
    }

# UP: vergleicht zwei Strings, case-insensitive
# > charptr1: Ab hier kommen die angesprochenen Characters im String1
# > len1: Anzahl der angesprochenen Characters im String1
# > charptr2: Ab hier kommen die angesprochenen Characters im String2
# > len2: Anzahl der angesprochenen Characters im String2
# < ergebnis: 0 falls gleich,
#             -1 falls String1 echt vor String2 kommt,
#             +1 falls String1 echt nach String2 kommt.
  local signean string_comp_ci (uintB* charptr1, uintL len1, uintB* charptr2, uintL len2);
  local signean string_comp_ci(charptr1,len1,charptr2,len2)
    var reg2 uintB* charptr1;
    var reg3 uintL len1;
    var reg2 uintB* charptr2;
    var reg3 uintL len2;
    { var reg1 uintB ch1;
      var reg1 uintB ch2;
      loop
        { # einer der Strings zu Ende ?
          if (len1==0) goto string1_end;
          if (len2==0) goto string2_end;
          # nächste Characters vergleichen:
          if (!((ch1 = up_case(*charptr1++)) == (ch2 = up_case(*charptr2++)))) break;
          # beide Zähler erniedrigen:
          len1--; len2--;
        }
      # zwei verschiedene Characters gefunden
      if (ch1 < ch2)
        return signean_minus; # String1 < String2
        else
        return signean_plus; # String1 > String2
      string1_end: # String1 zu Ende
        if (len2==0)
          return signean_null; # String1 = String2
          else
          return signean_minus; # String1 ist echtes Anfangsstück von String2
      string2_end: # String2 zu Ende, String1 noch nicht
        return signean_plus; # String2 ist echtes Anfangsstück von String1
    }

LISPFUN(string_equal,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING-EQUAL string1 string2 :start1 :end1 :start2 :end2), CLTL S. 301
  { var uintB* charptr1;
    var uintL len1;
    var uintB* charptr2;
    var uintL len2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2);
    # vergleichen:
    value1 = (((len1==len2) && string_eqcomp_ci(charptr1,charptr2,len1)) ? T : NIL);
    mv_count=1;
  }

LISPFUN(string_not_equal,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING-NOT-EQUAL string1 string2 :start1 :end1 :start2 :end2), CLTL S. 302
  { var uintB* charptr1;
    var uintL len1;
    var uintB* charptr2;
    var uintL len2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2);
    # vergleichen:
    value1 = (((len1==len2) && string_eqcomp_ci(charptr1,charptr2,len1)) ? NIL : T);
    mv_count=1;
  }

LISPFUN(string_lessp,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING-LESSP string1 string2 :start1 :end1 :start2 :end2), CLTL S. 302
  { var uintB* charptr1;
    var uintL len1;
    var uintB* charptr2;
    var uintL len2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2);
    # vergleichen:
    value1 = (string_comp_ci(charptr1,len1,charptr2,len2)<0 ? T : NIL);
    mv_count=1;
  }

LISPFUN(string_greaterp,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING-GREATERP string1 string2 :start1 :end1 :start2 :end2), CLTL S. 302
  { var uintB* charptr1;
    var uintL len1;
    var uintB* charptr2;
    var uintL len2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2);
    # vergleichen:
    value1 = (string_comp_ci(charptr1,len1,charptr2,len2)>0 ? T : NIL);
    mv_count=1;
  }

LISPFUN(string_not_greaterp,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING-NOT-GREATERP string1 string2 :start1 :end1 :start2 :end2), CLTL S. 302
  { var uintB* charptr1;
    var uintL len1;
    var uintB* charptr2;
    var uintL len2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2);
    # vergleichen:
    value1 = (string_comp_ci(charptr1,len1,charptr2,len2)<=0 ? T : NIL);
    mv_count=1;
  }

LISPFUN(string_not_lessp,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (STRING-NOT-LESSP string1 string2 :start1 :end1 :start2 :end2), CLTL S. 302
  { var uintB* charptr1;
    var uintL len1;
    var uintB* charptr2;
    var uintL len2;
    # Argumente überprüfen:
    test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2);
    # vergleichen:
    value1 = (string_comp_ci(charptr1,len1,charptr2,len2)>=0 ? T : NIL);
    mv_count=1;
  }

# UP: sucht einen String String1 in einem anderen String String2
# > charptr1: Ab hier kommen die angesprochenen Characters im String1
# > len1: Anzahl der angesprochenen Characters im String1
# > charptr2: Ab hier kommen die angesprochenen Characters im String2
# > len2: Anzahl der angesprochenen Characters im String2
# > start2: Startposition im String2
# > eqcomp: Vergleichsfunktion, &string_eqcomp oder &string_eqcomp_ci
# < ergebnis: NIL falls nicht gefunden,
#             Position im String2 (als Fixnum) falls gefunden.
  # eqcomp_fun sei der Typ einer solchen Vergleichsfunktion:
  typedef boolean (*eqcomp_fun) (uintB* charptr1, uintB* charptr2, uintL len);
  local object string_search(uintB* charptr1, uintL len1, uintB* charptr2, uintL len2, uintL start2, eqcomp_fun eqcomp);
  local object string_search(charptr1,len1,charptr2,len2,start2,eqcomp)
    var reg3 uintB* charptr1;
    var reg5 uintL len1;
    var reg1 uintB* charptr2;
    var reg7 uintL len2;
    var reg6 uintL start2;
    var reg5 eqcomp_fun eqcomp;
    { var reg2 uintL count;
      if (len1>len2) goto notfound; # Nur bei len1<=len2 kann String1 in String2 vorkommen.
      # Schleife:
      # for i=0..len2-len1:
      #   vergleiche String1 mit den len1 Characters ab charptr2[i].
      # Dazu Schleife len2-len1+1 mal durchlaufen, charptr2 und start2 wachsen.
      dotimespL(count,len2-len1+1,
        { if ((*eqcomp)(charptr1,charptr2,len1)) goto found; # vergleichen
          charptr2++; # weiterrücken
          start2++; # und Position von charptr2 mitzählen
        });
      notfound: return NIL;
      found: return fixnum(start2);
    }

LISPFUN(search_string_gleich,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (SYS::SEARCH-STRING= string1 string2 [:start1] [:end1] [:start2] [:end2])
# = (search string1 string2 :test #'char= [:start1] [:end1] [:start2] [:end2])
  { var uintB* charptr1;
    var uintL len1;
    var uintB* charptr2;
    var uintL len2;
    # Argumente überprüfen:
    var uintL start2 =
      test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2);
    # String1 in String2 suchen:
    value1 = string_search(charptr1,len1,charptr2,len2,start2,&string_eqcomp);
    mv_count=1;
  }

LISPFUN(search_string_equal,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (SYS::SEARCH-STRING-EQUAL string1 string2 [:start1] [:end1] [:start2] [:end2])
# = (search string1 string2 :test #'char-equal [:start1] [:end1] [:start2] [:end2])
  { var uintB* charptr1;
    var uintL len1;
    var uintB* charptr2;
    var uintL len2;
    # Argumente überprüfen:
    var uintL start2 =
      test_2_stringsym_limits(&charptr1,&len1,&charptr2,&len2);
    # String1 in String2 suchen:
    value1 = string_search(charptr1,len1,charptr2,len2,start2,&string_eqcomp_ci);
    mv_count=1;
  }

LISPFUN(make_string,1,0,norest,key,1, (kw(initial_element)) )
# (MAKE-STRING size :initial-element), CLTL S. 302
  { var reg2 uintL size;
    # size überprüfen:
    if (!(mposfixnump(STACK_1))) # size muß Fixnum >= 0 sein
      { pushSTACK(STACK_1); pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: ~ ist als Stringlänge nicht geeignet, da kein Fixnum >= 0." :
               ENGLISH ? "~: the string length ~ should be nonnegative fixnum" :
               FRANCAIS ? "~: La longueur de chaîne ~ doit être de type FIXNUM positif ou zéro." :
               ""
              );
      }
    size = posfixnum_to_L(STACK_1);
   {var reg5 object new_string = allocate_string(size); # neuen String besorgen
    # evtl. mit initial-element füllen:
    var reg4 object initial_element = STACK_0;
    if (eq(initial_element,unbound))
      ; # nicht angegeben -> nichts zu tun
      else
      if (!(string_char_p(initial_element))) # sonst: muß ein String-Char sein
        { # :initial-element-Argument noch in STACK_0
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: :INITIAL-ELEMENT ~ ist nicht vom Typ STRING-CHAR." :
                 ENGLISH ? "~: :initial-element ~ should be of type string-char" :
                 FRANCAIS ? "~: L'élément initial ~ n'est pas de type STRING-CHAR." :
                 ""
                );
        }
        else
        { var reg3 uintB ch = char_code(initial_element);
          # String mit ch vollschreiben:
          if (!(size==0))
            { var reg1 uintB* charptr = &TheSstring(new_string)->data[0];
              dotimespL(size,size, { *charptr++ = ch; } );
        }   }
    value1 = new_string; mv_count=1; skipSTACK(2);
  }}

LISPFUNN(string_both_trim,3)
# (SYS::STRING-BOTH-TRIM character-bag-left character-bag-right string)
# Grundfunktion für
# STRING-TRIM, STRING-LEFT-TRIM, STRING-RIGHT-TRIM, CLTL S. 302
# Methode:
# (let ((l (length string)))
#   (do ((i 0 (1+ i)))
#       (nil)
#     (when (or (= i l)
#               (not (find (char string i) character-bag-left))
#           )
#       (do ((j l (1- j)))
#           (nil)
#         (when (or (= i j)
#                   (not (find (char string (1- j)) character-bag-right))
#               )
#           (return (if (and (= i 0) (= j l)) string (substring string i j)))
# ) ) ) ) )
  { var reg3 object string = test_stringsymchar_arg(popSTACK()); # Argument in String umwandeln
    pushSTACK(string); # und wieder in den Stack
    pushSTACK(fixnum(vector_length(string))); # Länge als Fixnum in den Stack
    pushSTACK(Fixnum_0); # i := 0
    # Stackaufbau: bag-left, bag-right, string, l, i
    loop
      { if (eq(STACK_0,STACK_1)) break; # bei i = l (beides Fixnums): Schleife fertig
        # (char string i) bestimmen:
        pushSTACK(STACK_2); pushSTACK(STACK_1); funcall(L(char),2);
        # (find (char ...) character-bag-left) bestimmen:
        pushSTACK(value1); pushSTACK(STACK_5); funcall(L(find),2);
        if (nullp(value1)) break; # char nicht in character-bag-left -> Schleife fertig
        STACK_0 = fixnum_inc(STACK_0,1); # i := (1+ i)
      }
    pushSTACK(STACK_1); # j := l
    # Stackaufbau: bag-left, bag-right, string, l, i, j
    loop
      { if (eq(STACK_0,STACK_1)) break; # bei j = i (beides Fixnums): Schleife fertig
        # (char string (1- j)) bestimmen:
        pushSTACK(STACK_3); pushSTACK(fixnum_inc(STACK_1,-1)); funcall(L(char),2);
        # (find (char ...) character-bag-right) bestimmen:
        pushSTACK(value1); pushSTACK(STACK_5); funcall(L(find),2);
        if (nullp(value1)) break; # char nicht in character-bag-right -> Schleife fertig
        STACK_0 = fixnum_inc(STACK_0,-1); # j := (1- j)
      }
    # Stackaufbau: bag-left, bag-right, string, l, i, j
    # Die Zeichen mit Index <i oder >=j des Strings wegwerfen:
    { var reg4 object j = popSTACK();
      var reg4 object i = popSTACK();
      var reg4 object l = popSTACK();
      string = popSTACK();
      skipSTACK(2);
      if (eq(i,Fixnum_0) && eq(j,l))
        { value1 = string; } # bei i=0 und j=l ist nichts zu tun, string als Wert
        else
        { # Teilstück der Indizes >=i, <j herauskopieren:
          # (substring string i j) als Wert
          pushSTACK(string); pushSTACK(i); pushSTACK(j); funcall(L(substring),3);
        }
      mv_count=1;
  } }

# UP: wandelt die Characters eines Stringstücks in Großbuchstaben
# nstring_upcase(charptr,len);
# > uintB* charptr: Ab hier kommen die angesprochenen Characters
# > uintL len: Anzahl der angesprochenen Characters
  local void nstring_upcase (uintB* charptr, uintL len);
  local void nstring_upcase(charptr,len)
    var reg1 uintB* charptr;
    var reg2 uintL len;
    { dotimesL(len,len, { *charptr = up_case(*charptr); charptr++; } ); }

LISPFUN(nstring_upcase,1,0,norest,key,2, (kw(start),kw(end)) )
# (NSTRING-UPCASE string :start :end), CLTL S. 304
  { var object string;
    var local uintL start; # unbenutzt
    var uintL len;
    var reg1 uintB* charptr = test_string_limits(&string,&start,&len);
    nstring_upcase(charptr,len);
    value1 = string; mv_count=1;
  }

LISPFUN(string_upcase,1,0,norest,key,2, (kw(start),kw(end)) )
# (STRING-UPCASE string :start :end), CLTL S. 303
  { var object string;
    var uintL len;
    var reg1 uintB* charptr = test_1_stringsym_limits(&string,&len);
    nstring_upcase(charptr,len);
    value1 = string; mv_count=1;
  }

# UP: wandelt die Characters eines Stringstücks in Kleinbuchstaben
# nstring_downcase(charptr,len);
# > uintB* charptr: Ab hier kommen die angesprochenen Characters
# > uintL len: Anzahl der angesprochenen Characters
  global void nstring_downcase (uintB* charptr, uintL len);
  global void nstring_downcase(charptr,len)
    var reg1 uintB* charptr;
    var reg2 uintL len;
    { dotimesL(len,len, { *charptr = down_case(*charptr); charptr++; } ); }

LISPFUN(nstring_downcase,1,0,norest,key,2, (kw(start),kw(end)) )
# (NSTRING-DOWNCASE string :start :end), CLTL S. 304
  { var object string;
    var local uintL start; # unbenutzt
    var uintL len;
    var reg1 uintB* charptr = test_string_limits(&string,&start,&len);
    nstring_downcase(charptr,len);
    value1 = string; mv_count=1;
  }

LISPFUN(string_downcase,1,0,norest,key,2, (kw(start),kw(end)) )
# (STRING-DOWNCASE string :start :end), CLTL S. 303
  { var object string;
    var uintL len;
    var reg1 uintB* charptr = test_1_stringsym_limits(&string,&len);
    nstring_downcase(charptr,len);
    value1 = string; mv_count=1;
  }

# UP: wandelt die Worte eines Stringstücks in solche, die
# mit Großbuchstaben anfangen und mit Kleinbuchstaben weitergehen.
# nstring_capitalize(charptr,len);
# > uintB* charptr: Ab hier kommen die angesprochenen Characters
# > uintL len: Anzahl der angesprochenen Characters
  global void nstring_capitalize (uintB* charptr, uintL len);
  # Methode:
  # Jeweils abwechselnd nach Wortanfang suchen (und nichts umwandeln)
  # bzw. nach Wortende suchen (und dabei umwandeln).
  global void nstring_capitalize(charptr,len)
    var reg1 uintB* charptr;
    var reg2 uintL len;
    { # Suche den nächsten Wortanfang:
      suche_wortanfang:
        until (len==0)
          { if (alphanumericp(*charptr)) goto wortanfang;
            charptr++; len--;
          }
        return; # len=0 -> String zu Ende
      # Wortanfang gefunden
      wortanfang:
        *charptr = up_case(*charptr); # Zeichen in Großbuchstaben umwandeln
        charptr++;
        # Suche das Wortende:
        until (--len==0)
          { # mitten im Wort
            if (!(alphanumericp(*charptr))) goto suche_wortanfang;
            *charptr = down_case(*charptr); # Zeichen in Kleinbuchstaben umwandeln
            charptr++;
          }
        return; # len=0 -> String zu Ende
    }

LISPFUN(nstring_capitalize,1,0,norest,key,2, (kw(start),kw(end)) )
# (NSTRING-CAPITALIZE string :start :end), CLTL S. 304
  { var object string;
    var local uintL start; # unbenutzt
    var uintL len;
    var reg1 uintB* charptr = test_string_limits(&string,&start,&len);
    nstring_capitalize(charptr,len);
    value1 = string; mv_count=1;
  }

LISPFUN(string_capitalize,1,0,norest,key,2, (kw(start),kw(end)) )
# (STRING-CAPITALIZE string :start :end), CLTL S. 303
  { var object string;
    var uintL len;
    var reg1 uintB* charptr = test_1_stringsym_limits(&string,&len);
    nstring_capitalize(charptr,len);
    value1 = string; mv_count=1;
  }

LISPFUNN(string,1) # (STRING object), CLTL S. 304
  { value1 = test_stringsymchar_arg(popSTACK()); mv_count=1; }

LISPFUNN(name_char,1) # (NAME-CHAR name), CLTL S. 243
  { # Argument in einen String umwandeln, Character mit diesem Namen suchen:
    value1 = name_char(test_stringsymchar_arg(popSTACK()));
    mv_count=1;
  }

LISPFUN(substring,2,1,norest,nokey,0,NIL)
# (SUBSTRING string start [end]) wie SUBSEQ, aber nur für Strings
  { var reg4 object string;
    var reg3 uintL len;
    var reg1 uintL start;
    var reg2 uintL end;
    # String/Symbol-Argument überprüfen:
    string = test_stringsymchar_arg(STACK_2);
    len = vector_length(string);
    # Nun ist len die Länge (<2^oint_addr_len).
    # :START-Argument überprüfen:
      # start := Index STACK_1, Defaultwert 0, muß <=len sein:
      test_index(STACK_1,start=,1,0,<=,len,":START-",":start-");
    # start ist jetzt der Wert des :START-Arguments.
    # :END-Argument überprüfen:
      # end := Index STACK_0, Defaultwert len, muß <=len sein:
      test_index(STACK_0,end=,2,len,<=,len,":END-",":end-");
    # end ist jetzt der Wert des :END-Arguments.
    # Vergleiche :START und :END Argumente:
    if (!(start <= end))
      { pushSTACK(STACK_0); # :END-Index
        pushSTACK(STACK_2); # :START-Index
        pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: :START-Index ~ darf den :END-Index ~ nicht überschreiten." :
               ENGLISH ? "~: :start-index ~ must not be greater than :end-index ~" :
               FRANCAIS ? "~: L'index :START ~ ne doit pas être supérieur à l'index :END ~." :
               ""
              );
      }
    skipSTACK(3);
    # Teilstring herausziehen:
    pushSTACK(string); # alten String retten
   {var reg2 uintL count = end-start; # Anzahl der zu kopierenden Characters
    var reg5 object new_string = allocate_string(count); # neuer String
    string = popSTACK(); # alter String
    {var uintL len; # nochmals die Länge des alten Strings
     var uintB* charptr1 = unpack_string(string,&len) + start;
     var uintB* charptr2 = &TheSstring(new_string)->data[0];
     dotimesL(count,count, { *charptr2++ = *charptr1++; } );
    }
    value1 = new_string; mv_count=1;
  }}

# UP: bildet einen aus mehreren Strings zusammengehängten String.
# string_concat(argcount)
# > uintC argcount: Anzahl der Argumente
# > auf dem STACK: die Argumente (sollten Strings sein)
# > subr_self: Aufrufer (ein SUBR) (unnötig, falls alle Argumente Strings sind)
# < ergebnis: Gesamtstring, neu erzeugt
# < STACK: aufgeräumt
# kann GC auslösen
  global object string_concat (uintC argcount);
  global object string_concat(argcount)
    var reg8 uintC argcount;
    { var reg9 object* args_pointer = (args_end_pointer STACKop (uintL)argcount);
      # args_pointer = Pointer über die Argumente
      # Überprüfe, ob es alles Strings sind, und addiere die Längen:
      var reg9 uintL total_length = 0;
      { var reg2 object* argptr = args_pointer;
        var reg3 uintC count;
        dotimesC(count,argcount,
          { var reg1 object arg = NEXT(argptr); # nächstes Argument
            if (!(stringp(arg))) fehler_string(arg);
            total_length += vector_length(arg);
          });
      }
      # total_length ist jetzt die Gesamtlänge.
      { var reg6 object new_string = allocate_string(total_length); # neuer String
        var reg1 uintB* charptr2 = &TheSstring(new_string)->data[0];
        var reg5 object* argptr = args_pointer;
        dotimesC(argcount,argcount,
          { var reg4 object arg = NEXT(argptr); # nächster Argument-String
            var uintL len; # dessen Länge
            var reg2 uintB* charptr1 = unpack_string(arg,&len);
            var reg3 uintL count;
            # Kopiere len Characters von charptr1 nach charptr2:
            dotimesL(count,len, { *charptr2++ = *charptr1++; } );
          });
        set_args_end_pointer(args_pointer); # STACK aufräumen
        return new_string;
    } }

LISPFUN(string_concat,0,0,rest,nokey,0,NIL)
# (STRING-CONCAT {string})
# bildet einen aus den Argumenten zusammengehängten String
  { value1 = string_concat(argcount); mv_count=1; }

