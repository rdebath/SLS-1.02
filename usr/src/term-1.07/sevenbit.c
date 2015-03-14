
#include "includes.h"
#include "debug.h"
/* gets size bits from starting from bit 'curr' in the bit stream data */

int s_2_e_buff(un_char *data, un_char *out, int len) {
    int     byte,
            bit,
            j;
    byte = 0;
    bit = 0;
    j = 0;
    do {
      out[j] = ((data[byte++] & 127) >> bit);
      out[j] |= (data[byte] & 127) << (7 - bit);
      if (++bit == 7) {
	bit = 0;
	++byte;
      }
      ++j;
    } while (byte + 1< len);
    return j;
}

int e_2_s_put(un_char *out, un_char data, int key) {
  int bit = 0;
  bit = key&7;
  key>>=3;
  if (bit ==7) {
    out[++key]=0;
    bit = 0;
  }
  out[key++] |= (data<<bit) & 127;
  out[key] = data >> (7 - bit);
  ++bit;
  return key * 8 + bit;
}

