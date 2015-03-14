char hextab[] = "0123456789abcdef";

void
writez(fd, str)
     int fd;
     unsigned char *str;
{
  for (; *str; str++)
    write(1, str, 1);
}

#if 0
void
numout(num, base)
     unsigned long num;
     int base;
{
  if (num >= base) numout(num / base, base);
  write(1, &hextab[num % base], 1);
}
#endif

int
foo(arg)
     int arg;
{
  return arg+1;
}

main()
{
  unsigned char c = 0;

  cache_on();

  set_debug_traps();
  breakpoint();

  writez(1, "Got to here\n");

  while (1)
    {
      read(0, &c, 1);
      if (c == 'g')
	break;

#if 1
      if (c == 'd')
	{
#if 0
	  writez(1, "Entering debugger\r\n");

	  set_debug_traps();

	  writez(1, "Just did set_debug_traps()\r\n");
	  breakpoint();
#endif
	  writez(1, "Just got out of breakpoint()\r\n");
	  break;
	}
#endif
      writez(1, "echo ");
      write(1, &c, 1);
      writez(1, "\r\n");
    }

  writez(1, "Hello world\n");

  while (1)
    {
      read(0, &c, 1);

      if ((c & 0x7f) == 4)
	break;

      writez(1, "Char is ");
      write(1, &hextab[c >> 4], 1);
      write(1, &hextab[c & 0xf], 1);
      writez(1, "\r\n");
    }

  writez(1, "I escaped!\r\n");
}
