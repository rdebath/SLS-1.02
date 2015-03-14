#ifndef WARNING_H
#define WARNING_H

extern void __libc_warning (const char *);

#define __LIBC_WARNING(name)	{ \
  static int __old_##name##_warned = 0; \
  if (!__old_##name##_warned) { \
    __old_##name##_warned++; \
    __libc_warning (#name); \
  } \
}

#endif /* WARNING_H */
