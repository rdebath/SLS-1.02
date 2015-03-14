/* version control information */

/* Change only the part after the decimal when making local changes.
 * Prefix is changed only by N3EUA. Persons other than KA9Q making local
 * changes and fixes should append letters to the suffix.
 *  I'm starting my own version numbers for the linux version --clh
 * 3 - ambiguous -- I forgot to increment for several releases
 * 4 - clear OLFCR when in raw mode, implement first cut as X server
 * 5 - use line discipline 1 for SLIP, to use kernel packet mode
 * 6 - fix ethernet (sort of)
 * 7 - add mput, mget, fix cslip when multiple connections
 * 8 - warn when input on another session
 * 9 - telnet client
 * 10 - fix telunix
 * 11 - for telnet client, restore tty modes before killing client
 * 12 - cleanup properly if telnet client is killed oddly
 *      implement "start x ask"
 * 13 - fix some null pointers; cleanup if host lookup fails
 * 14 - I think we finally got the corrupted malloc problem
 */
char version[] = "890421.1a.linux.14 (n6xjj clh)";
