PROG=	awk
SRCS=	main.c eval.c builtin.c msg.c iop.c io.c field.c array.c \
	node.c version.c missing.c re.c awk.c regex.c dfa.c
DPADD=	${LIBM}
LDADD=	-lm
CFLAGS+= -g

.include <bsd.prog.mk>
