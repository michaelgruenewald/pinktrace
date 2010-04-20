dnl vim: set sw=4 sts=4 ts=4 noet ft=config foldmethod=marker foldmarker={{{,}}} :

dnl {{{ Program, version
AC_PREREQ(2.59)
AC_INIT([src/pink.c])

# Libtool versioning - only update before a public release.
# If the code has changed at all: REVISION++.
# If any interfaces have changed at all: CURRENT++, REVISION=0.
# If interfaces changed but it is backwards compatible: AGE++, else AGE=0.
#  i.e. the library supports interfaces CURRENT - AGE up to CURRENT.
#  if two libraries have identical CURRENT and AGE, then newest REVISION used.
CURRENT=0
AGE=0
REVISION=1

LT_VERSION_INFO="$CURRENT:$REVISION:$AGE"
AC_SUBST(LT_VERSION_INFO)

AM_INIT_AUTOMAKE(libpinktrace, [0.01])
dnl }}}

dnl {{{ Toolchain checks
AC_PROG_CC_C99
if test x"$ac_cv_prog_cc_c99" = x"no"; then
	AC_MSG_ERROR([libpinktrace requires a C compiler that supports ISO C99!])
fi
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_LIBTOOL
dnl }}}

dnl {{{ Extra CFLAGS
PINKTRACE_CFLAGS=
WANTED_CFLAGS="-pedantic -Wall -W -Wextra -Wbad-function-cast -Wcast-align -Wcast-qual -Wfloat-equal -Wformat=2 -Wformat-security -Wformat-nonliteral -Winit-self -Winline -Wlogical-op -Wmissing-prototypes -Wmissing-declarations -Wmissing-format-attribute -Wmissing-noreturn -Wpointer-arith -Wredundant-decls -Wshadow -Wswitch-default -Wunused -Wvla"
for flag in $WANTED_CFLAGS ; do
	AX_CHECK_COMPILER_FLAGS([$flag], [PINKTRACE_CFLAGS="$PINKTRACE_CFLAGS $flag"],)
done
AC_SUBST([PINKTRACE_CFLAGS])
dnl }}}

dnl {{{ Library checks
PKG_PROG_PKG_CONFIG([0.20.0])
PKG_CHECK_MODULES([check], [check >= 0.9.4],,)
dnl }}}

dnl {{{ Doxygen
AC_MSG_CHECKING([whether to enable doxygen])
AC_ARG_ENABLE([doxygen],
			  AS_HELP_STRING([--enable-doxygen], [Enable 'make doxygen' (developer docs)]),
			  [HAVE_DOXYGEN=$enableval
			   AC_MSG_RESULT([$enableval])],
			  [AC_MSG_RESULT([autodetect])
			   AC_CHECK_PROG(HAVE_DOXYGEN, [doxygen], [yes], [no])])
AC_SUBST([HAVE_DOXYGEN])
AM_CONDITIONAL([HAVE_DOXYGEN], test "x$HAVE_DOXYGEN" = "xyes")
dnl }}}

dnl {{{ Output
AM_CONFIG_HEADER(config.h)
AC_OUTPUT(
		  pinktrace.pc
		  Makefile
		  src/Makefile
		  tests/Makefile
		  doc/Makefile
		  doc/doxygen.conf
	)
dnl }}}
