dnl vim: set sw=4 sts=4 ts=4 noet ft=config foldmethod=marker foldmarker={{{,}}} :

dnl {{{ Program, version
AC_PREREQ(2.59)
AC_INIT([src/pink-bitness.c])
AC_CANONICAL_HOST

VERSION_MAJOR=0
VERSION_MINOR=0
VERSION_MICRO=1
VERSION_SUFFIX=
VERSION_FULL="$VERSION_MAJOR.$VERSION_MINOR.$VERSION_MICRO$VERSION_SUFFIX"
VERSION="$VERSION_FULL"
VERSION_LIB_CURRENT=`expr 100 '*' $VERSION_MAJOR + $VERSION_MINOR`
VERSION_LIB_REVISION="$VERSION_MICRO"

AC_SUBST([VERSION_MAJOR])
AC_SUBST([VERSION_MINOR])
AC_SUBST([VERSION_MICRO])
AC_SUBST([VERSION_FULL])
AC_SUBST([VERSION_SUFFIX])
AC_SUBST([VERSION_LIB_CURRENT])
AC_SUBST([VERSION_LIB_REVISION])

AM_INIT_AUTOMAKE(pinktrace, [$VERSION_FULL])

dnl {{{ Git revision
if test -d "${GIT_DIR:-${ac_top_srcdir:-./}/.git}" ; then
	GITHEAD=`git describe 2>/dev/null`
	if test -z ${GITHEAD} ; then
		GITHEAD=`git rev-parse HEAD`
	fi
	if test -n "`git diff-index -m --name-only HEAD`" ; then
		GITHEAD=${GITHEAD}-dirty
	fi
else
	GITVERSION=
fi
AC_MSG_CHECKING([for git head])
AC_ARG_WITH([git-head],
	AS_HELP_STRING([--with-git-head], [Specify Git head]),
	GITHEAD=`eval echo $withval`,
	GITHEAD=`eval echo $GITHEAD`)
AC_MSG_RESULT([$GITHEAD])
AC_SUBST([GITHEAD])
dnl }}}

PINKTRACE_PC_VERSION="${VERSION_FULL}"
PINKTRACE_PC_SLOT="${VERSION_MAJOR}.${VERSION_MINOR}${VERSION_SUFFIX}"
PINKTRACE_PC_SLOT="${PINKTRACE_PC_SLOT}_"`echo ${GITHEAD} | sed -e 's,\(........\(-dirty\)\?\)$,#\1,' -e 's,.*#,,'`

AC_MSG_CHECKING([for slot])
AC_MSG_RESULT([$PINKTRACE_PC_SLOT])
AC_SUBST([PINKTRACE_PC_SLOT])

PINKTRACE_CFLAGS=
PINKTRACE_PC_CFLAGS=
PINKTRACE_PC_LIBS="-lpinktrace_${PINKTRACE_PC_SLOT}"
dnl }}}

dnl {{{ Toolchain checks
AC_PROG_CC_C99
if test x"$ac_cv_prog_cc_c99" = x"no"; then
	AC_MSG_ERROR([pinktrace requires a C compiler that supports ISO C99!])
fi
AC_GNU_SOURCE
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_LIBTOOL
AC_PROG_SED
dnl }}}

dnl {{{ Check for supported OS
AC_MSG_CHECKING([for supported OS])
case "$host_os" in
	linux*)
		AC_MSG_RESULT([$host_os])
		AC_MSG_CHECKING([for supported kernel])
		kernel_version=`uname -r | cut -d'-' -f1`
		case "$kernel_version" in
			2.6*)
				AC_MSG_RESULT([$kernel_version])
#				kernel_minor_version=`echo $kernel_version | cut -d'.' -f3`
#				if test "$kernel_minor_version" -lt 29; then
#					AC_MSG_WARN([pinktrace works slow on kernels <2.6.29 due to a ptrace bug!])
#					AC_MSG_WARN([See http://git.kernel.org/?p=linux/kernel/git/torvalds/linux-2.6.git;a=commit;h=53da1d9456fe7f87a920a78fdbdcf1225d197cb7 for the fix!])
#				fi
				;;
			2.5*)
				kernel_minor_version=`echo $kernel_version | cut -d'.' -f3`
				if test "$kernel_minor_version" -lt 46; then
					AC_MSG_RESULT([NO!])
					AC_MSG_ERROR([linux kernel version $kernel_version is not supported by pinktrace])
				fi
				AC_MSG_RESULT([$kernel_version])
				;;
			*)
				AC_MSG_RESULT([NO!])
				AC_MSG_ERROR([linux kernel version $kernel_version is not supported by pinktrace])
				;;
		esac
		;;
	*)
		AC_MSG_RESULT([NO!])
		AC_MSG_ERROR([operating system $host_os is not supported by pinktrace])
		;;
esac
dnl }}}

dnl {{{ Check for supported architecture
AC_MSG_CHECKING([for supported architecture])
case "$host_cpu" in
i[[3456]]86|pentium)
	arch=i386
	AC_DEFINE([I386], 1, [Define for the i386 architecture.])
	;;
x86?64*)
	arch=x86_64
	AC_DEFINE([X86_64], 1, [Define for the AMD x86-64 architecture.])
	;;
ia64)
	arch=ia64
	AC_DEFINE([IA64], 1, [Define for the IA64 architecture.])
	;;
powerpc64*)
	arch=powerpc64
	AC_DEFINE([POWERPC64], 1, [Define for the POWERPC64 architecture.])
	;;
powerpc*)
	arch=powerpc
	AC_DEFINE([POWERPC], 1, [Define for the POWERPC architecture.])
	;;
*)
	AC_MSG_RESULT([NO!])
	AC_MSG_ERROR([architecture $host_cpu is not supported by sydbox])
	;;
esac
AC_MSG_RESULT([$arch])
AM_CONDITIONAL(I386, test x"$arch" = x"i386")
AM_CONDITIONAL(X86_64, test x"$arch" = x"x86_64")
AM_CONDITIONAL(IA64, test x"$arch" = x"ia64")
AM_CONDITIONAL(POWERPC, test x"$arch" = x"powerpc")
AM_CONDITIONAL(POWERPC64, test x"$arch" = x"powerpc64")
AM_CONDITIONAL(PERSONALITY_ONE,
			   test x"$arch" = x"i386" -o x"$arch" = x"ia64" -o x"$arch" = x"powerpc" -o x"$arch" = x"powerpc64")
AM_CONDITIONAL(PERSONALITY_TWO, test x"$arch" = x"x86_64")

dnl Export some useful information via pkg-config --cflags
if test x"$arch" = x"i386" -o x"$arch" = x"powerpc" ; then
	PINKTRACE_PC_CFLAGS="${PINKTRACE_PC_CFLAGS} -DPINKTRACE_DEFAULT_PERSONALITY=32"
else
	PINKTRACE_PC_CFLAGS="${PINKTRACE_PC_CFLAGS} -DPINKTRACE_DEFAULT_PERSONALITY=64"
fi

if test x"$arch" = x"i386" -o x"$arch" = x"ia64" -o x"$arch" = x"powerpc" -o x"$arch" = x"powerpc64" ; then
	PINKTRACE_PC_CFLAGS="${PINKTRACE_PC_CFLAGS} -DPINKTRACE_SUPPORTED_PERSONALITIES=1"
else
	PINKTRACE_PC_CFLAGS="${PINKTRACE_PC_CFLAGS} -DPINKTRACE_SUPPORTED_PERSONALITIES=2"
fi
dnl }}}

dnl {{{ Check headers
REQUIRED_HEADERS="sys/ptrace.h sys/types.h sys/wait.h netinet/in.h sys/un.h"
for header in $REQUIRED_HEADERS ; do
	AC_CHECK_HEADER([$header], [],
					AC_MSG_ERROR([Required header $header not found!]))
done
AC_CHECK_HEADERS([sys/reg.h], [], [])
dnl }}}

dnl {{{ Check types
AC_CHECK_TYPES([struct pt_all_user_regs, struct ia64_fpreg],,,
			   [#include <sys/ptrace.h>])
dnl }}}

dnl {{{ Check for gcc
AM_CONDITIONAL(GCC, test x"$GCC" = x"yes")
dnl }}}

dnl {{{ Check for IPV6 support
AC_ARG_ENABLE([ipv6],
			  [AS_HELP_STRING([--disable-ipv6],
							  [disable IPV6 support for network sandboxing])],
			  WANT_IPV6="$enableval",
			  WANT_IPV6="yes")
if test x"$WANT_IPV6" = x"yes" ; then
	APR_CHECK_SOCKADDR_IN6
	CHECK_AF_INET6_DEFINE
	AC_MSG_CHECKING([for IPV6 support])
	if test x"$have_sockaddr_in6" = x"1" ; then
		if test x"$have_af_inet6" = x"1" ; then
			AC_MSG_RESULT([yes])
		else
			AC_MSG_RESULT([no -- no AF_INET6 define])
			WANT_IPV6=no
		fi
	else
		AC_MSG_RESULT([no -- no sockaddr_in6])
		WANT_IPV6=no
	fi
else
	AC_MSG_CHECKING([for IPV6 support])
	AC_MSG_RESULT([no])
fi
AM_CONDITIONAL(WANT_IPV6, test x"$WANT_IPV6" = x"yes")
if test x"$WANT_IPV6" = x"yes" ; then
	PINKTRACE_CFLAGS="${PINKTRACE_CFLAGS} -DPINKTRACE_HAVE_IPV6=1"
	PINKTRACE_PC_CFLAGS="${PINKTRACE_PC_CFLAGS} -DPINKTRACE_HAVE_IPV6=1"
else
	PINKTRACE_CFLAGS="${PINKTRACE_CFLAGS} -DPINKTRACE_HAVE_IPV6=0"
	PINKTRACE_PC_CFLAGS="${PINKTRACE_PC_CFLAGS} -DPINKTRACE_HAVE_IPV6=0"
fi
dnl }}}

dnl {{{ Extra CFLAGS
WANTED_CFLAGS="-pedantic -Wall -W -Wextra -Wbad-function-cast -Wcast-align -Wcast-qual -Wfloat-equal -Wformat=2 -Wformat-security -Wformat-nonliteral -Winit-self -Winline -Wlogical-op -Wmissing-prototypes -Wmissing-declarations -Wmissing-format-attribute -Wmissing-noreturn -Wpointer-arith -Wredundant-decls -Wshadow -Wswitch-default -Wunused -Wvla"
for flag in $WANTED_CFLAGS ; do
	AX_CHECK_COMPILER_FLAGS([$flag], [PINKTRACE_CFLAGS="$PINKTRACE_CFLAGS $flag"],)
done
AC_SUBST([PINKTRACE_CFLAGS])
dnl }}}

dnl {{{ pkg-config
AC_SUBST([PINKTRACE_PC_CFLAGS])
AC_SUBST([PINKTRACE_PC_LIBS])
AC_SUBST([PINKTRACE_PC_VERSION])
AC_SUBST([PINKTRACE_PC_SLOT])
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

dnl {{{ Use AM_SILENT_RULES if available
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])
dnl }}}

dnl {{{ Built sources
GENERATED_FILE=misc/generated-file.txt
AC_SUBST_FILE([GENERATED_FILE])
dnl }}}

dnl {{{ Output
AM_CONFIG_HEADER(config.h)
AC_OUTPUT(
		  pinktrace.pc
		  Makefile
		  include/pinktrace/about.h
		  src/Makefile
		  tests/Makefile
		  doc/Makefile
		  doc/doxygen.conf
		  examples/Makefile
	)
dnl }}}
