#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT([guile-rpc], [0.1], [ludovic.courtes@laas.fr])
AC_CONFIG_AUX_DIR(build-aux)
AC_CONFIG_MACRO_DIR(m4)

AM_INIT_AUTOMAKE(guile-rpc, 0.1)

AC_CONFIG_SRCDIR([modules/rpc/xdr.scm])


# Checks for Guile.
GUILE_PROGS
GUILE_SITE_DIR

# Guile-Lint (http://www.geocities.com/user42_kevin/guile-lint/index.html)
AC_PATH_PROG([GUILE_LINT], [guile-lint])
AC_SUBST([GUILE_LINT])
AM_CONDITIONAL([HAVE_GUILE_LINT], test "x$GUILE_LINT" != "x")

# Need the R6RS libs.
GUILE_MODULE_REQUIRED([r6rs bytevector])
GUILE_MODULE_REQUIRED([r6rs i/o ports])

# SRFI-34 is available in Guile 1.8, just checking to make sure.
GUILE_MODULE_REQUIRED([srfi srfi-34])

# Need SRFI-35 (available in `guile-library').
GUILE_MODULE_REQUIRED([srfi srfi-35])


# Output.
AC_CONFIG_FILES([Makefile
	         doc/Makefile
	         modules/Makefile
		 utils/Makefile
		 tests/Makefile])

AC_CONFIG_FILES([utils/grpc-rpcinfo], [chmod +x utils/grpc-rpcinfo])

AC_OUTPUT
