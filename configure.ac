### configure.ac					-*- Autoconf -*-

AC_PREREQ(2.62)

AC_INIT(MrSGUIDE, m4_esyscmd_s([awk '/^Version:/ {print $2}' DESCRIPTION]))

## Set R_HOME, respecting an environment variable if one is set
: ${R_HOME=$(R RHOME)}
if test -z "${R_HOME}"; then
    AC_MSG_ERROR([Could not determine R_HOME.])
fi
## Use R to set CXX and CXXFLAGS
CXX=$(${R_HOME}/bin/R CMD config CXX)
CXXFLAGS=$("${R_HOME}/bin/R" CMD config CXXFLAGS)

## We are using C++
AC_LANG(C++)
AC_REQUIRE_CPP

## Check the C++ compiler using the CXX value set
AC_PROG_CXX
## If it is g++, we have GXX set so let's examine it
if test "${GXX}" = yes; then
    AC_MSG_CHECKING([whether g++ version is sufficient])
    gxx_version=$(${CXX} -v 2>&1 | awk '/^.*g.. version/ {print $3}')
    case ${gxx_version} in
        1.*|2.*|3.*|4.0.*|4.1.*|4.2.*|4.3.*|4.4.*|4.5.*|4.6.*|4.7.0|4.7.1)
             AC_MSG_RESULT([no])
             AC_MSG_WARN([Only g++ version 4.7.2 or greater can be used with RcppArmadillo.])
             AC_MSG_ERROR([Please use a different compiler.])
        ;;
        4.7.[2-9]|4.8.*|4.9.*|5.*|6.*|7.*|8.*)
             gxx_newer_than_45="-fpermissive"
             AC_MSG_RESULT([(${gxx_version}) yes])
        ;;
    esac
fi

dnl this the meat of R's m4/openmp.m4
  OPENMP_[]_AC_LANG_PREFIX[]FLAGS=
  AC_ARG_ENABLE([openmp],
    [AS_HELP_STRING([--disable-openmp], [do not use OpenMP])])
  if test "$enable_openmp" != no; then
    AC_CACHE_CHECK([for $[]_AC_CC[] option to support OpenMP],
      [ac_cv_prog_[]_AC_LANG_ABBREV[]_openmp],
      [AC_LINK_IFELSE([_AC_LANG_OPENMP],
   	 [ac_cv_prog_[]_AC_LANG_ABBREV[]_openmp='none needed'],
	 [ac_cv_prog_[]_AC_LANG_ABBREV[]_openmp='unsupported'
	  for ac_option in -fopenmp -xopenmp -qopenmp \
                           -openmp -mp -omp -qsmp=omp -homp \
			   -fopenmp=libomp \
                           -Popenmp --openmp; do
	    ac_save_[]_AC_LANG_PREFIX[]FLAGS=$[]_AC_LANG_PREFIX[]FLAGS
	    _AC_LANG_PREFIX[]FLAGS="$[]_AC_LANG_PREFIX[]FLAGS $ac_option"
	    AC_LINK_IFELSE([_AC_LANG_OPENMP],
	      [ac_cv_prog_[]_AC_LANG_ABBREV[]_openmp=$ac_option])
	    _AC_LANG_PREFIX[]FLAGS=$ac_save_[]_AC_LANG_PREFIX[]FLAGS
	    if test "$ac_cv_prog_[]_AC_LANG_ABBREV[]_openmp" != unsupported; then
	      break
	    fi
	  done])])
    case $ac_cv_prog_[]_AC_LANG_ABBREV[]_openmp in #(
      "none needed" | unsupported)
	;; #(
      *)
	OPENMP_[]_AC_LANG_PREFIX[]FLAGS=$ac_cv_prog_[]_AC_LANG_ABBREV[]_openmp ;;
    esac
  fi

AC_SUBST(OPENMP_CXXFLAGS)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
