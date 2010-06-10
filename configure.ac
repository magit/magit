AC_INIT(magit, 0.8.1)
AC_CONFIG_SRCDIR([magit.el])
AM_INIT_AUTOMAKE([1.10])

AC_ARG_WITH([site-start],
	AS_HELP_STRING([--with-site-start], [Provide the emacs site-start directory.  The default is $(sysconfdir)/emacs/site-start.el.]),
	[ SITESTART="$withval" ],
	[ SITESTART='$(sysconfdir)/emacs/site-start.d' ])

AC_SUBST([SITESTART])
AC_CONFIG_FILES([Makefile 
	         magit-pkg.el
		 magit.spec])

AC_OUTPUT
