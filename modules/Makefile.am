guilemoduledir = $(GUILE_SITE)

nobase_dist_guilemodule_DATA =				\
  rpc/xdr.scm rpc/xdr/types.scm				\
  rpc/rpc.scm rpc/rpc/types.scm rpc/rpc/transports.scm	\
  rpc/rpc/server.scm rpc/rpc/portmap.scm

EXTRA_DIST = srfi/srfi-64.scm srfi/srfi-64.upstream.scm

if HAVE_GUILE_LINT
all-local: $(nobase_dist_guilemodule_DATA)
	$(GUILE_LINT) -L $(srcdir) $^
endif
