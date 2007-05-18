EXTRA_DIST = grpc-rpcinfo.in

bin_SCRIPTS = grpc-rpcinfo

if HAVE_GUILE_LINT
all-local: $(bin_SCRIPTS)
	$(GUILE_LINT) -L $(top_srcdir)/modules $^
endif
