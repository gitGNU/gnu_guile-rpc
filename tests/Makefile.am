TESTS_ENVIRONMENT = $(GUILE) -L $(top_srcdir)/modules

TESTS = xdr.test xdr-encoding.test rpc.test server-loop.test
EXTRA_DIST = $(TESTS)

if HAVE_GUILE_LINT
all-local: $(TESTS)
	$(GUILE_LINT) -L $(top_srcdir)/modules $^
endif
