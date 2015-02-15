all: test

.PHONY: all test compile-test

compile-test:
	@REBAR_EXTRA_DEPS=1 rebar get-deps compile

test:
	@REBAR_EXTRA_DEPS=1 rebar eunit
