.PHONY: all clean

ERLBIN      := /opt/erlang/bin/erl

UNAME       := $(shell uname)

BEAMS       := $(patsubst src/%, ebin/%, $(patsubst %.erl, %.beam, $(wildcard src/*.erl)))
ECINCLUDES  := -I include
ECFLAGS     := +debug_info

ERLDIR      := $(shell $(ERLBIN) -noinput -eval 'io:format("~s",[code:root_dir()]),halt().')
ERTSVERS    := $(shell $(ERLBIN) -noinput -eval 'io:format("~s",[erlang:system_info(version)]),halt().')
ERLINTRFCE  := $(shell $(ERLBIN) -noinput -eval 'io:format("~s",[filename:basename(code:lib_dir(erl_interface))]),halt().')
DRIVER      := priv/emq_posix.so

ifeq ($(UNAME),Linux)
CFLAGS      := -fpic -shared
endif
ifeq ($(UNAME),Darwin)
CFLAGS      := -bundle -flat_namespace -undefined suppress
endif
ifeq ($(UNAME),SunOS)
CFLAGS      := -fpic -shared 
endif
CFLAGS      := $(CFLAGS) -Wall -g -Iinclude -I$(ERLDIR)/erts-$(ERTSVERS)/include \
               -I$(ERLDIR)/lib/$(ERLINTRFCE)/include
LDFLAGS     := -L$(ERLDIR)/lib/$(ERLINTRFCE)/lib

all: $(BEAMS) $(DRIVER)

ebin/%.beam: src/%.erl
	@echo "[ERLC]" $<": "$@
	@erlc -o ebin/ $(ECINCLUDES) $(ECFLAGS) $<

priv/%.so: c_src/%.c
	@echo "[GCC]" $<": "$@
	@gcc -o $@ $(CFLAGS) $(LDFLAGS) $<  -lerl_interface -lei -lrt

clean:
	rm -f ebin/*.beam
	rm -f priv/*.so
	rm -f $(EDEF)
