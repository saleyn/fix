# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR    := $(shell pwd)
BASEDIR   := $(if $(patsubst %c_src,,$(lastword $(CURDIR))),$(abspath $(CURDIR)/..),..)
OUTDIR    := $(REBAR_BARE_COMPILER_OUTPUT_DIR)
REBAR_ENV ?= $(shell echo "${REBAR_BUILD_DIR}" | sed 's,^.*_build/,,')

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -noinput -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]), halt(0).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -noinput -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)]), halt(0).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)]), halt(0).")

SOURCES := $(wildcard fix_nif.cpp) $(wildcard fix_variant*.cpp)
OBJECTS := $(addsuffix .o,  $(basename $(SOURCES)))
TARGETS := $(addprefix $(BASEDIR)/priv/, $(addsuffix .so, $(basename $(SOURCES))))
HEADERS := util.hpp

INC_DIR :=

ifneq ($(OUTDIR),$(BASEDIR))
ifneq ($(OUTDIR),)
	TARGETS += $(addprefix $(OUTDIR)/priv/, $(addsuffix .so, $(basename $(SOURCES))))
endif
endif

all: $(TARGETS)

# System type and C compiler/flags.

ifneq ($(or $(findstring $(NIF_DEBUG),1 true),$(findstring test,$(REBAR_ENV))),)
  ADDFLAGS := -g -DDEBUG -O0
else
  ADDFLAGS := -g -O3 -DNDEBUG
endif

UNAME_SYS  := $(shell uname -s)

ifeq ($(UNAME_SYS),Darwin)
  CXX      ?= cc++
  LDFLAGS  ?= -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS),FreeBSD)
  CXX 		 ?= cc++
else # ifeq ($(UNAME_SYS),Linux)
  CXX 		 ?= g++
endif

CXXFLAGS += $(ADDFLAGS) $(INC_DIR) -std=c++2a -finline-functions -Wall

info:
	@echo "Targets:  $(TARGETS)"
	@echo "CXX:      $(CXX)"
	@echo "CXXFLAGS: $(CXXFLAGS)"


CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)

LDLIBS  += -L $(ERL_INTERFACE_LIB_DIR) -lei -ldl
LDFLAGS += -shared

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

$(BASEDIR)/priv/%.so: %.o | $(BASEDIR)/priv
	$(link_verbose) $(CXX) $< $(LDFLAGS) $(LDLIBS) -o $@

$(BASEDIR)/priv:
	@mkdir -p $@

ifneq ($(BASEDIR),$(OUTDIR))
ifneq (,$(OUTDIR))
$(OUTDIR)/priv/%.so: $(BASEDIR)/priv/%.so | $(OUTDIR)/priv
	@if [ "$(shell readlink -f $<)" == "$(shell readlink -f $@)" ]; then \
		echo "Skip copying file to $@: source and destination are the same"; \
	else \
	  cp -vf "$<" "$@"; \
	fi

$(OUTDIR)/priv:
	@mkdir -p $@
endif
endif

%.o: %.cpp $(HEADERS)
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

.PRECIOUS: $(OBJECTS)

clean:
	@rm -f $(TARGETS) $(OBJECTS)
