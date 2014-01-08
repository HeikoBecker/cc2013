BUILDDIR    ?= build
CFG         ?= default
LLVM_CONFIG ?= llvm-config
NAME        ?= c4
SRCDIR      ?= src

all:

-include $(CFG).cfg

Q ?= @


UNAME := $(shell uname)

# for mac another compiler
ifeq ($(UNAME), Darwin)
  CXX := /opt/local/bin/g++-mp-4.8
endif

BINDIR := $(BUILDDIR)/$(CFG)
BIN    := $(BINDIR)/$(NAME)
SUBDIRS := lexer
SRC    := $(sort $(wildcard $(SRCDIR)/utils/*.cc) $(wildcard $(SRCDIR)/*.cc) $(wildcard $(SRCDIR)/lexer/*.cc) $(wildcard $(SRCDIR)/parser/*.cc))
OBJ    := $(SRC:$(SRCDIR)/%.cc=$(BINDIR)/%.o)
DEP    := $(OBJ:%.o=%.d)

LLVM_CFLAGS  := $(shell $(LLVM_CONFIG) --cppflags)
LLVM_LDFLAGS := $(shell $(LLVM_CONFIG) --ldflags --libs core)

CFLAGS   += $(LLVM_CFLAGS) -Wall -W -Werror -O2

CXXFLAGS += $(CFLAGS) -std=c++11
LDFLAGS  += $(LLVM_LDFLAGS)

DUMMY := $(shell mkdir -p $(sort $(dir $(OBJ))))

.PHONY: all clean

all: $(BIN)

debug: CFLAGS += -DDEBUG -g -Wextra -pedantic-errors -O0
debug: CXXFLAGS += -DDEBUG -g -Wextra -pedantic-errors -O0
debug: $(BIN)

-include $(CFG).cfg
-include $(DEP)

clean:
	@echo "===> CLEAN"
	$(Q)rm -fr $(BINDIR)

$(BIN): $(OBJ)
	@echo "===> LD $@"
	$(Q)$(CXX) -o $(BIN) $(OBJ) $(LDFLAGS)

$(BINDIR)/%.o: $(SRCDIR)/%.cc
	@echo "===> CXX $<"
	$(Q)$(CXX) $(CXXFLAGS) -MMD -c -o $@ $<
