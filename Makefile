BUILDDIR ?= build
CFG      ?= default
NAME     ?= c4
SRCDIR   ?= src

Q ?= @


UNAME := $(shell uname)

# for mac another compiler
ifeq ($(UNAME), Darwin)
  CXX := /opt/local/bin/g++-mp-4.8
endif

BINDIR := $(BUILDDIR)/$(CFG)
BIN    := $(BINDIR)/$(NAME)
SUBDIRS := lexer
SRC    := $(sort $(wildcard $(SRCDIR)/*.cc) $(wildcard $(SRCDIR)/lexer/*.cc))
OBJ    := $(SRC:$(SRCDIR)/%.cc=$(BINDIR)/%.o)
DEP    := $(OBJ:%.o=%.d)

CFLAGS   += -Wall -W -Werror
CXXFLAGS += $(CFLAGS) -std=c++11

DUMMY := $(shell mkdir -p $(sort $(dir $(OBJ))))

.PHONY: all clean

all: $(BIN)

debug: CFLAGS += -DDEBUG -g -Wextra
debug: CXXFLAGS += -DDEBUG -g -Wextra
debug: $(BIN)

-include $(CFG).cfg

-include $(DEP)

clean:
	@echo "===> CLEAN"
	$(Q)rm -fr $(BINDIR)

$(BIN): $(OBJ)
	@echo "===> LD $@"
	$(Q)$(CXX) -o $(BIN) $(OBJ)

$(BINDIR)/%.o: $(SRCDIR)/%.cc
	@echo "===> CXX $<"
	$(Q)$(CXX) $(CXXFLAGS) -MMD -c -o $@ $<
