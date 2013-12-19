BUILDDIR    ?= build
CFG         ?= default
LLVM_CONFIG ?= llvm-config
NAME        ?= c4
SRCDIR      ?= src

all:

-include $(CFG).cfg

Q ?= @

BINDIR := $(BUILDDIR)/$(CFG)
BIN    := $(BINDIR)/$(NAME)
SRC    := $(sort $(shell find $(SRCDIR) -name '*.cc'))
OBJ    := $(SRC:$(SRCDIR)/%.cc=$(BINDIR)/%.o)
DEP    := $(OBJ:%.o=%.d)

LLVM_CFLAGS  := $(shell $(LLVM_CONFIG) --cppflags)
LLVM_LDFLAGS := $(shell $(LLVM_CONFIG) --ldflags --libs core)

CFLAGS   += $(LLVM_CFLAGS) -Wall -W -Werror
CXXFLAGS += $(CFLAGS) -std=c++11
LDFLAGS  += $(LLVM_LDFLAGS)

DUMMY := $(shell mkdir -p $(sort $(dir $(OBJ))))

.PHONY: all clean

all: $(BIN)

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
