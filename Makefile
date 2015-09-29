GHC=ghc
GHCFLAGS += -Wall

RUNGHC=runghc
RUNGHCFLAGS=-Wall -fno-warn-unused-imports

HAPPY=happy 
HAPPYFLAGS=-agci

ALEX=alex 
ALEXFLAGS=-gi

LIBSRC = $(shell find Language -type f) \
	dist/build/Language/C/Parser/Lexer.hs \
	dist/build/Language/C/Parser/Parser.hs

#
# To support cabal sandbox
#
ifneq ($(wildcard .cabal-sandbox/*-packages.conf.d),)
GHCFLAGS += \
	-no-user-package-db \
	-package-db $(wildcard .cabal-sandbox/*-packages.conf.d)

RUNGHCFLAGS += \
	-no-user-package-db \
	-package-db --ghc-arg=$(wildcard .cabal-sandbox/*-packages.conf.d)
endif

#
# Support Cabal's MIN_VERSION
#
GHCFLAGS += -optP-include -optPdist/build/autogen/cabal_macros.h

.PHONY : all
all : Language/C/Syntax-instances.hs

.PHONY : clean
clean :
	rm -rf parse obj \
	dist/build/Language/C/Parser/Lexer.hs \
	dist/build/Language/C/Parser/Parser.hs

dist/build/autogen/cabal_macros.h :
	cabal build

dist/build/Language/C/Parser/Parser.hs : Language/C/Parser/Parser.y dist/build/autogen/cabal_macros.h
	$(HAPPY) $(HAPPYFLAGS) -o $@ $<

dist/build/Language/C/Parser/Lexer.hs : Language/C/Parser/Lexer.x dist/build/autogen/cabal_macros.h
	$(ALEX) $(ALEXFLAGS) -o $@ $<

Language/C/Syntax-instances.hs : bin/gen-instances.hs bin/Derive.hs
	$(RUNGHC) $(RUNGHCFLAGS) -ibin -DONLY_TYPEDEFS $< > $@ || rm -f $@

test : Test.hs $(LIBSRC)
	@mkdir -p obj
	$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj \
		-i. -idist/build \
		-o $@

neg : Neg.hs $(LIBSRC)
	@mkdir -p obj
	$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj \
		-i. -idist/build \
		-o $@
