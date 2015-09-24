GHC=ghc

HAPPY=happy 
HAPPY_ARGS=-agci

ALEX=alex 
ALEX_ARGS=-gi

APACHE_VERSION=2.4.16
APR_VERSION=1.5.2
APRUTIL_VERSION=1.5.4

GHCFLAGS = -Wall -O

# So we can use MIN_VERSION macros
GHCFLAGS += -optP-include -optP../../dist/build/autogen/cabal_macros.h

# So we can use MIN_VERSION macros
GHCFLAGS += -optP-include -optP../../dist/build/autogen/cabal_macros.h

#
# To support cabal sandbox
#
ifneq ($(wildcard ../../.cabal-sandbox/*-packages.conf.d),)
GHCFLAGS += \
	-no-user-package-db \
	-package-db $(wildcard ../../.cabal-sandbox/*-packages.conf.d)
endif

SOURCE = Main.hs Opts.hs \
	$(shell find ../../Language -name .svn -prune -o -type f | grep -v .svn) \
	../../dist/build/Language/C/Parser/Lexer.hs \
	../../dist/build/Language/C/Parser/Parser.hs

.PHONY : all
all : parse

.PHONY : clean
clean :
	rm -rf parse obj \
	../../dist/build/Language/C/Parser/Lexer.hs \
	../../dist/build/Language/C/Parser/Parser.hs

../../dist/build/Language/C/Parser/Parser.hs : ../../Language/C/Parser/Parser.y
	$(HAPPY) $(HAPPY_ARGS) -o $@ $<

../../dist/build/Language/C/Parser/Lexer.hs : ../../Language/C/Parser/Lexer.x
	$(ALEX) $(ALEX_ARGS) -o $@ $<

parse : $(SOURCE)
	@mkdir -p obj
	$(GHC) $(GHCFLAGS) --make Main.hs -odir obj -hidir obj \
		-i../.. -i../../dist/build \
		-o $@

.PHONY : apache-code
apache-code : httpd-$(APACHE_VERSION).tar.bz2 apr-$(APR_VERSION).tar.bz2 apr-util-$(APRUTIL_VERSION).tar.bz2
	tar -xf httpd-$(APACHE_VERSION).tar.bz2
	mkdir -p httpd-$(APACHE_VERSION)/srclib/apr
	tar -C httpd-$(APACHE_VERSION)/srclib/apr --strip-components=1 -xf apr-$(APR_VERSION).tar.bz2
	mkdir -p httpd-$(APACHE_VERSION)/srclib/apr-util
	tar -C httpd-$(APACHE_VERSION)/srclib/apr-util --strip-components=1 -xf apr-util-$(APRUTIL_VERSION).tar.bz2
	(cd httpd-$(APACHE_VERSION); \
	 CFLAGS="-save-temps" ./configure --with-included-apr; \
	 $(MAKE))

httpd-$(APACHE_VERSION).tar.bz2 :
	wget http://mirror.nexcess.net/apache/httpd/httpd-$(APACHE_VERSION).tar.bz2

apr-$(APR_VERSION).tar.bz2 :
	wget http://mirror.nexcess.net/apache/apr/apr-$(APR_VERSION).tar.bz2

apr-util-$(APRUTIL_VERSION).tar.bz2 :
	wget http://mirror.nexcess.net/apache/apr/apr-util-$(APRUTIL_VERSION).tar.bz2
