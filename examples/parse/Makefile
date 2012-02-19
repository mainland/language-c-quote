GHC=ghc

HAPPY=happy 
HAPPY_ARGS=-agci

ALEX=alex 
ALEX_ARGS=-gi

APACHE_VERSION=2.2.22

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
	$(GHC) $(GHC_ARGS) --make Main.hs -odir obj -hidir obj \
		-i../.. -i../../dist/build \
		-o $@

.PHONY : apache-code
apache-code : httpd-$(APACHE_VERSION).tar.gz
	tar zxf httpd-$(APACHE_VERSION).tar.gz
	(cd httpd-$(APACHE_VERSION); \
	 CFLAGS="-save-temps" ./configure; \
	 $(MAKE))

httpd-$(APACHE_VERSION).tar.gz:
	wget http://archive.apache.org/dist/httpd/httpd-$(APACHE_VERSION).tar.gz
