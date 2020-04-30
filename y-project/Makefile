
%.lex.sml : %.lex
	mllex $<
%.grm.sml : %.grm
	mlyacc $<
all: tigc

PHONY: all clean

tigc: tiger.mlb  tiger.lex.sml tiger.grm.sig tiger.grm.sml tiger.sml ast.sml beautify.sml
	mlton -output tigc tiger.mlb

clean: 
	rm tigc
	   


