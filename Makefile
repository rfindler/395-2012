# Path to bin directory for K (assumed local link)
KBIN=k/bin
ENV=env PATH=$(KBIN):/usr/local/bin:$${PATH}
KOMPILE=$(ENV) kompile
KRUN=$(ENV) krun
RACKET=$(ENV) racket

.DELETE_ON_ERROR: tests.out

KS=$(wildcard *.k)
MAUDES=$(KS:.k=-compiled.maude)

all : kompile

kontinuous : 
	@ while true ; do make kompile > kompiling.out 2>&1 ; mv kompiling.out kompile.out ; sleep 1 ; done

watch :
	# Needs watch installed
	watch -n 1 "cat kompile.out"

test262 :
	# Needs hg ; pegged to revision 334
	hg clone -r 334 http://hg.ecmascript.org/tests/test262 $@

kompile : $(MAUDES)

%-compiled.maude : %.k
	$(KOMPILE) $<

tests.out : $(MAUDES)
	date >  $@
	$(RACKET) run-tests.rkt >> $@
	cat $@

clean :
	rm -rf *.maude .k *.out 
