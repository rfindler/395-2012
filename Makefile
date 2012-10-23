# Path to bin directory for K (assumed local link)
KBIN=k/bin
ENV=env PATH=$(KBIN):/usr/local/bin:$${PATH}
KOMPILE=$(ENV) kompile
KRUN=$(ENV) krun
RACKET=$(ENV) racket

.DELETE_ON_ERROR: tests.out

KS=imp.k
MAUDES=$(KS:.k=-compiled.maude)

all : tests.out

kompile : $(MAUDES)

%-compiled.maude : %.k
	$(KOMPILE) $<

tests.out : $(MAUDES)
	date >  $@
	$(RACKET) run-tests.rkt >> $@
	cat $@

backup.cron.jobs :
	crontab -l > $@

kron.jobs : backup.cron.jobs
	cat $< > $@
	echo '*/1 * * * * make -C $(CURDIR)' >> $@

kron-on : kron.jobs
	crontab $<

kron-off :
	crontab -r
	@if [ -e backup.cron.jobs ] ; then \
		echo crontab backup.cron.jobs ; \
		crontab backup.cron.jobs ; \
	fi
	rm -f backup.cron.jobs kron.jobs

clean :
	rm -rf *.maude .k *.out
