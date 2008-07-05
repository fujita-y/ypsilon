PROG 	= ../ypsilon

STDLIB  = ../stdlib

all :
	@$(PROG) --sitelib=$(STDLIB) ./build-composit.scm
	@$(PROG) --sitelib=$(STDLIB) ./build-debug-core.scm
	@$(PROG) --sitelib=$(STDLIB) ./build-debug-boot.scm
	@$(PROG) --sitelib=$(STDLIB) ./build-boot-fasl.scm
	@$(PROG) --sitelib=$(STDLIB) ./build-boot-code.scm
	@$(PROG) --sitelib=$(STDLIB) ./build-core-fasl.scm
	@$(PROG) --sitelib=$(STDLIB) ./build-core-code.scm
