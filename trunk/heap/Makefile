PROG 	= ../ypsilon

STDLIB  = ../stdlib

all :
	@$(PROG) --sitelib=$(STDLIB) --clean-acc ./build-composit.scm
	@$(PROG) --sitelib=$(STDLIB) --clean-acc ./build-debug-core.scm
	@$(PROG) --sitelib=$(STDLIB) --clean-acc ./build-debug-boot.scm
	@$(PROG) --sitelib=$(STDLIB) --clean-acc ./build-boot-fasl.scm
	@$(PROG) --sitelib=$(STDLIB) --clean-acc ./build-boot-code.scm
	@$(PROG) --sitelib=$(STDLIB) --clean-acc ./build-core-fasl.scm
	@$(PROG) --sitelib=$(STDLIB) --clean-acc ./build-core-code.scm
