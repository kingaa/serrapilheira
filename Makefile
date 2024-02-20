MODULES = prep misc intro stochsim pfilter mif measles ebola parest

default: index.html acknowledge.html modules README.md 

modules:
	for module in $(MODULES); do ($(MAKE) -C $$module | tee $$module.out); done

include rules.mk

welcome.html: welcome.md
	$(REXE) -e "rmarkdown::render(\"$^\",output_format=\"revealjs::revealjs_presentation\")"

clean:
	for module in $(MODULES); do $(MAKE) -C $$module clean; done

fresh:
	for module in $(MODULES); do $(MAKE) -C $$module fresh; done
