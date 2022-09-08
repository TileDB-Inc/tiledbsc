RSCRIPT := Rscript --no-save --no-restore

PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`

# Pre-computed vignettes
RMDS		:= $(patsubst %Rmd.orig, %Rmd, $(wildcard vignettes/*.orig))
# Standard vignettes
RMDS		+= $(wildcard vignettes/*.Rmd)
# Rendered vignettes
HTMLS		:= $(patsubst vignettes%.Rmd, doc%.html, $(RMDS))

# Rendered markdown for conversion to IPython notebooks
MDS			:= $(RMDS:%.Rmd=%.md)
IPYNBS	:= $(RMDS:%.Rmd=%.ipynb)

all: vignettes docs check

build:
	@echo "Building package"
	@R CMD build .

check: build
	@echo "Checking package"
	@R CMD check --as-cran --no-tests $(PKGNAME)_$(PKGVERS).tar.gz

docs:
	@echo "Building documentation"
	@Rscript -e "devtools::document()"

test:
	Rscript -e "devtools::test()"

ipynbs: $(IPYNBS)
vignettes: $(RMDS) $(HTMLS)

%.ipynb: %.md
	@echo "Converting $< to $@"
	@pandoc --from markdown --to ipynb --output $@ $<

%.md: %.Rmd
	@echo "Converting $< to $@"
	@$(RSCRIPT) -e "rmarkdown::render(input = '$<', output_file = '$(@F)', output_dir = '$(@D)', output_format = rmarkdown::md_document(variant = 'commonmark', preserve_yaml = TRUE))"
	@sed -i '' 's/``` r/``` code/g' $@

doc/%.html: vignettes/%.Rmd
	@echo "Building vignette $< to $@"
	@$(RSCRIPT) -e "devtools::build_vignettes()"

# vignettes that require external datasets or additional package to run are
# pre-computed, following the approach outlined in
# https://ropensci.org/blog/2019/12/08/precompute-vignettes/
vignettes/%.Rmd: vignettes/%.Rmd.orig
	@echo "Knitting pre-computed vignette $< to $@"
	# switch to vignettes directory so images are saved there
	@cd vignettes; $(RSCRIPT) -e "knitr::knit('$(<F)', '$(@F)', quiet = TRUE)"

clean:
	@rm -f vignettes/*.{md,ipynb,html,R}
	@rm -rf doc
	@rm -rf $(PKGNAME)_$(PKGVERS).tar.gz $(PKGNAME).Rcheck
