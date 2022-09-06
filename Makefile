RSCRIPT := Rscript --no-save --no-restore

# Pre-computed vignettes
RMDS		:= $(patsubst %Rmd.orig, %Rmd, $(wildcard vignettes/*.orig))
# Standard vignettes
RMDS		+= $(wildcard vignettes/*.Rmd)
# Rendered vignettes
HTMLS		:= $(patsubst vignettes%.Rmd, doc%.html, $(RMDS))

# Rendered markdown for conversion to IPython notebooks
MDS			:= $(RMDS:%.Rmd=%.md)
IPYNBS	:= $(RMDS:%.Rmd=%.ipynb)

ipynbs: $(IPYNBS)
vignettes: $(RMDS) $(HTMLS)
all: $(ipynbs) $(vignettes)

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

# precomputed vignettes
vignettes/%.Rmd: vignettes/%.Rmd.orig
	@echo "Knitting pre-computed vignette $< to $@"
	@$(RSCRIPT) -e "knitr::knit('$<', '$@', quiet = TRUE)"

clean:
	@rm -f vignettes/*.{md,ipynb,html,R}
	@rm -rf doc
