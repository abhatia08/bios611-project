.PHONY: clean
.PHONY: shiny
SHELL: /bin/bash

# Clean action to remove previously existing processed products
clean:
	rm -f derived_data/*
	rm -f figures/*
	rm -f report.pdf

# Final report for the Project
report.pdf: report.tex
	pdflatex report.tex

# Produce clean datasets used in this project
derived_data/yelp_tidy.csv: source_data/*\
			scripts/util.R
			Rscript scripts/cleanup.R

derived_data/ahrf_subset.csv: source_data/*\
			scripts/util.R
			Rscript scripts/cleanup.R

derived_data/percent_nonwhite_pop.csv: source_data/*\
			scripts/util.R
			Rscript scripts/cleanup.R

derived_data/population_by_age.csv: source_data/*\
			scripts/util.R
			Rscript scripts/cleanup.R

# Shiny setup
shiny: scripts/shiny.R
	Rscript scripts/shiny.R ${PORT}