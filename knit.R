#! /usr/bin/env Rscript
#
# Helper script to knit R-Markdown from command line.
# Usage: Rscript knit.R

library(rmarkdown)
library(prettydoc)
library(knitr)

rmarkdown::render("analysis.Rmd")
