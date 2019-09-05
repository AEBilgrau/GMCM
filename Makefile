################################################################################
# Makefile for building GMCM package
#
# Use for preparing package
################################################################################

# Inspried by
#   https://raw.githubusercontent.com/yihui/knitr/master/Makefile

# Be sure to set
# Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.26\\bin\\gswin64c.exe")

# Setup package name, version, folder src
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: prereq build install check site

site:
	Rscript -e "pkgdown::build_site()"

docs:
	Rscript -e "devtools::document(roclets=c('rd', 'namespace'))"

build: prereq
	cd ..;\
	R CMD build --resave-data --compact-vignettes=both $(PKGSRC)

install: prereq build
	cd ..;\
	R CMD INSTALL --preclean --resave-data $(PKGNAME)_$(PKGVERS).tar.gz

check:
	cd ..;\
	R CMD check --as-cran $(PKGNAME)_$(PKGVERS).tar.gz

prereq:
	Rscript -e 'stopifnot(Sys.getenv("R_GSCMD") != "")' \
	Rscript -e 'stopifnot(rmarkdown::pandoc_available())'

shinydeploy:
	Rscript -e 'setwd("inst/shiny"); rsconnect::deployApp(appName = "GMCM")'

clean:
	cd ..;\
	rm -r $(PKGNAME).Rcheck
