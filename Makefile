SRCDIR := pkg

PACKAGE := $(shell Rscript --vanilla -e 'cat(read.dcf(file="${SRCDIR}/DESCRIPTION",fields="Package"))')
VERSION := $(shell Rscript --vanilla -e 'cat(read.dcf(file="${SRCDIR}/DESCRIPTION",fields="Version"))')

ARCHIVE := $(PACKAGE)_$(VERSION).tar.gz

RCHKIMG := kalibera-rchk-master-def.simg

export R_LIBS_USER = $(PWD)/lib/R/library

.PHONY: default

default: install-quick sync-Rlib

.PHONY: all
all: roxygenize build install sync-Rlib installspec

.PHONY: describe
describe:
	cat ${SRCDIR}/DESCRIPTION

.PHONY: show
show:
	echo "Package ${PACKAGE} version ${VERSION}"

build-wo-vignettes:
	Rscript --vanilla -e 'roxygen2::roxygenize(package.dir="${SRCDIR}")'
	echo "Building ${ARCHIVE}"
	R CMD build --no-build-vignettes $(SRCDIR)

.PHONY: build
build:
	Rscript --vanilla -e 'roxygen2::roxygenize(package.dir="${SRCDIR}")'
	echo "Building ${ARCHIVE}"
	R CMD build $(SRCDIR)

.PHONY: check-dir
check-dir: 
	echo "Checking package ${PACKAGE} version ${VERSION} in ${SRCDIR}"
	R CMD check $(SRCDIR)

.PHONY: check
check: 
	R CMD check $(ARCHIVE)
#env _R_CHECK_FORCE_SUGGESTS_=false R CMD check $(ARCHIVE)

.PHONY: build-and-check
build-and-check: build 
	R CMD check $(ARCHIVE)

install-dir:
	echo "Installing package ${PACKAGE} version ${VERSION} from ${SRCDIR}"
	R CMD INSTALL --with-keep.source $(SRCDIR)

install-quick:
	echo "Installing package ${PACKAGE} version ${VERSION} from ${SRCDIR}"
	R CMD INSTALL --no-byte-compile --with-keep.source --data-compress=none --no-test-load $(SRCDIR)

.PHONY: install-dir-clean
install-dir-clean:
	echo "Installing package ${PACKAGE} version ${VERSION} from ${SRCDIR}"
	R CMD INSTALL --pre-clean $(SRCDIR)

.PHONY: install
install:
	R CMD INSTALL $(ARCHIVE)

.PHONY: installspec
installspec:
	. bin/activate; \
	python rkernel/install.py

sync-Rlib:
	rsync -ai lib/R/library/ $(HOME)/.jupyter/R/library/
	rsync -ai lib/R/library/ $(HOME)/.local/pipx/venvs/jupyter/lib/R/library

.PHONY: build-and-install
build-and-install: build install

.PHONY: check-cran
check-cran:
	R CMD check --as-cran $(ARCHIVE)

roxygenize:
	Rscript --vanilla -e 'roxygen2::roxygenize(package.dir="${SRCDIR}")'

win-builder-devel:
	curl -T $(ARCHIVE) ftp://win-builder.r-project.org/R-devel/

win-builder-release:
	curl -T $(ARCHIVE) ftp://win-builder.r-project.org/R-release/

win-builder-oldrelease:
	curl -T $(ARCHIVE) ftp://win-builder.r-project.org/R-oldrelease/

rhub-macos:
	Rscript --vanilla -e 'rhub::check("${ARCHIVE}",platform="macos-highsierra-release-cran",show_status = FALSE)'

rhub-windows:
	Rscript --vanilla -e 'rhub::check_on_windows("${ARCHIVE}",show_status = FALSE)'

rhub-for-cran:
	Rscript --vanilla -e 'rhub::check_for_cran("${ARCHIVE}",show_status = FALSE)'

rhub-rchk:
	Rscript --vanilla -e 'rhub::check("${ARCHIVE}",platform="ubuntu-rchk",show_status = FALSE)'

rhub-windows-devel:
	Rscript --vanilla -e 'rhub::check("${ARCHIVE}",platform="windows-x86_64-devel",show_status = FALSE)'

rhub-windows-oldrel:
	Rscript --vanilla -e 'rhub::check("${ARCHIVE}",platform="windows-x86_64-oldrel",show_status = FALSE)'

rhub-debian-clang-devel:
	Rscript --vanilla -e 'rhub::check("${ARCHIVE}",platform="debian-clang-devel",show_status = FALSE)'

rhub-fedora-clang-devel:
	Rscript --vanilla -e 'rhub::check("${ARCHIVE}",platform="fedora-clang-devel",show_status = FALSE)'

rhub-list-checks:
	Rscript -e 'rhub::list_my_checks("melff@elff.eu")'

check-reverse:
	rm -rfv depends/$(PACKAGE)_*
	cp -v $(ARCHIVE) depends/ 
	cp -rfv $(ARCHIVE) depends/*.Rcheck 
	Rscript --vanilla -e 'options(repos="https://ftp.gwdg.de/pub/misc/cran/"); tools::check_packages_in_dir("depends",reverse="all",Ncpus=8); tools::check_packages_in_dir_details("depends")'

clean-reverse:
	rm -rfv depends/*

install-deps:
	Rscript --vanilla -e 'devtools::install_deps(pkg="${SRCDIR}")'

# install-basic-deps:
# 	Rscript --vanilla -e 'install.packages(c("devtools","roxygen2"),repos="https://cloud.r-project.org")'

