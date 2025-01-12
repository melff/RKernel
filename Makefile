SRCDIR := pkg

PACKAGE := $(shell Rscript --vanilla -e 'cat(read.dcf(file="${SRCDIR}/DESCRIPTION",fields="Package"))')
VERSION := $(shell Rscript --vanilla -e 'cat(read.dcf(file="${SRCDIR}/DESCRIPTION",fields="Version"))')

ARCHIVE := $(PACKAGE)_$(VERSION).tar.gz

RCHKIMG := kalibera-rchk-master-def.simg

#export R_LIBS_USER = $(PWD)/lib/R/library

.PHONY: default

default: install-quick installspec 
	date

.PHONY: all
all: roxygenize build install installspec 
	date

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
	Rscript --vanilla -e 'RKernel::installspec()'

.PHONY: build-and-install
build-and-install: build install

.PHONY: check-cran
check-cran:
	R CMD check --as-cran $(ARCHIVE)

roxygenize:
	Rscript --vanilla -e 'roxygen2::roxygenize(package.dir="${SRCDIR}")'

install-deps:
	Rscript --vanilla -e 'devtools::install_deps(pkg="${SRCDIR}")'

# install-basic-deps:
# 	Rscript --vanilla -e 'install.packages(c("devtools","roxygen2"),repos="https://cloud.r-project.org")'

pkgdown:
	Rscript --vanilla -e 'pkgdown::build_site(pkg="${SRCDIR}")'