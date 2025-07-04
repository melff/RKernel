% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install.R
\name{install}
\alias{install}
\alias{installspec}
\title{install the R Kernel}
\usage{
install(...)

installspec(
  user = TRUE,
  prefix = NULL,
  kernel_name = "rkernel",
  display_name = "R",
  env = NULL
)
}
\arguments{
\item{...}{Arguments passed on to \code{installspec}}

\item{user}{Logical, whether to install the kernel in the user's home directory}

\item{prefix}{NULL or a character string with a path prefix}

\item{kernel_name}{String; the name of the kernel. To be used 
e.g. as the '--kernel=' argument for the CLI call 'jupyter console'}

\item{display_name}{String; the name as it appears in the Jupyter
web interface}

\item{env}{A list or NULL; optional environmental variables
for the R kernel process. For example, to get single-threaded 
open-blas use 
"\code{env = list(OPENBLAS_NUM_THREADS = 1)}".
To force a single OMP thread use
"\code{env = list(OMP_NUM_THREADS = 1)}"}
}
\description{
install the R Kernel
}
\section{Functions}{
\itemize{
\item \code{installspec()}: Install the R Kernel spec

}}
