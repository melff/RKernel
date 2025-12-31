# install the R Kernel

install the R Kernel

## Usage

``` r
install(...)

installspec(
  user = TRUE,
  prefix = NULL,
  kernel_name = "rkernel",
  display_name = "R",
  env = NULL
)
```

## Arguments

- ...:

  Arguments passed on to `installspec`

- user:

  Logical, whether to install the kernel in the user's home directory

- prefix:

  NULL or a character string with a path prefix

- kernel_name:

  String; the name of the kernel. To be used e.g. as the '–kernel='
  argument for the CLI call 'jupyter console'

- display_name:

  String; the name as it appears in the Jupyter web interface

- env:

  A list or NULL; optional environmental variables for the R kernel
  process. For example, to get single-threaded open-blas use
  "`env = list(OPENBLAS_NUM_THREADS = 1)`". To force a single OMP thread
  use "`env = list(OMP_NUM_THREADS = 1)`"

## Functions

- `installspec()`: Install the R Kernel spec
