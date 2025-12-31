# Comms - connections between the kernel and the frontend

This R6 Class provides for bidirectional communication between the R
Kernel and the Jupyter frontend, e.g. a Jupyter notebook

## Usage

``` r
Comm(...)
```

## Arguments

- ...:

  Arguments passed to the inializer

## Details

Objects of this class are used to communicate to the frontend via
[custom
messages](https://jupyter-client.readthedocs.io/en/latest/messaging.html#custom-messages).

## Functions

- `Comm()`: A constructor function for objects of class "CommClass"

## Public fields

- `id`:

  A character string, the comm id

- `target_name`:

  A character string, the target

- `handlers`:

  A list of handler functions

- `data`:

  A list of data

## Methods

### Public methods

- [`CommClass$new()`](#method-Comm-new)

- [`CommClass$open()`](#method-Comm-open)

- [`CommClass$send()`](#method-Comm-send)

- [`CommClass$close()`](#method-Comm-close)

- [`CommClass$clone()`](#method-Comm-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a 'Comm' object

#### Usage

    CommClass$new(target_name, id = uuid(), handlers = list())

#### Arguments

- `target_name`:

  A string, the name of the target

- `id`:

  A string, the comm id

- `handlers`:

  A list of handler functions

- `kernel`:

  The relevant kernel

------------------------------------------------------------------------

### Method [`open()`](https://rdrr.io/r/base/connections.html)

Open a comm

#### Usage

    CommClass$open(data, metadata = emptyNamedList, buffers = NULL)

#### Arguments

- `data`:

  A named list

- `metadata`:

  A named list

- `buffers`:

  A list of raw vectors or NULL

------------------------------------------------------------------------

### Method `send()`

Send data through a comm

#### Usage

    CommClass$send(data, metadata = emptyNamedList, buffers = NULL)

#### Arguments

- `data`:

  A named list

- `metadata`:

  A named list

- `buffers`:

  A list of raw vectors or NULL

------------------------------------------------------------------------

### Method [`close()`](https://rdrr.io/r/base/connections.html)

Close a comm

#### Usage

    CommClass$close(
      data = emptyNamedList,
      metadata = emptyNamedList,
      buffers = NULL
    )

#### Arguments

- `data`:

  A named list

- `metadata`:

  A named list

- `buffers`:

  A list of raw vectors or NULL

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CommClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
