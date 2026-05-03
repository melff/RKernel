# Traitlets

The class `TraitClass` brings (some of) the functionality of the
[traitlets framework](https://traitlets.readthedocs.io/) on which the
[ipywidgets framework](https://ipywidgets.readthedocs.io) is based to
*R*.

The function `TraitInstance` returns information needed by a
[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
object to construct a `TraitClass` object.

## Usage

``` r
Trait(...)

TraitInstance(Class, ...)
```

## Arguments

- ...:

  Arguments passed to the inializer

- Class:

  An R6 Class that inherits from "TraitClass"

## Functions

- `Trait()`: A Baseline Trait Constructor

- `TraitInstance()`: A "Delayed Constructor" for Traits, to be used by
  constructors of derived classes.

## Public fields

- `value`:

  The value of the trait

- `observers`:

  A list of functions to be called as notification callbacks

- `validators`:

  A list of functions to check the validity of a

## Methods

### Public methods

- [`Trait$set()`](#method-Trait-set)

- [`Trait$get()`](#method-Trait-get)

- [`Trait$new()`](#method-Trait-initialize)

- [`Trait$clone()`](#method-Trait-clone)

------------------------------------------------------------------------

### `Trait$set()`

Set the value of the trait

#### Usage

    Trait$set(value, notify = FALSE)

#### Arguments

- `value`:

  The value to be set

- `notify`:

  Logical; whether to call notification callbacks

------------------------------------------------------------------------

### `Trait$get()`

Get the trait value

#### Usage

    Trait$get()

------------------------------------------------------------------------

### `Trait$new()`

Initialize the trait, i.e. set an initial value

#### Usage

    Trait$new(initial)

#### Arguments

- `initial`:

  The initial value

- `coerce`:

  Logical; whether to coerce the initial value to the approriate mode.

------------------------------------------------------------------------

### `Trait$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Trait$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
