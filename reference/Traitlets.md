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

- [`TraitClass$set()`](#method-Trait-set)

- [`TraitClass$get()`](#method-Trait-get)

- [`TraitClass$new()`](#method-Trait-new)

- [`TraitClass$clone()`](#method-Trait-clone)

------------------------------------------------------------------------

### Method `set()`

Set the value of the trait

#### Usage

    TraitClass$set(value, notify = FALSE)

#### Arguments

- `value`:

  The value to be set

- `notify`:

  Logical; whether to call notification callbacks

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get the trait value

#### Usage

    TraitClass$get()

------------------------------------------------------------------------

### Method `new()`

Initialize the trait, i.e. set an initial value

#### Usage

    TraitClass$new(initial)

#### Arguments

- `initial`:

  The initial value

- `coerce`:

  Logical; whether to coerce the initial value to the approriate mode.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TraitClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
