# A Base Class for Traits that are R6 Objects

A Base Class for Traits that are R6 Objects

## Super class

[`Trait`](https://melff.github.io/RKernel/reference/Traitlets.md) -\>
`R6Trait`

## Public fields

- `value`:

  An R6 object

- `class`:

  The R6 class of `value`

## Methods

### Public methods

- [`R6Trait$validator()`](#method-R6Trait-validator)

- [`R6Trait$new()`](#method-R6Trait-initialize)

- [`R6Trait$clone()`](#method-R6Trait-clone)

Inherited methods

- [`Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### `R6Trait$validator()`

Checks wether `value` has the corret class

#### Usage

    R6Trait$validator(value)

#### Arguments

- `value`:

  A value about to be assigned to the trait.

------------------------------------------------------------------------

### `R6Trait$new()`

Initialize an object

#### Usage

    R6Trait$new(Class, ...)

#### Arguments

- `Class`:

  Class of the object

- `...`:

  Values used for initialization

------------------------------------------------------------------------

### `R6Trait$clone()`

The objects of this class are cloneable with this method.

#### Usage

    R6Trait$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
