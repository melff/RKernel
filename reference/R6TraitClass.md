# A Base Class for Traits that are R6 Objects

A Base Class for Traits that are R6 Objects

A Base Class for Traits that are R6 Objects

## Super class

[`RKernel::Trait`](https://melff.github.io/RKernel/reference/Traitlets.md)
-\> `R6Trait`

## Public fields

- `value`:

  An R6 object

- `class`:

  The R6 class of `value`

## Methods

### Public methods

- [`R6TraitClass$validator()`](#method-R6Trait-validator)

- [`R6TraitClass$new()`](#method-R6Trait-new)

- [`R6TraitClass$clone()`](#method-R6Trait-clone)

Inherited methods

- [`RKernel::Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`RKernel::Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### Method `validator()`

Checks wether `value` has the corret class

#### Usage

    R6TraitClass$validator(value)

#### Arguments

- `value`:

  A value about to be assigned to the trait.

------------------------------------------------------------------------

### Method `new()`

Initialize an object

#### Usage

    R6TraitClass$new(Class, ...)

#### Arguments

- `Class`:

  Class of the object

- `...`:

  Values used for initialization

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    R6TraitClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
