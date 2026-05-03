# An Enumerated Strings Trait

An Enumerated Strings Trait

## Super classes

[`Trait`](https://melff.github.io/RKernel/reference/Traitlets.md) -\>
[`Unicode`](https://melff.github.io/RKernel/reference/Unicode.md) -\>
`StrEnum`

## Public fields

- `enum`:

  a character vector

- `optional`:

  A logical value, whether value can be empty.

## Methods

### Public methods

- [`StrEnum$validator()`](#method-StrEnum-validator)

- [`StrEnum$new()`](#method-StrEnum-initialize)

- [`StrEnum$clone()`](#method-StrEnum-clone)

Inherited methods

- [`Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### `StrEnum$validator()`

Check whether the assigned vector is one of the allowed enumerated
strings.

#### Usage

    StrEnum$validator(value)

#### Arguments

- `value`:

  A value to be assigned to the trait

------------------------------------------------------------------------

### `StrEnum$new()`

Initialize the trait.

#### Usage

    StrEnum$new(enum, default = character(0), optional = FALSE)

#### Arguments

- `enum`:

  A character vector of permitted enumerated strings.

- `default`:

  The default value

- `optional`:

  Logical can the value be empty?

------------------------------------------------------------------------

### `StrEnum$clone()`

The objects of this class are cloneable with this method.

#### Usage

    StrEnum$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
