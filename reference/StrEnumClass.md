# An Enumerated Strings Trait

An Enumerated Strings Trait

An Enumerated Strings Trait

## Super classes

[`RKernel::Trait`](https://melff.github.io/RKernel/reference/Traitlets.md)
-\>
[`RKernel::Unicode`](https://melff.github.io/RKernel/reference/Unicode.md)
-\> `StrEnum`

## Public fields

- `enum`:

  a character vector

- `optional`:

  A logical value, whether value can be empty.

## Methods

### Public methods

- [`StrEnumClass$validator()`](#method-StrEnum-validator)

- [`StrEnumClass$new()`](#method-StrEnum-new)

- [`StrEnumClass$clone()`](#method-StrEnum-clone)

Inherited methods

- [`RKernel::Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`RKernel::Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### Method `validator()`

Check whether the assigned vector is one of the allowed enumerated
strings.

#### Usage

    StrEnumClass$validator(value)

#### Arguments

- `value`:

  A value to be assigned to the trait

------------------------------------------------------------------------

### Method `new()`

Initialize the trait.

#### Usage

    StrEnumClass$new(enum, default = character(0), optional = FALSE)

#### Arguments

- `enum`:

  A character vector of permitted enumerated strings.

- `default`:

  The default value

- `optional`:

  Logical can the value be empty?

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    StrEnumClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
