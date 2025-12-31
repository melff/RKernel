# A Color String Trait

A Color String Trait

A Color String Trait

## Usage

``` r
Color(...)
```

## Arguments

- ...:

  Arguments passed to the trait instance initializer

## Super classes

[`RKernel::Trait`](https://melff.github.io/RKernel/reference/Traitlets.md)
-\>
[`RKernel::Unicode`](https://melff.github.io/RKernel/reference/Unicode.md)
-\> `Color`

## Public fields

- `optional`:

  Logical value, whether a length-zero value is allowed.

## Methods

### Public methods

- [`ColorTraitClass$validator()`](#method-Color-validator)

- [`ColorTraitClass$clone()`](#method-Color-clone)

Inherited methods

- [`RKernel::Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`RKernel::Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)
- [`RKernel::Unicode$initialize()`](https://melff.github.io/RKernel/reference/Unicode.html#method-initialize)

------------------------------------------------------------------------

### Method `validator()`

Check the value assigned to the traitlet.

#### Usage

    ColorTraitClass$validator(value)

#### Arguments

- `value`:

  The value assigned to the traitlet.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ColorTraitClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
