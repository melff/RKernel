# A Color String Trait

A Color String Trait

## Usage

``` r
Color(...)
```

## Arguments

- ...:

  Arguments passed to the trait instance initializer

## Super classes

[`Trait`](https://melff.github.io/RKernel/reference/Traitlets.md) -\>
[`Unicode`](https://melff.github.io/RKernel/reference/Unicode.md) -\>
`Color`

## Public fields

- `optional`:

  Logical value, whether a length-zero value is allowed.

## Methods

### Public methods

- [`Color$validator()`](#method-Color-validator)

- [`Color$clone()`](#method-Color-clone)

Inherited methods

- [`Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)
- [`Unicode$initialize()`](https://melff.github.io/RKernel/reference/Unicode.html#method-initialize)

------------------------------------------------------------------------

### `Color$validator()`

Check the value assigned to the traitlet.

#### Usage

    Color$validator(value)

#### Arguments

- `value`:

  The value assigned to the traitlet.

------------------------------------------------------------------------

### `Color$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Color$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
