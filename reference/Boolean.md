# Boolean Traitlets

A class and a constructor function to create boolean trait(let)s.

## Usage

``` r
Boolean(...)
```

## Arguments

- ...:

  Arguments that are passed to the initialize method of 'BooleanClass'

## Super class

[`RKernel::Trait`](https://melff.github.io/RKernel/reference/Traitlets.md)
-\> `Boolean`

## Public fields

- `value`:

  A logical vector, usually of length 1

- `optional`:

  Logical, whether an initializing logical value must be provided

- `coerce`:

  Logical, whether 'as.logical()' is implicitely used when a value is
  assigned to the trait

- `length`:

  Integer, the length of the logical vector that poses as the value of
  the traitlet.

## Methods

### Public methods

- [`BooleanClass$validator()`](#method-Boolean-validator)

- [`BooleanClass$new()`](#method-Boolean-new)

- [`BooleanClass$clone()`](#method-Boolean-clone)

Inherited methods

- [`RKernel::Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`RKernel::Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### Method `validator()`

A validator method

#### Usage

    BooleanClass$validator(value)

#### Arguments

- `value`:

  A value to be checked for validity

------------------------------------------------------------------------

### Method `new()`

The initializing method

#### Usage

    BooleanClass$new(
      initial,
      coerce = TRUE,
      optional = length(initial) == 0,
      length = 1
    )

#### Arguments

- `initial`:

  A value with which the traitlet is initialized

- `coerce`:

  Logical, used to initialize the 'coerce' field

- `optional`:

  Logical, used to initialize the 'optional' field

- `length`:

  Integer, used to initialize the 'length' field

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BooleanClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
