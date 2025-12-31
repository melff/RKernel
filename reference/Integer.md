# Integer Traitlets

A class and a constructor function to create integer vector trait(let)s.

## Usage

``` r
Integer(...)

# S3 method for class 'Integer'
as.integer(x, ...)

# S3 method for class 'Integer'
as.numeric(x, ...)

# S3 method for class 'Integer'
to_json(x, ...)
```

## Arguments

- ...:

  Other arguments.

- x:

  An integer traitlet.

## Super class

[`RKernel::Trait`](https://melff.github.io/RKernel/reference/Traitlets.md)
-\> `Integer`

## Public fields

- `value`:

  An integer vector.

- `optional`:

  Logical value, whether a length-zero value is allowed.

- `coerce`:

  Logical value, whether assignments to the value field should be
  coerced to the appropriate type.

- `length`:

  Integer number, the length the value should have.

## Methods

### Public methods

- [`IntegerClass$validator()`](#method-Integer-validator)

- [`IntegerClass$new()`](#method-Integer-new)

- [`IntegerClass$clone()`](#method-Integer-clone)

Inherited methods

- [`RKernel::Trait$get()`](https://melff.github.io/RKernel/reference/Trait.html#method-get)
- [`RKernel::Trait$set()`](https://melff.github.io/RKernel/reference/Trait.html#method-set)

------------------------------------------------------------------------

### Method `validator()`

Check the value assigned to the traitlet.

#### Usage

    IntegerClass$validator(value)

#### Arguments

- `value`:

  The value assigned to the traitlet.

------------------------------------------------------------------------

### Method `new()`

Initialize the traitlet.

#### Usage

    IntegerClass$new(
      initial = integer(0),
      coerce = TRUE,
      optional = length(initial) == 0,
      length = 1L
    )

#### Arguments

- `initial`:

  An integer vector, the initial value for the traitlet.

- `coerce`:

  coerce Logical value, whether assignments to the value field should be
  coerced to the appropriate type.

- `optional`:

  Logical value, whether a length-zero value is allowed.

- `length`:

  Integer number, the length the value should have.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    IntegerClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
