# A data type analogous to Python dictionaries

Objects of class "dictionary" behave similar to dictionaries. They can
contain any other kind of objects, but like with Python dictionaries,
only scalar indices are allowed. Unlike with Python dictionaries,
numeric indices can be used well as character indices.

## Usage

``` r
dictionary(...)

# S3 method for class 'dictionary'
x[i]

# S3 method for class 'dictionary'
x[i] <- value

# S3 method for class 'dictionary'
print(x, force = FALSE, ...)
```

## Arguments

- ...:

  Arbitrary objects. Should be tagged, yet currently name tags are not
  yet checked for.

- x:

  A dictionary object

- i:

  A scalar integer or character string

- value:

  An arbitrary object

- force:

  A logical scalar, if TRUE, each element of the dictionary is printed,
  if FALSE, just a brief summary is printed.

## Methods (by generic)

- `[`: Get an element from a dictionary

- `` `[`(dictionary) <- value ``: Set an element in a dictionary

- `print(dictionary)`: Print a dictionary

## Functions

- `dictionary()`: A dictionary constructor
