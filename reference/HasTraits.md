# The Base Class of Objects with Traits

Objects in class HasTraits have traits as components that are correctly
initialized using delayed construction with the
[`TraitInstance`](https://melff.github.io/RKernel/reference/Traitlets.md)
function.

## Public fields

- `call`:

  The generating call

- `traits`:

  A list of traits

- `suspended`:

  Logical value; whether notifying observers is suspended.

- `observers`:

  A list of observers, i.e. callback functions called by the `notify`
  method.

## Methods

### Public methods

- [`HasTraits$new()`](#method-HasTraits-new)

- [`HasTraits$notify()`](#method-HasTraits-notify)

- [`HasTraits$observe()`](#method-HasTraits-observe)

- [`HasTraits$validate()`](#method-HasTraits-validate)

- [`HasTraits$clone()`](#method-HasTraits-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize an object

#### Usage

    HasTraits$new(...)

#### Arguments

- `...`:

  Initializsing values

------------------------------------------------------------------------

### Method `notify()`

Notify observers about a trait being set to a value.

#### Usage

    HasTraits$notify(tn, value)

#### Arguments

- `tn`:

  A string, the name of the trait.

- `value`:

  The value to which the trait is set.

------------------------------------------------------------------------

### Method `observe()`

Install or remove an observer function.

#### Usage

    HasTraits$observe(tn, observer, remove = FALSE)

#### Arguments

- `tn`:

  A string, the name of a trait.

- `observer`:

  A callback function, which should take three arguments, (1) the trait
  name, (2) the object that has the trait, (3)

- `remove`:

  A logical value, indicates whether the observer is to be removed or
  added

------------------------------------------------------------------------

### Method `validate()`

Install or remove the validator function of a trait.

#### Usage

    HasTraits$validate(tn, validator, remove = FALSE)

#### Arguments

- `tn`:

  A string, the name of a trait.

- `validator`:

  A callback function

- `remove`:

  A logical value, indicates whether the validator is to be removed or
  added

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    HasTraits$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
