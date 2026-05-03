# Widgets for Floating Point Number Ranges

An R6 class and a constructor function for the creation of widgets that
can be used to manipulate floating pairs of point numbers that are
bounded within an interval, where a pair defines a number range.

## Usage

``` r
BoundedFloatRangeWidget(value, min, max, ...)
```

## Arguments

- value:

  A pair of floating point values.

- min:

  The lower bound of the enclosing interval.

- max:

  The upper bound of the enclosing interval.

- ...:

  Other arguments, passed to the superclass initializer.

## Details

The function `BoundedFloatRangeWidget` creates objects of the R6 Class
"BoundedFloatRangeWidgetClass", which in turn have the S3 class
attribute "BoundedFloatRangeWidget"

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `ValueWidget` -\> `BoundedFloatRangeWidget`

## Public fields

- `value`:

  A [Float](https://melff.github.io/RKernel/reference/Float.md)
  traitlet.

- `min`:

  A [Float](https://melff.github.io/RKernel/reference/Float.md)
  traitlet, the minimum allowed value.

- `max`:

  A [Float](https://melff.github.io/RKernel/reference/Float.md)
  traitlet, the maximum allowed value.

## Methods

### Public methods

- [`BoundedFloatRangeWidget$validate_value()`](#method-BoundedFloatRangeWidget-validate_value)

- [`BoundedFloatRangeWidget$validate_min()`](#method-BoundedFloatRangeWidget-validate_min)

- [`BoundedFloatRangeWidget$validate_max()`](#method-BoundedFloatRangeWidget-validate_max)

- [`BoundedFloatRangeWidget$new()`](#method-BoundedFloatRangeWidget-initialize)

- [`BoundedFloatRangeWidget$clone()`](#method-BoundedFloatRangeWidget-clone)

Inherited methods

- [`HasTraits$notify()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-notify)
- [`HasTraits$observe()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-observe)
- [`HasTraits$validate()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-validate)
- [`Widget$_send()`](https://melff.github.io/RKernel/reference/Widget.html#method-_send)
- [`Widget$check_version()`](https://melff.github.io/RKernel/reference/Widget.html#method-check_version)
- [`Widget$close()`](https://melff.github.io/RKernel/reference/Widget.html#method-close)
- [`Widget$display_data()`](https://melff.github.io/RKernel/reference/Widget.html#method-display_data)
- [`Widget$get_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-get_state)
- [`Widget$handle_buffers()`](https://melff.github.io/RKernel/reference/Widget.html#method-handle_buffers)
- [`Widget$handle_comm_msg()`](https://melff.github.io/RKernel/reference/Widget.html#method-handle_comm_msg)
- [`Widget$handle_comm_opened()`](https://melff.github.io/RKernel/reference/Widget.html#method-handle_comm_opened)
- [`Widget$handle_custom_msg()`](https://melff.github.io/RKernel/reference/Widget.html#method-handle_custom_msg)
- [`Widget$handle_displayed()`](https://melff.github.io/RKernel/reference/Widget.html#method-handle_displayed)
- [`Widget$handle_event()`](https://melff.github.io/RKernel/reference/Widget.html#method-handle_event)
- [`Widget$on_displayed()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_displayed)
- [`Widget$on_event()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_event)
- [`Widget$on_msg()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_msg)
- [`Widget$open()`](https://melff.github.io/RKernel/reference/Widget.html#method-open)
- [`Widget$send()`](https://melff.github.io/RKernel/reference/Widget.html#method-send)
- [`Widget$send_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-send_state)
- [`Widget$set_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-set_state)
- [`DOMWidget$add_class()`](https://melff.github.io/RKernel/reference/DOMWidget.html#method-add_class)
- [`DOMWidget$has_class()`](https://melff.github.io/RKernel/reference/DOMWidget.html#method-has_class)
- [`DOMWidget$remove_class()`](https://melff.github.io/RKernel/reference/DOMWidget.html#method-remove_class)
- `ValueWidget$on_change()`

------------------------------------------------------------------------

### `BoundedFloatRangeWidget$validate_value()`

Validate the "value" after assignment.

#### Usage

    BoundedFloatRangeWidget$validate_value(value)

#### Arguments

- `value`:

  A value, should be numeric.

------------------------------------------------------------------------

### `BoundedFloatRangeWidget$validate_min()`

Validate the "min" field after assignment.

#### Usage

    BoundedFloatRangeWidget$validate_min(min)

#### Arguments

- `min`:

  A minimum value, should be numeric.

------------------------------------------------------------------------

### `BoundedFloatRangeWidget$validate_max()`

Validate the "max" field after assignment.

#### Usage

    BoundedFloatRangeWidget$validate_max(max)

#### Arguments

- `max`:

  A maximum value, should be numeric.

------------------------------------------------------------------------

### `BoundedFloatRangeWidget$new()`

#### Usage

    BoundedFloatRangeWidget$new(...)

#### Arguments

- `...`:

  Arguments passed to the superclass initializer.

------------------------------------------------------------------------

### `BoundedFloatRangeWidget$clone()`

The objects of this class are cloneable with this method.

#### Usage

    BoundedFloatRangeWidget$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
