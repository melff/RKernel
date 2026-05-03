# Widgets for Bounded Floating Point Numbers on a logarithmic scale

An R6 class and a constructor function for the creation of widgets that
can be used to manipulate floating point numbers that are bounded within
an interval on an logarithmic scale

## Usage

``` r
BoundedLogFloatWidget(value, min, max, base, ...)
```

## Arguments

- value:

  The floating point value.

- min:

  The lower bound of the interval.

- max:

  The upper bound of the interval.

- base:

  The base of the logarithm.

- ...:

  Other arguments, passed to the superclass initializer.

## Details

The function `BoundedLogFloatWidget` creates objects of the R6 Class
"BoundedLogFloatWidgetClass", which in turn have the S3 class attribute
"BoundedLogFloatWidget"

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `ValueWidget` -\> `BoundedLogFloatWidget`

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

- `base`:

  A [Float](https://melff.github.io/RKernel/reference/Float.md)
  traitlet, the logarithmic base.

## Methods

### Public methods

- [`BoundedLogFloatWidget$validate_value()`](#method-BoundedLogFloatWidget-validate_value)

- [`BoundedLogFloatWidget$validate_min()`](#method-BoundedLogFloatWidget-validate_min)

- [`BoundedLogFloatWidget$validate_max()`](#method-BoundedLogFloatWidget-validate_max)

- [`BoundedLogFloatWidget$new()`](#method-BoundedLogFloatWidget-initialize)

- [`BoundedLogFloatWidget$clone()`](#method-BoundedLogFloatWidget-clone)

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

### `BoundedLogFloatWidget$validate_value()`

Validate the "value" after assignment.

#### Usage

    BoundedLogFloatWidget$validate_value(value)

#### Arguments

- `value`:

  A value, should be numeric.

------------------------------------------------------------------------

### `BoundedLogFloatWidget$validate_min()`

Validate the "min" field after assignment.

#### Usage

    BoundedLogFloatWidget$validate_min(min)

#### Arguments

- `min`:

  A minimum value, should be numeric.

------------------------------------------------------------------------

### `BoundedLogFloatWidget$validate_max()`

Validate the "max" field after assignment.

#### Usage

    BoundedLogFloatWidget$validate_max(max)

#### Arguments

- `max`:

  A maximum value, should be numeric.

------------------------------------------------------------------------

### `BoundedLogFloatWidget$new()`

Initialize an object.

#### Usage

    BoundedLogFloatWidget$new(value, min, max, ...)

#### Arguments

- `value`:

  The floating point value.

- `min`:

  The lower bound of the interval.

- `max`:

  The upper bound of the interval.

- `...`:

  Other arguments, passed to the superclass initializer.

------------------------------------------------------------------------

### `BoundedLogFloatWidget$clone()`

The objects of this class are cloneable with this method.

#### Usage

    BoundedLogFloatWidget$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
