# Widgets for Bounded Floating Point Numbers

An R6 class and a constructor function for the creation of widgets that
can be used to manipulate floating point numbers that are bounded within
an interval

## Usage

``` r
BoundedFloatWidget(value, min, max, ...)
```

## Arguments

- value:

  The floating point value

- min:

  The lower bound of the interval

- max:

  The upper bound of the interval

- ...:

  Other arguments, passed to the superclass initializer

## Details

The function `BoundedFloatWidget` creates objects of the R6 Class
"BoundedFloatWidgetClass", which in turn have the S3 class attribute
"BoundedFloatWidget"

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `ValueWidget` -\> `BoundedFloatWidget`

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

- [`BoundedFloatWidget$validate_value()`](#method-BoundedFloatWidget-validate_value)

- [`BoundedFloatWidget$validate_min()`](#method-BoundedFloatWidget-validate_min)

- [`BoundedFloatWidget$validate_max()`](#method-BoundedFloatWidget-validate_max)

- [`BoundedFloatWidget$new()`](#method-BoundedFloatWidget-initialize)

- [`BoundedFloatWidget$clone()`](#method-BoundedFloatWidget-clone)

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

### `BoundedFloatWidget$validate_value()`

Validate the "value" after assignment.

#### Usage

    BoundedFloatWidget$validate_value(value)

#### Arguments

- `value`:

  A value, should be numeric.

------------------------------------------------------------------------

### `BoundedFloatWidget$validate_min()`

Validate the "min" field after assignment.

#### Usage

    BoundedFloatWidget$validate_min(min)

#### Arguments

- `min`:

  A minimum value, should be numeric.

------------------------------------------------------------------------

### `BoundedFloatWidget$validate_max()`

Validate the "max" field after assignment.

#### Usage

    BoundedFloatWidget$validate_max(max)

#### Arguments

- `max`:

  A maximum value, should be numeric.

------------------------------------------------------------------------

### `BoundedFloatWidget$new()`

Initialize an object.

#### Usage

    BoundedFloatWidget$new(value, min, max, ...)

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

### `BoundedFloatWidget$clone()`

The objects of this class are cloneable with this method.

#### Usage

    BoundedFloatWidget$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
