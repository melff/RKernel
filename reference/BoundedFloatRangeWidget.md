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

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\>
[`RKernel::Widget`](https://melff.github.io/RKernel/reference/Widgets.md)
-\>
[`RKernel::DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`RKernel::DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `RKernel::ValueWidget` -\> `BoundedFloatRangeWidget`

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

- [`BoundedFloatRangeWidgetClass$validate_value()`](#method-BoundedFloatRangeWidget-validate_value)

- [`BoundedFloatRangeWidgetClass$validate_min()`](#method-BoundedFloatRangeWidget-validate_min)

- [`BoundedFloatRangeWidgetClass$validate_max()`](#method-BoundedFloatRangeWidget-validate_max)

- [`BoundedFloatRangeWidgetClass$new()`](#method-BoundedFloatRangeWidget-new)

- [`BoundedFloatRangeWidgetClass$clone()`](#method-BoundedFloatRangeWidget-clone)

Inherited methods

- [`RKernel::HasTraits$notify()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-notify)
- [`RKernel::HasTraits$observe()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-observe)
- [`RKernel::HasTraits$validate()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-validate)
- [`RKernel::Widget$_send()`](https://melff.github.io/RKernel/reference/Widget.html#method-_send)
- [`RKernel::Widget$check_version()`](https://melff.github.io/RKernel/reference/Widget.html#method-check_version)
- [`RKernel::Widget$close()`](https://melff.github.io/RKernel/reference/Widget.html#method-close)
- [`RKernel::Widget$display_data()`](https://melff.github.io/RKernel/reference/Widget.html#method-display_data)
- [`RKernel::Widget$get_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-get_state)
- [`RKernel::Widget$handle_buffers()`](https://melff.github.io/RKernel/reference/Widget.html#method-handle_buffers)
- [`RKernel::Widget$handle_comm_msg()`](https://melff.github.io/RKernel/reference/Widget.html#method-handle_comm_msg)
- [`RKernel::Widget$handle_comm_opened()`](https://melff.github.io/RKernel/reference/Widget.html#method-handle_comm_opened)
- [`RKernel::Widget$handle_custom_msg()`](https://melff.github.io/RKernel/reference/Widget.html#method-handle_custom_msg)
- [`RKernel::Widget$handle_displayed()`](https://melff.github.io/RKernel/reference/Widget.html#method-handle_displayed)
- [`RKernel::Widget$handle_event()`](https://melff.github.io/RKernel/reference/Widget.html#method-handle_event)
- [`RKernel::Widget$on_displayed()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_displayed)
- [`RKernel::Widget$on_event()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_event)
- [`RKernel::Widget$on_msg()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_msg)
- [`RKernel::Widget$open()`](https://melff.github.io/RKernel/reference/Widget.html#method-open)
- [`RKernel::Widget$send()`](https://melff.github.io/RKernel/reference/Widget.html#method-send)
- [`RKernel::Widget$send_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-send_state)
- [`RKernel::Widget$set_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-set_state)
- [`RKernel::DOMWidget$add_class()`](https://melff.github.io/RKernel/reference/DOMWidget.html#method-add_class)
- [`RKernel::DOMWidget$has_class()`](https://melff.github.io/RKernel/reference/DOMWidget.html#method-has_class)
- [`RKernel::DOMWidget$remove_class()`](https://melff.github.io/RKernel/reference/DOMWidget.html#method-remove_class)
- [`RKernel::ValueWidget$on_change()`](https://melff.github.io/RKernel/reference/ValueWidget.html#method-on_change)

------------------------------------------------------------------------

### Method `validate_value()`

Validate the "value" after assignment.

#### Usage

    BoundedFloatRangeWidgetClass$validate_value(value)

#### Arguments

- `value`:

  A value, should be numeric.

------------------------------------------------------------------------

### Method `validate_min()`

Validate the "min" field after assignment.

#### Usage

    BoundedFloatRangeWidgetClass$validate_min(min)

#### Arguments

- `min`:

  A minimum value, should be numeric.

------------------------------------------------------------------------

### Method `validate_max()`

Validate the "max" field after assignment.

#### Usage

    BoundedFloatRangeWidgetClass$validate_max(max)

#### Arguments

- `max`:

  A maximum value, should be numeric.

------------------------------------------------------------------------

### Method `new()`

#### Usage

    BoundedFloatRangeWidgetClass$new(...)

#### Arguments

- `...`:

  Arguments passed to the superclass initializer.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BoundedFloatRangeWidgetClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
