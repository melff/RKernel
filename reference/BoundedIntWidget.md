# Widgets for Bounded Integer Numbers

An R6 class and a constructor function for the creation of widgets that
can be used to manipulate integer numbers that are bounded within an
interval.

## Usage

``` r
BoundedIntWidget(value, min, max, ...)

BoundedIntRangeWidget(value, min, max, ...)
```

## Arguments

- value:

  A pair of integer values

- min:

  The lower bound of the enclosing interval

- max:

  The upper bound of the enclosing interval

- ...:

  Other arguments, passed to the superclass initializer

## Details

The function `BoundedIntWidget` creates objects of the R6 Class
"BoundedIntWidgetClass", which in turn have the S3 class attribute
"BoundedIntWidget".

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `ValueWidget` -\> `BoundedIntWidget`

## Public fields

- `value`:

  A [Integer](https://melff.github.io/RKernel/reference/Integer.md)
  traitlet.

- `min`:

  A [Integer](https://melff.github.io/RKernel/reference/Integer.md)
  traitlet, the minimum allowed value.

- `max`:

  A [Integer](https://melff.github.io/RKernel/reference/Integer.md)
  traitlet, the maximum allowed value.

## Methods

### Public methods

- [`BoundedIntWidget$validate_value()`](#method-BoundedIntWidget-validate_value)

- [`BoundedIntWidget$validate_min()`](#method-BoundedIntWidget-validate_min)

- [`BoundedIntWidget$validate_max()`](#method-BoundedIntWidget-validate_max)

- [`BoundedIntWidget$new()`](#method-BoundedIntWidget-initialize)

- [`BoundedIntWidget$clone()`](#method-BoundedIntWidget-clone)

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

### `BoundedIntWidget$validate_value()`

Validate the "value" after assignment.

#### Usage

    BoundedIntWidget$validate_value(value)

#### Arguments

- `value`:

  A value, should be an integer number.

------------------------------------------------------------------------

### `BoundedIntWidget$validate_min()`

Validate the "min" field after assignment.

#### Usage

    BoundedIntWidget$validate_min(min)

#### Arguments

- `min`:

  A minimum value, should be an integer number.

------------------------------------------------------------------------

### `BoundedIntWidget$validate_max()`

Validate the "max" field after assignment.

#### Usage

    BoundedIntWidget$validate_max(max)

#### Arguments

- `max`:

  A maximum value, should be an integer number.

------------------------------------------------------------------------

### `BoundedIntWidget$new()`

#### Usage

    BoundedIntWidget$new(...)

#### Arguments

- `...`:

  Arguments passed to the superclass initializer

------------------------------------------------------------------------

### `BoundedIntWidget$clone()`

The objects of this class are cloneable with this method.

#### Usage

    BoundedIntWidget$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `ValueWidget` -\> `BoundedIntRangeWidget`

## Public fields

- `value`:

  A [Integer](https://melff.github.io/RKernel/reference/Integer.md)
  traitlet.

- `min`:

  A [Integer](https://melff.github.io/RKernel/reference/Integer.md)
  traitlet, the minimum allowed value.

- `max`:

  A [Integer](https://melff.github.io/RKernel/reference/Integer.md)
  traitlet, the maximum allowed value.

## Methods

### Public methods

- [`BoundedIntRangeWidget$validate_value()`](#method-BoundedIntRangeWidget-validate_value)

- [`BoundedIntRangeWidget$validate_min()`](#method-BoundedIntRangeWidget-validate_min)

- [`BoundedIntRangeWidget$validate_max()`](#method-BoundedIntRangeWidget-validate_max)

- [`BoundedIntRangeWidget$new()`](#method-BoundedIntRangeWidget-initialize)

- [`BoundedIntRangeWidget$clone()`](#method-BoundedIntRangeWidget-clone)

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

### `BoundedIntRangeWidget$validate_value()`

Validate the "value" after assignment.

#### Usage

    BoundedIntRangeWidget$validate_value(value)

#### Arguments

- `value`:

  A value, should be an integer number.

------------------------------------------------------------------------

### `BoundedIntRangeWidget$validate_min()`

Validate the "min" field after assignment.

#### Usage

    BoundedIntRangeWidget$validate_min(min)

#### Arguments

- `min`:

  A minimum value, should be an integer number.

------------------------------------------------------------------------

### `BoundedIntRangeWidget$validate_max()`

Validate the "max" field after assignment.

#### Usage

    BoundedIntRangeWidget$validate_max(max)

#### Arguments

- `max`:

  A maximum value, should be an integer number.

------------------------------------------------------------------------

### `BoundedIntRangeWidget$new()`

#### Usage

    BoundedIntRangeWidget$new(...)

#### Arguments

- `...`:

  Arguments passed to the superclass initializer.

------------------------------------------------------------------------

### `BoundedIntRangeWidget$clone()`

The objects of this class are cloneable with this method.

#### Usage

    BoundedIntRangeWidget$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
