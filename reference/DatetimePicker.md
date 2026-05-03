# Datetime Picker Widgets

An R6 class and constructor function for datetime picker widgets

## Usage

``` r
DatetimePicker(...)

NaiveDatetimePicker(...)
```

## Arguments

- ...:

  Arguments passed to the inializer

## Functions

- `DatetimePicker()`: A constructor for dat picker widgets

- `NaiveDatetimePicker()`: A constructor for dat picker widgets

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `ValueWidget` -\> `DatetimePicker`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend

- `_view_name`:

  Name of the Javascript view in the frontend

- `value`:

  The date and time. If non-zero length, must have valid timezone info.

- `disabled`:

  Boolean, whether the user can make changes

- `min`:

  Minimum selectable date and time. If non-zero length, must have valid
  timezone info.

- `max`:

  Maximum selectable date and time. If non-zero length, must have valid
  timezone info.

## Methods

### Public methods

- [`DatetimePicker$validate_tz()`](#method-DatetimePicker-validate_tz)

- [`DatetimePicker$validate_value()`](#method-DatetimePicker-validate_value)

- [`DatetimePicker$validate_min()`](#method-DatetimePicker-validate_min)

- [`DatetimePicker$validate_max()`](#method-DatetimePicker-validate_max)

- [`DatetimePicker$new()`](#method-DatetimePicker-initialize)

- [`DatetimePicker$clone()`](#method-DatetimePicker-clone)

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

### `DatetimePicker$validate_tz()`

Check whether time zone is valid.

#### Usage

    DatetimePicker$validate_tz(value)

#### Arguments

- `value`:

  A value

------------------------------------------------------------------------

### `DatetimePicker$validate_value()`

Check wether "value" is within range.

#### Usage

    DatetimePicker$validate_value(value)

#### Arguments

- `value`:

  A date and time to be checked for validity

------------------------------------------------------------------------

### `DatetimePicker$validate_min()`

Validate the "min" field after assignment.

#### Usage

    DatetimePicker$validate_min(min)

#### Arguments

- `min`:

  A minimum date and time to be checked for validity

------------------------------------------------------------------------

### `DatetimePicker$validate_max()`

Validate the "max" field after assignment.

#### Usage

    DatetimePicker$validate_max(max)

#### Arguments

- `max`:

  A maximum date and time to be checked for validity

------------------------------------------------------------------------

### `DatetimePicker$new()`

#### Usage

    DatetimePicker$new(...)

#### Arguments

- `...`:

  Arguments passed to the superclass initializer

------------------------------------------------------------------------

### `DatetimePicker$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DatetimePicker$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `ValueWidget` -\> `DatetimePicker` -\> `NaiveDatetimePicker`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend

- `value`:

  The date and time. If non-zero length, must have valid timezone info.

- `min`:

  Minimum selectable date and time. If non-zero length, must have valid
  timezone info.

- `max`:

  Maximum selectable date and time. If non-zero length, must have valid
  timezone info.

## Methods

### Public methods

- [`NaiveDatetimePicker$validate_tz()`](#method-NaiveDatetimePicker-validate_tz)

- [`NaiveDatetimePicker$validate_value()`](#method-NaiveDatetimePicker-validate_value)

- [`NaiveDatetimePicker$new()`](#method-NaiveDatetimePicker-initialize)

- [`NaiveDatetimePicker$clone()`](#method-NaiveDatetimePicker-clone)

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
- [`DatetimePicker$validate_max()`](https://melff.github.io/RKernel/reference/DatetimePicker.html#method-validate_max)
- [`DatetimePicker$validate_min()`](https://melff.github.io/RKernel/reference/DatetimePicker.html#method-validate_min)

------------------------------------------------------------------------

### `NaiveDatetimePicker$validate_tz()`

Check whether time zone is valid.

#### Usage

    NaiveDatetimePicker$validate_tz(value)

#### Arguments

- `value`:

  A value

------------------------------------------------------------------------

### `NaiveDatetimePicker$validate_value()`

Check wether "value" is within range.

#### Usage

    NaiveDatetimePicker$validate_value(value)

#### Arguments

- `value`:

  A date and time to be checked for validity

------------------------------------------------------------------------

### `NaiveDatetimePicker$new()`

#### Usage

    NaiveDatetimePicker$new(...)

#### Arguments

- `...`:

  Arguments passed to the superclass initializer

------------------------------------------------------------------------

### `NaiveDatetimePicker$clone()`

The objects of this class are cloneable with this method.

#### Usage

    NaiveDatetimePicker$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
