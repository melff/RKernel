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

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\>
[`RKernel::Widget`](https://melff.github.io/RKernel/reference/Widgets.md)
-\>
[`RKernel::DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`RKernel::DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `RKernel::ValueWidget` -\> `DatetimePicker`

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

- [`DatetimePickerClass$validate_tz()`](#method-DatetimePicker-validate_tz)

- [`DatetimePickerClass$validate_value()`](#method-DatetimePicker-validate_value)

- [`DatetimePickerClass$validate_min()`](#method-DatetimePicker-validate_min)

- [`DatetimePickerClass$validate_max()`](#method-DatetimePicker-validate_max)

- [`DatetimePickerClass$new()`](#method-DatetimePicker-new)

- [`DatetimePickerClass$clone()`](#method-DatetimePicker-clone)

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

### Method `validate_tz()`

Check whether time zone is valid.

#### Usage

    DatetimePickerClass$validate_tz(value)

#### Arguments

- `value`:

  A value

------------------------------------------------------------------------

### Method `validate_value()`

Check wether "value" is within range.

#### Usage

    DatetimePickerClass$validate_value(value)

#### Arguments

- `value`:

  A date and time to be checked for validity

------------------------------------------------------------------------

### Method `validate_min()`

Validate the "min" field after assignment.

#### Usage

    DatetimePickerClass$validate_min(min)

#### Arguments

- `min`:

  A minimum date and time to be checked for validity

------------------------------------------------------------------------

### Method `validate_max()`

Validate the "max" field after assignment.

#### Usage

    DatetimePickerClass$validate_max(max)

#### Arguments

- `max`:

  A maximum date and time to be checked for validity

------------------------------------------------------------------------

### Method `new()`

#### Usage

    DatetimePickerClass$new(...)

#### Arguments

- `...`:

  Arguments passed to the superclass initializer

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DatetimePickerClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\>
[`RKernel::Widget`](https://melff.github.io/RKernel/reference/Widgets.md)
-\>
[`RKernel::DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`RKernel::DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `RKernel::ValueWidget` -\> `RKernel::DatetimePicker` -\>
`NaiveDatetimePicker`

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

- [`NaiveDatetimePickerClass$validate_tz()`](#method-NaiveDatetimePicker-validate_tz)

- [`NaiveDatetimePickerClass$validate_value()`](#method-NaiveDatetimePicker-validate_value)

- [`NaiveDatetimePickerClass$new()`](#method-NaiveDatetimePicker-new)

- [`NaiveDatetimePickerClass$clone()`](#method-NaiveDatetimePicker-clone)

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
- [`RKernel::DatetimePicker$validate_max()`](https://melff.github.io/RKernel/reference/DatetimePicker.html#method-validate_max)
- [`RKernel::DatetimePicker$validate_min()`](https://melff.github.io/RKernel/reference/DatetimePicker.html#method-validate_min)

------------------------------------------------------------------------

### Method `validate_tz()`

Check whether time zone is valid.

#### Usage

    NaiveDatetimePickerClass$validate_tz(value)

#### Arguments

- `value`:

  A value

------------------------------------------------------------------------

### Method `validate_value()`

Check wether "value" is within range.

#### Usage

    NaiveDatetimePickerClass$validate_value(value)

#### Arguments

- `value`:

  A date and time to be checked for validity

------------------------------------------------------------------------

### Method `new()`

#### Usage

    NaiveDatetimePickerClass$new(...)

#### Arguments

- `...`:

  Arguments passed to the superclass initializer

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    NaiveDatetimePickerClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
