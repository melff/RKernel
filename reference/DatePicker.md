# Date Picker Widgets

An R6 class and constructor function for date picker widgets

## Usage

``` r
DatePicker(...)
```

## Arguments

- ...:

  Arguments passed to the inializer

## Functions

- `DatePicker()`: A constructor for dat picker widgets

## Super classes

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\>
[`RKernel::Widget`](https://melff.github.io/RKernel/reference/Widgets.md)
-\>
[`RKernel::DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`RKernel::DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `RKernel::ValueWidget` -\> `DatePicker`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend

- `_view_name`:

  Name of the Javascript view in the frontend

- `value`:

  The date

- `disabled`:

  Boolean, whether the user can make changes

- `min`:

  Minimum selectable date

- `max`:

  Maximum selectable date

- `step`:

  Date step used for the picker in days

## Methods

### Public methods

- [`DatePickerClass$validate_value()`](#method-DatePicker-validate_value)

- [`DatePickerClass$validate_min()`](#method-DatePicker-validate_min)

- [`DatePickerClass$validate_max()`](#method-DatePicker-validate_max)

- [`DatePickerClass$new()`](#method-DatePicker-new)

- [`DatePickerClass$clone()`](#method-DatePicker-clone)

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

Check wether "value" is within range.

#### Usage

    DatePickerClass$validate_value(value)

#### Arguments

- `value`:

  A value

------------------------------------------------------------------------

### Method `validate_min()`

Validate the "min" field after assignment.

#### Usage

    DatePickerClass$validate_min(min)

#### Arguments

- `min`:

  A minimum value, should be an integer number.

------------------------------------------------------------------------

### Method `validate_max()`

Validate the "max" field after assignment.

#### Usage

    DatePickerClass$validate_max(max)

#### Arguments

- `max`:

  A maximum value, should be an integer number.

------------------------------------------------------------------------

### Method `new()`

#### Usage

    DatePickerClass$new(...)

#### Arguments

- `...`:

  Arguments passed to the superclass initializer

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DatePickerClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
