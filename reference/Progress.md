# Progress bars

Classes and constructor functions for progress bars and styling of them

## Usage

``` r
ProgressStyle(...)

IntProgress(value = 0L, min = 0L, max = 100L, ...)

FloatProgress(value = 0L, min = 0L, max = 100L, ...)
```

## Arguments

- ...:

  Other arguments.

- value:

  A floating point number, the initial position of the progress bar

- min:

  An floating point number, the minumum value

- max:

  An floating point number, the maximum value

## Functions

- `ProgressStyle()`: A constructor for a progress bar style

- `IntProgress()`: A constructor for a progress bar style

- `FloatProgress()`: A constructor for a progress bar style

## Super classes

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\>
[`RKernel::Widget`](https://melff.github.io/RKernel/reference/Widgets.md)
-\>
[`RKernel::DescriptionStyle`](https://melff.github.io/RKernel/reference/DescriptionStyle.md)
-\> `ProgrssStyle`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend

- `bar_color`:

  The colour of the progress bar

## Methods

### Public methods

- [`ProgressStyleClass$clone()`](#method-ProgrssStyle-clone)

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
- [`RKernel::Widget$initialize()`](https://melff.github.io/RKernel/reference/Widget.html#method-initialize)
- [`RKernel::Widget$on_displayed()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_displayed)
- [`RKernel::Widget$on_event()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_event)
- [`RKernel::Widget$on_msg()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_msg)
- [`RKernel::Widget$open()`](https://melff.github.io/RKernel/reference/Widget.html#method-open)
- [`RKernel::Widget$send()`](https://melff.github.io/RKernel/reference/Widget.html#method-send)
- [`RKernel::Widget$send_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-send_state)
- [`RKernel::Widget$set_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-set_state)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ProgressStyleClass$clone(deep = FALSE)

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
-\> `RKernel::ValueWidget` -\>
[`RKernel::BoundedIntWidget`](https://melff.github.io/RKernel/reference/BoundedIntWidget.md)
-\> `IntProgress`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend

- `_view_name`:

  Name of the Javascript view in the frontend

- `orientation`:

  Orientation of the progress bar, either "horizontal" or "vertical"

- `bar_style`:

  General style of the progress bar, either "successs", "info",
  "warning" or "danger"

- `style`:

  Styling of the progress bar, an instance of "ProgressStyleClass"

## Methods

### Public methods

- [`IntProgressClass$clone()`](#method-IntProgress-clone)

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
- [`RKernel::BoundedIntWidget$initialize()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-initialize)
- [`RKernel::BoundedIntWidget$validate_max()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-validate_max)
- [`RKernel::BoundedIntWidget$validate_min()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-validate_min)
- [`RKernel::BoundedIntWidget$validate_value()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-validate_value)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    IntProgressClass$clone(deep = FALSE)

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
-\> `RKernel::ValueWidget` -\>
[`RKernel::BoundedFloatWidget`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.md)
-\> `FloatProgress`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend

- `_view_name`:

  Name of the Javascript view in the frontend

- `orientation`:

  Orientation of the progress bar, either "horizontal" or "vertical"

- `bar_style`:

  General style of the progress bar, either "successs", "info",
  "warning" or "danger"

- `style`:

  Styling of the progress bar, an instance of "ProgressStyleClass"

## Methods

### Public methods

- [`FloatProgressClass$clone()`](#method-FloatProgress-clone)

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
- [`RKernel::BoundedFloatWidget$initialize()`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.html#method-initialize)
- [`RKernel::BoundedFloatWidget$validate_max()`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.html#method-validate_max)
- [`RKernel::BoundedFloatWidget$validate_min()`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.html#method-validate_min)
- [`RKernel::BoundedFloatWidget$validate_value()`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.html#method-validate_value)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FloatProgressClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
