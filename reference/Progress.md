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

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DescriptionStyle`](https://melff.github.io/RKernel/reference/DescriptionStyle.md)
-\> `ProgrssStyle`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend

- `bar_color`:

  The colour of the progress bar

## Methods

### Public methods

- [`ProgrssStyle$clone()`](#method-ProgrssStyle-clone)

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
- [`Widget$initialize()`](https://melff.github.io/RKernel/reference/Widget.html#method-initialize)
- [`Widget$on_displayed()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_displayed)
- [`Widget$on_event()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_event)
- [`Widget$on_msg()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_msg)
- [`Widget$open()`](https://melff.github.io/RKernel/reference/Widget.html#method-open)
- [`Widget$send()`](https://melff.github.io/RKernel/reference/Widget.html#method-send)
- [`Widget$send_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-send_state)
- [`Widget$set_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-set_state)

------------------------------------------------------------------------

### `ProgrssStyle$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ProgrssStyle$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `ValueWidget` -\>
[`BoundedIntWidget`](https://melff.github.io/RKernel/reference/BoundedIntWidget.md)
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

- [`IntProgress$clone()`](#method-IntProgress-clone)

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
- [`BoundedIntWidget$initialize()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-initialize)
- [`BoundedIntWidget$validate_max()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-validate_max)
- [`BoundedIntWidget$validate_min()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-validate_min)
- [`BoundedIntWidget$validate_value()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-validate_value)

------------------------------------------------------------------------

### `IntProgress$clone()`

The objects of this class are cloneable with this method.

#### Usage

    IntProgress$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `ValueWidget` -\>
[`BoundedFloatWidget`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.md)
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

- [`FloatProgress$clone()`](#method-FloatProgress-clone)

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
- [`BoundedFloatWidget$initialize()`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.html#method-initialize)
- [`BoundedFloatWidget$validate_max()`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.html#method-validate_max)
- [`BoundedFloatWidget$validate_min()`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.html#method-validate_min)
- [`BoundedFloatWidget$validate_value()`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.html#method-validate_value)

------------------------------------------------------------------------

### `FloatProgress$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FloatProgress$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
