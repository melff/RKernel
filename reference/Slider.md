# Sliders

Classes and constructor functions for sliders (integer and
floating-point ones)

## Usage

``` r
IntSlider(value = 0L, min = 0L, max = 100L, ...)

IntRangeSlider(value = c(0L, 50L), min = 0L, max = 100L, ...)

FloatSlider(value = 0, min = 0, max = 100, ...)

FloatRangeSlider(value = c(0, 50), min = 0, max = 100, ...)

FloatLogSlider(value = 1, min = 0, max = 40, base = 10, ...)
```

## Arguments

- value:

  A floating point number, the current value of the slider

- min:

  A floating point number, the minimum value

- max:

  A floating point number, the maximum value

- ...:

  Other arguments.

- base:

  A floating point number, the base of the logarithm

## Functions

- `IntSlider()`: An integer slider constructor

- `IntRangeSlider()`: An integer range slider constructor

- `FloatSlider()`: A floating point slider constructor

- `FloatRangeSlider()`: A floating point slider range constructor

- `FloatLogSlider()`: A floating point log-slider constructor

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DescriptionStyle`](https://melff.github.io/RKernel/reference/DescriptionStyle.md)
-\> `SliderStyle`

## Public fields

- `_model_name`:

  Name of the Javascript frontend model

- `handle_color`:

  Unicode string, the color of the slider handle

## Methods

### Public methods

- [`SliderStyle$clone()`](#method-SliderStyle-clone)

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

### `SliderStyle$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SliderStyle$clone(deep = FALSE)

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
-\> `IntSlider`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend

- `_view_name`:

  Name of the Javascript view in the frontend

- `step`:

  An [`Integer`](https://melff.github.io/RKernel/reference/Integer.md)
  traitlet, the minimal step size per slider movement

- `orientation`:

  A Unicode string, either "horizontal" or "vertical"

- `readout`:

  A logical value, whether the value should be showns (read out)

- `readout_format`:

  A unicode string, the format specification

- `continuous_update`:

  A logical value, whether values should be updated as the slider is
  moved by the user

- `disabled`:

  A logical value, whether the slider is disabled

- `style`:

  A SliderStyle widget

- `behavior`:

  A string that describes the ddragging behavior.

## Methods

### Public methods

- [`IntSlider$clone()`](#method-IntSlider-clone)

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

### `IntSlider$clone()`

The objects of this class are cloneable with this method.

#### Usage

    IntSlider$clone(deep = FALSE)

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
[`BoundedIntRangeWidget`](https://melff.github.io/RKernel/reference/BoundedIntWidget.md)
-\> `IntRangeSlider`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend

- `_view_name`:

  Name of the Javascript view in the frontend

- `step`:

  An [`Integer`](https://melff.github.io/RKernel/reference/Integer.md)
  traitlet, the minimal step size per slider movement

- `orientation`:

  A Unicode string, either "horizontal" or "vertical"

- `readout`:

  A logical value, whether the value should be showns (read out)

- `readout_format`:

  A logical value, whether values should be updated as the slider is
  moved by the user

- `continuous_update`:

  A logical value, whether values should be updated as the slider is
  moved by the user

- `disabled`:

  A logical value, whether the slider is disabled

- `style`:

  A SliderStyle widget

- `behavior`:

  A string that describes the ddragging behavior.

## Methods

### Public methods

- [`IntRangeSlider$clone()`](#method-IntRangeSlider-clone)

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
- [`BoundedIntRangeWidget$initialize()`](https://melff.github.io/RKernel/reference/BoundedIntRangeWidget.html#method-initialize)
- [`BoundedIntRangeWidget$validate_max()`](https://melff.github.io/RKernel/reference/BoundedIntRangeWidget.html#method-validate_max)
- [`BoundedIntRangeWidget$validate_min()`](https://melff.github.io/RKernel/reference/BoundedIntRangeWidget.html#method-validate_min)
- [`BoundedIntRangeWidget$validate_value()`](https://melff.github.io/RKernel/reference/BoundedIntRangeWidget.html#method-validate_value)

------------------------------------------------------------------------

### `IntRangeSlider$clone()`

The objects of this class are cloneable with this method.

#### Usage

    IntRangeSlider$clone(deep = FALSE)

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
-\> `FloatSlider`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend

- `_view_name`:

  Name of the Javascript view in the frontend

- `step`:

  A [`Float`](https://melff.github.io/RKernel/reference/Float.md)
  traitlet, the minimal step size per slider movement

- `orientation`:

  A Unicode string, either "horizontal" or "vertical"

- `readout`:

  A logical value, whether the value should be showns (read out)

- `readout_format`:

  A logical value, whether values should be updated as the slider is
  moved by the user

- `continuous_update`:

  A logical value, whether values should be updated as the slider is
  moved by the user

- `disabled`:

  A logical value, whether the slider is disabled

- `style`:

  A SliderStyle widget

- `behavior`:

  A string that describes the ddragging behavior.

## Methods

### Public methods

- [`FloatSlider$clone()`](#method-FloatSlider-clone)

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

### `FloatSlider$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FloatSlider$clone(deep = FALSE)

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
[`BoundedFloatRangeWidget`](https://melff.github.io/RKernel/reference/BoundedFloatRangeWidget.md)
-\> `FloatRangeSlider`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend

- `_view_name`:

  Name of the Javascript view in the frontend

- `step`:

  A [`Float`](https://melff.github.io/RKernel/reference/Float.md)
  traitlet, the minimal step size per slider movement

- `orientation`:

  A Unicode string, either "horizontal" or "vertical"

- `readout`:

  A logical value, whether the value should be showns (read out)

- `readout_format`:

  A logical value, whether values should be updated as the slider is
  moved by the user

- `continuous_update`:

  A logical value, whether values should be updated as the slider is
  moved by the user

- `disabled`:

  A logical value, whether the slider is disabled

- `style`:

  A SliderStyle widget

- `behavior`:

  A string that describes the ddragging behavior.

## Methods

### Public methods

- [`FloatRangeSlider$clone()`](#method-FloatRangeSlider-clone)

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
- [`BoundedFloatRangeWidget$initialize()`](https://melff.github.io/RKernel/reference/BoundedFloatRangeWidget.html#method-initialize)
- [`BoundedFloatRangeWidget$validate_max()`](https://melff.github.io/RKernel/reference/BoundedFloatRangeWidget.html#method-validate_max)
- [`BoundedFloatRangeWidget$validate_min()`](https://melff.github.io/RKernel/reference/BoundedFloatRangeWidget.html#method-validate_min)
- [`BoundedFloatRangeWidget$validate_value()`](https://melff.github.io/RKernel/reference/BoundedFloatRangeWidget.html#method-validate_value)

------------------------------------------------------------------------

### `FloatRangeSlider$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FloatRangeSlider$clone(deep = FALSE)

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
[`BoundedLogFloatWidget`](https://melff.github.io/RKernel/reference/BoundedLogFloatWidget.md)
-\> `FloatLogSlider`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend

- `_view_name`:

  Name of the Javascript view in the frontend

- `step`:

  A [`Float`](https://melff.github.io/RKernel/reference/Float.md)
  traitlet, the minimal step size per slider movement

- `orientation`:

  A Unicode string, either "horizontal" or "vertical"

- `readout`:

  A logical value, whether the value should be showns (read out)

- `readout_format`:

  A logical value, whether values should be updated as the slider is
  moved by the user

- `continuous_update`:

  A logical value, whether values should be updated as the slider is
  moved by the user

- `disabled`:

  A [`Boolean`](https://melff.github.io/RKernel/reference/Boolean.md)
  traitlet, whether the slider is disabled

- `base`:

  A [`Float`](https://melff.github.io/RKernel/reference/Float.md)
  traitlet, the base of the logarithm

- `style`:

  A SliderStyle widget

- `behavior`:

  A string that describes the ddragging behavior.

## Methods

### Public methods

- [`FloatLogSlider$clone()`](#method-FloatLogSlider-clone)

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
- [`BoundedLogFloatWidget$initialize()`](https://melff.github.io/RKernel/reference/BoundedLogFloatWidget.html#method-initialize)
- [`BoundedLogFloatWidget$validate_max()`](https://melff.github.io/RKernel/reference/BoundedLogFloatWidget.html#method-validate_max)
- [`BoundedLogFloatWidget$validate_min()`](https://melff.github.io/RKernel/reference/BoundedLogFloatWidget.html#method-validate_min)
- [`BoundedLogFloatWidget$validate_value()`](https://melff.github.io/RKernel/reference/BoundedLogFloatWidget.html#method-validate_value)

------------------------------------------------------------------------

### `FloatLogSlider$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FloatLogSlider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
