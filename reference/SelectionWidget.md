# Selection widgets

Classes and constructors for selection widgets, i.e. dropdowns,
listboxes etc.

## Usage

``` r
SelectionWidget(options, value, ...)

Dropdown(options, value, ...)

RadioButtons(options, value, ...)

ListBox(options, value, ...)

ToggleButtons(options, value, ...)

SelectionSlider(options, value, ...)

MultipleSelectionWidget(options, value, ...)

ListBoxMultiple(options, value, ...)

SelectionRangeSlider(options, value, ...)
```

## Arguments

- options:

  A named vector or a vector coerceable into a character vector.

- value:

  A trait.

- ...:

  Any other arguments, ignored.

## Functions

- `SelectionWidget()`: A constructor function for selection widgets.

- `Dropdown()`: The construction function for dropdown widgets.

- `RadioButtons()`: The construction function for radiobuttons widgets.

- `ListBox()`: The construction function for listbox-selection widgets.

- `ToggleButtons()`: The construction function for togglebuttons
  widgets.

- `SelectionSlider()`: The construction function for listbox widgets
  with multiple selections.

- `MultipleSelectionWidget()`: The construction function for
  multiple-selection widgets.

- `ListBoxMultiple()`: The construction function for listbox widgets
  with multiple selections.

- `SelectionRangeSlider()`: The construction function for listbox
  widgets with multiple selections.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `SelectionWidget`

## Public fields

- `_options_labels`:

  A unicode vector of option labels.

- `index`:

  An integer that refers to currently selected item.

## Active bindings

- `value`:

  The selected option

## Methods

### Public methods

- [`SelectionWidget$validate_index()`](#method-SelectionWidget-validate_index)

- [`SelectionWidget$new()`](#method-SelectionWidget-initialize)

- [`SelectionWidget$clone()`](#method-SelectionWidget-clone)

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

------------------------------------------------------------------------

### `SelectionWidget$validate_index()`

Validate an index argument.

#### Usage

    SelectionWidget$validate_index(index)

#### Arguments

- `index`:

  The index to be checked.

------------------------------------------------------------------------

### `SelectionWidget$new()`

Initialiser

#### Usage

    SelectionWidget$new(options, value, ...)

#### Arguments

- `options`:

  A named vector or a vector coerceable into a character vector.

- `value`:

  Name of a selectable option.

- `...`:

  Any other arguments, ignored.

------------------------------------------------------------------------

### `SelectionWidget$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SelectionWidget$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `SelectionWidget` -\> `Dropdown`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend.

- `_view_name`:

  Name of the Javascript view in the frontend.

## Methods

### Public methods

- [`Dropdown$clone()`](#method-Dropdown-clone)

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
- [`SelectionWidget$initialize()`](https://melff.github.io/RKernel/reference/SelectionWidget.html#method-initialize)
- [`SelectionWidget$validate_index()`](https://melff.github.io/RKernel/reference/SelectionWidget.html#method-validate_index)

------------------------------------------------------------------------

### `Dropdown$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Dropdown$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `SelectionWidget` -\> `RadioButtons`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend.

- `_view_name`:

  Name of the Javascript view in the frontend.

## Methods

### Public methods

- [`RadioButtons$clone()`](#method-RadioButtons-clone)

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
- [`SelectionWidget$initialize()`](https://melff.github.io/RKernel/reference/SelectionWidget.html#method-initialize)
- [`SelectionWidget$validate_index()`](https://melff.github.io/RKernel/reference/SelectionWidget.html#method-validate_index)

------------------------------------------------------------------------

### `RadioButtons$clone()`

The objects of this class are cloneable with this method.

#### Usage

    RadioButtons$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `SelectionWidget` -\> `ListboxSelect`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend.

- `_view_name`:

  Name of the Javascript view in the frontend.

- `rows`:

  An integer, the number of rows.

## Methods

### Public methods

- [`ListboxSelect$clone()`](#method-ListboxSelect-clone)

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
- [`SelectionWidget$initialize()`](https://melff.github.io/RKernel/reference/SelectionWidget.html#method-initialize)
- [`SelectionWidget$validate_index()`](https://melff.github.io/RKernel/reference/SelectionWidget.html#method-validate_index)

------------------------------------------------------------------------

### `ListboxSelect$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ListboxSelect$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DescriptionStyle`](https://melff.github.io/RKernel/reference/DescriptionStyle.md)
-\> `ToggleButtonsStyle`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend.

- `button_width`:

  A unicode string, the width in CSS language

- `font_weight`:

  A unicode string, the font weight in CSS language

## Methods

### Public methods

- [`ToggleButtonsStyle$clone()`](#method-ToggleButtonsStyle-clone)

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

### `ToggleButtonsStyle$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ToggleButtonsStyle$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `SelectionWidget` -\> `ToggleButtons`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend.

- `_view_name`:

  Name of the Javascript view in the frontend.

- `tooltips`:

  A unicode vector with tooltips.

- `icons`:

  A unicode vector with icon specs.

- `style`:

  A TobbleButtonStyle widget

- `button_style`:

  A character string, one of "primary", "success", "info", "warning",
  "danger", or the empty string.

## Methods

### Public methods

- [`ToggleButtons$clone()`](#method-ToggleButtons-clone)

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
- [`SelectionWidget$initialize()`](https://melff.github.io/RKernel/reference/SelectionWidget.html#method-initialize)
- [`SelectionWidget$validate_index()`](https://melff.github.io/RKernel/reference/SelectionWidget.html#method-validate_index)

------------------------------------------------------------------------

### `ToggleButtons$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ToggleButtons$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `SelectionWidget` -\> `SelectionSlider`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend.

- `_view_name`:

  Name of the Javascript view in the frontend.

- `orientation`:

  A Unicode string, either "horizontal" or "vertical"

- `readout`:

  A logical value, whether the value should be showns (read out)

- `continuous_update`:

  A logical value, whether values should be updated as the slider is
  moved by the user

- `style`:

  A SliderStyle widget

- `behavior`:

  A string that describes the ddragging behavior.

## Methods

### Public methods

- [`SelectionSlider$clone()`](#method-SelectionSlider-clone)

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
- [`SelectionWidget$initialize()`](https://melff.github.io/RKernel/reference/SelectionWidget.html#method-initialize)
- [`SelectionWidget$validate_index()`](https://melff.github.io/RKernel/reference/SelectionWidget.html#method-validate_index)

------------------------------------------------------------------------

### `SelectionSlider$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SelectionSlider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `MultipleSelectionWidget`

## Public fields

- `_options_labels`:

  A unicode string vector with labels

- `index`:

  An integer vector of indices of currenlty selected elements.

## Active bindings

- `value`:

  The selected option

## Methods

### Public methods

- [`MultipleSelectionWidget$validate_index()`](#method-MultipleSelectionWidget-validate_index)

- [`MultipleSelectionWidget$new()`](#method-MultipleSelectionWidget-initialize)

- [`MultipleSelectionWidget$clone()`](#method-MultipleSelectionWidget-clone)

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

------------------------------------------------------------------------

### `MultipleSelectionWidget$validate_index()`

Validate an index.

#### Usage

    MultipleSelectionWidget$validate_index(index)

#### Arguments

- `index`:

  An index, the index to be checked.

------------------------------------------------------------------------

### `MultipleSelectionWidget$new()`

Initialiser

#### Usage

    MultipleSelectionWidget$new(options, value, ...)

#### Arguments

- `options`:

  A named vector or a vector coerceable into a character vector.

- `value`:

  Names of selectable options.

- `...`:

  Any other arguments, ignored.

------------------------------------------------------------------------

### `MultipleSelectionWidget$clone()`

The objects of this class are cloneable with this method.

#### Usage

    MultipleSelectionWidget$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `MultipleSelectionWidget` -\> `ListboxSelectMultiple`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend.

- `_view_name`:

  Name of the Javascript view in the frontend.

- `rows`:

  An integer, the number of rows.

## Methods

### Public methods

- [`ListboxSelectMultiple$clone()`](#method-ListboxSelectMultiple-clone)

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
- [`MultipleSelectionWidget$initialize()`](https://melff.github.io/RKernel/reference/MultipleSelectionWidget.html#method-initialize)
- [`MultipleSelectionWidget$validate_index()`](https://melff.github.io/RKernel/reference/MultipleSelectionWidget.html#method-validate_index)

------------------------------------------------------------------------

### `ListboxSelectMultiple$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ListboxSelectMultiple$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `MultipleSelectionWidget` -\> `SelectionRangeSlider`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend.

- `_view_name`:

  Name of the Javascript view in the frontend.

- `orientation`:

  A Unicode string, either "horizontal" or "vertical"

- `readout`:

  A logical value, whether the value should be showns (read out)

- `continuous_update`:

  A logical value, whether values should be updated as the slider is
  moved by the user

- `style`:

  A SliderStyle widget

- `behavior`:

  A string that describes the ddragging behavior.

## Methods

### Public methods

- [`SelectionRangeSlider$validate_index()`](#method-SelectionRangeSlider-validate_index)

- [`SelectionRangeSlider$new()`](#method-SelectionRangeSlider-initialize)

- [`SelectionRangeSlider$clone()`](#method-SelectionRangeSlider-clone)

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

------------------------------------------------------------------------

### `SelectionRangeSlider$validate_index()`

Validate an index.

#### Usage

    SelectionRangeSlider$validate_index(index)

#### Arguments

- `index`:

  An index, the index to be checked.

------------------------------------------------------------------------

### `SelectionRangeSlider$new()`

Initialiser

#### Usage

    SelectionRangeSlider$new(options, value, ...)

#### Arguments

- `options`:

  A named vector or a vector coerceable into a character vector.

- `value`:

  Names of selectable options.

- `...`:

  Any other arguments, ignored.

------------------------------------------------------------------------

### `SelectionRangeSlider$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SelectionRangeSlider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
