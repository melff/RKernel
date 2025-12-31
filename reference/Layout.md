# Widget Layout Manipulation

An R6 class and a constructor function for the creation of a layout
widget, which itself is used to manipulate the layout of a
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md).

## Usage

``` r
Layout(...)
```

## Arguments

- ...:

  Arguments passed to the inializer

## Details

The function `Layout` creates objects of the R6 Class "LayoutClass",
which in turn have the S3 class attribute "Layout"

## Functions

- `Layout()`: The Layout constructor function

## Super classes

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\>
[`RKernel::Widget`](https://melff.github.io/RKernel/reference/Widgets.md)
-\> `Layout`

## Public fields

- `_view_name`:

  Name of the Javascript view in the frontend.

- `_view_module`:

  Name of the Javascript view module in the frontend.

- `_view_module_version`:

  Version of the Javascript view module in the frontend.

- `_model_name`:

  Name of the Javascript model in the frontend.

- `align_content`:

  An optional string, if non-empty, one of "flex-start", "flex-end",
  "center", "space-between", "space-around", "space-evenly", "stretch"

- `align_items`:

  An optional string, if non-empty, one of "flex-start", "flex-end",
  "center", "baseline", "stretch"

- `align_self`:

  An optional string, if non-empty, one of "flex-start", "flex-end",
  "center", "baseline", "stretch"

- `bottom`:

  Position from bottom, an optional string that should, if non-empty,
  contain a valid CSS dimension

- `border`:

  An optional string with a valid CSS border specification

- `border_top`:

  An optional string with a valid CSS border specification

- `border_right`:

  An optional string with a valid CSS border specification

- `border_bottom`:

  An optional string with a valid CSS border specification

- `border_left`:

  An optional string with a valid CSS border specification

- `display`:

  An optional string with a valid CSS display property

- `flex`:

  An optional string with a valid CSS flex property

- `flex_flow`:

  An optional string with a valid CSS flex_flow property

- `height`:

  An optional string with a valid CSS height

- `justify_content`:

  An optional string, if non-empty, one of "flex-start", "flex-end",
  "center", "space-between", "space-around".

- `justify_items`:

  An optional string, if non-empty, one of "flex-start", "flex-end", or
  "center"

- `left`:

  Position from left, an optional string that should, if non-empty,
  contain a valid CSS dimension

- `margin`:

  An optional string, if non-empty, should be a valid CSS margin
  specification

- `max_height`:

  An optional string, if non-emtpy, should be a valid CSS dimension

- `max_width`:

  An optional string, if non-emtpy, should be a valid CSS dimension

- `min_height`:

  An optional string, if non-emtpy, should be a valid CSS dimension

- `min_width`:

  An optional string, if non-emtpy, should be a valid CSS dimension

- `overflow`:

  An optonal string, if non-empty, should be a valid CSS overflow
  specification

- `order`:

  An optional string, if non-empty should contain a number

- `padding`:

  An optional string, if non-emtpy should be a valid CSS dimension

- `right`:

  Position from right, an optional string, if non-empty, should be a
  valid CSS dimension

- `top`:

  Position from top, an optional string, if non-empty, should be a valid
  CSS dimension

- `visibility`:

  An optional string, if non-empty, should be either "visible" or
  "hidden"

- `width`:

  An optional string, if non-empty, should be a valid CSS dimension

- `object_fit`:

  An optional string, if non-empty, should be one of "contain", "cover",
  "fill", "scale-down", "none"

- `object_position`:

  An optional string, if non-empty, should be a valid CSS
  object-position specification

- `grid_auto_columns`:

  An optional string, if non-empty should be valid CSS code for the
  grid-auto-columns property

- `grid_auto_flow`:

  An optional string, if non-empty should be valid CSS code for the
  grid-auto-flow property

- `grid_auto_rows`:

  An optional string, if non-empty should be valid CSS code for the
  grid-auto-rows property

- `grid_gap`:

  An optional string, if non-empty should be valid CSS code for the
  grid-gap property

- `grid_template_rows`:

  An optional string, if non-empty should be valid CSS code for the
  grid-template-rows property

- `grid_template_columns`:

  An optional string, if non-empty should be valid CSS code for the
  grid-template-columns property

- `grid_template_areas`:

  An optional string, if non-empty should be valid CSS code for the
  grid-template-areas property

- `grid_row`:

  An optional string, if non-empty should be valid CSS code for the
  grid-row property

- `grid_column`:

  An optional string, if non-empty should be valid CSS code for the
  grid-column property

- `grid_area`:

  An optional string, if non-empty should be valid CSS code for the
  grid-area property

## Methods

### Public methods

- [`LayoutClass$observe_border()`](#method-Layout-observe_border)

- [`LayoutClass$new()`](#method-Layout-new)

- [`LayoutClass$clone()`](#method-Layout-clone)

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

------------------------------------------------------------------------

### Method `observe_border()`

Synchronize border traits

#### Usage

    LayoutClass$observe_border(nm, self, value)

#### Arguments

- `nm`:

  Name of the trait (a dummy argument)

- `self`:

  The object

- `value`:

  A CSS string

------------------------------------------------------------------------

### Method `new()`

Initialize an object

#### Usage

    LayoutClass$new(...)

#### Arguments

- `...`:

  Arguments passed to the superclass initializer

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LayoutClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
