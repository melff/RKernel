# Synchronize two Widgets Using a Link

An R6 class and a constructor function for the creation of a link
widget, which links two widgets so that their values are synchronized

## Usage

``` r
WidgetLink(source, target, ...)

DirectionalLink(source, target, ...)
```

## Arguments

- source:

  A link with two elements, the first is a widget, the second is one of
  its traits.

- target:

  A link with two elements, the first is a widget, the second is one of
  its traits.

- ...:

  Other arguments passed to the inializer

## Details

The function `WidgetLink` creates objects of the R6 Class
"WidgetLinkClass", which in turn have the S3 class attribute
"WidgetLink"

## Functions

- `WidgetLink()`: The WidgetLink constructor function

- `DirectionalLink()`: The WidgetLink constructor function

## Super classes

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\>
[`RKernel::Widget`](https://melff.github.io/RKernel/reference/Widgets.md)
-\> `RKernel::CoreWidget` -\> `WidgetLink`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend.

- `source`:

  A pair of Unicode strings, the first is the JSON representation of a
  widget, the second is the name of a trait(let) of the widget.

- `target`:

  A pair of Unicode strings, the first is the JSON representation of a
  widget, the second is the name of a trait(let) of the widget.

## Methods

### Public methods

- [`WidgetLinkClass$new()`](#method-WidgetLink-new)

- [`WidgetLinkClass$clone()`](#method-WidgetLink-clone)

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

### Method `new()`

An initializer method

#### Usage

    WidgetLinkClass$new(source, target, ...)

#### Arguments

- `source`:

  A list with two elements, a widget and the name of a trait(let).

- `target`:

  A list with two elements, a widget and the name of a trait(let).

- `...`:

  Futher arguments, passed to the superclass initializer.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    WidgetLinkClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\>
[`RKernel::Widget`](https://melff.github.io/RKernel/reference/Widgets.md)
-\> `RKernel::CoreWidget` -\> `RKernel::WidgetLink` -\>
`DirectionalLink`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend.

## Methods

### Public methods

- [`DirectionalLinkClass$clone()`](#method-DirectionalLink-clone)

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
- [`RKernel::WidgetLink$initialize()`](https://melff.github.io/RKernel/reference/WidgetLink.html#method-initialize)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DirectionalLinkClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
