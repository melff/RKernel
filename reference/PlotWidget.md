# Widgets to receive plots graphics

Class and constructors show graphics created by code

## Usage

``` r
SVGWidget(...)

# S3 method for class 'SVGWidget'
with(data, expr, envir = list(), enclos = parent.frame(), ...)

PlotWidget(...)
```

## Arguments

- ...:

  Arguments, passed to the ImageWidget constructors.

- data:

  An "SVGWidget" object

- expr:

  An expression to evaluate, or a sequence of expression, encapsulated
  by curly braces.

- envir:

  An environment or a list within which \`expr\` is evaluated.

- enclos:

  An enclosing environment.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `ValueWidget` -\>
[`StringWidget`](https://melff.github.io/RKernel/reference/StringWidget.md)
-\> [`HTML`](https://melff.github.io/RKernel/reference/StringWidget.md)
-\> `SVGWidget`

## Methods

### Public methods

- [`SVGWidget$new()`](#method-SVGWidget-initialize)

- [`SVGWidget$activate()`](#method-SVGWidget-activate)

- [`SVGWidget$suspend()`](#method-SVGWidget-suspend)

- [`SVGWidget$render()`](#method-SVGWidget-render)

- [`SVGWidget$clone()`](#method-SVGWidget-clone)

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

### `SVGWidget$new()`

Initialize the object

#### Usage

    SVGWidget$new(..., width = NULL, height = NULL)

#### Arguments

- `...`:

  Arguments passed to the superclass initializer

- `width`:

  A character string, giving the width as a CSS property

- `height`:

  A character string, giving the height as a CSS property

------------------------------------------------------------------------

### `SVGWidget$activate()`

Activate widget as graphics device

#### Usage

    SVGWidget$activate()

------------------------------------------------------------------------

### `SVGWidget$suspend()`

Suspend widget as graphics device

#### Usage

    SVGWidget$suspend()

------------------------------------------------------------------------

### `SVGWidget$render()`

Render contents of graphics device as SVG

#### Usage

    SVGWidget$render()

------------------------------------------------------------------------

### `SVGWidget$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SVGWidget$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
