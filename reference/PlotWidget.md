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

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\>
[`RKernel::Widget`](https://melff.github.io/RKernel/reference/Widgets.md)
-\>
[`RKernel::DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`RKernel::DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `RKernel::ValueWidget` -\>
[`RKernel::StringWidget`](https://melff.github.io/RKernel/reference/StringWidget.md)
-\>
[`RKernel::HTML`](https://melff.github.io/RKernel/reference/StringWidget.md)
-\> `SVGWidget`

## Public fields

- `context`:

  A Context instance or NULL

## Methods

### Public methods

- [`SVGWidgetClass$new()`](#method-SVGWidget-new)

- [`SVGWidgetClass$activate()`](#method-SVGWidget-activate)

- [`SVGWidgetClass$suspend()`](#method-SVGWidget-suspend)

- [`SVGWidgetClass$render()`](#method-SVGWidget-render)

- [`SVGWidgetClass$clone()`](#method-SVGWidget-clone)

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

### Method `new()`

Initialize the object

#### Usage

    SVGWidgetClass$new(..., width = NULL, height = NULL)

#### Arguments

- `...`:

  Arguments passed to the superclass initializer

- `width`:

  A character string, giving the width as a CSS property

- `height`:

  A character string, giving the height as a CSS property

------------------------------------------------------------------------

### Method `activate()`

Activate widget as graphics device

#### Usage

    SVGWidgetClass$activate()

------------------------------------------------------------------------

### Method `suspend()`

Suspend widget as graphics device

#### Usage

    SVGWidgetClass$suspend()

------------------------------------------------------------------------

### Method `render()`

Render contents of graphics device as SVG

#### Usage

    SVGWidgetClass$render()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SVGWidgetClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
