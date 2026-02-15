# Widgets to receive output

Classes and constructors to wrap output created by code

## Usage

``` r
OutputWidget(append_output = FALSE, ...)

# S3 method for class 'OutputWidget'
with(data, expr, envir = list(), enclos = parent.frame(), clear = TRUE, ...)
```

## Arguments

- append_output:

  Logical value, whether new output is appended to existing output in
  the widget or the output is overwritten

- ...:

  Other arguments, ignored.

- data:

  An "OutputWidget" object

- expr:

  An expression to evaluate, or a sequence of expression, encapsulated
  by curly braces.

- envir:

  An environment or a list within which \`expr\` is evaluated.

- enclos:

  An enclosing environment.

- clear:

  A logical value, whether clear the output before evaluating \`expr\`

## Super classes

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\>
[`RKernel::Widget`](https://melff.github.io/RKernel/reference/Widgets.md)
-\>
[`RKernel::DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\> `OutputWidget`

## Public fields

- `_view_name`:

  Name of the Javascript model view in the frontend

- `_model_name`:

  Name of the Javascript model in the frontend

- `_view_module`:

  Name of the module where the view is defined

- `_model_module`:

  Name of the Javascript module with the model

- `_view_module_version`:

  Version of the module where the view is defined

- `_model_module_version`:

  Version of the module where the model is defined

- `msg_id`:

  Unicode string with the id of the last message sent to the frontend.

- `outputs`:

  A list with output strings

## Methods

### Public methods

- [`OutputWidgetClass$new()`](#method-OutputWidget-new)

- [`OutputWidgetClass$display()`](#method-OutputWidget-display)

- [`OutputWidgetClass$clear()`](#method-OutputWidget-clear)

- [`OutputWidgetClass$stdout()`](#method-OutputWidget-stdout)

- [`OutputWidgetClass$stderr()`](#method-OutputWidget-stderr)

- [`OutputWidgetClass$handle_msg()`](#method-OutputWidget-handle_msg)

- [`OutputWidgetClass$clone()`](#method-OutputWidget-clone)

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

------------------------------------------------------------------------

### Method `new()`

Initializing function

#### Usage

    OutputWidgetClass$new(append_output = TRUE, graphics = FALSE, ...)

#### Arguments

- `append_output`:

  Logical, whether existing output should be appended to or overwritten.

- `graphics`:

  Logical, whether graphics should be captured and processed within the
  widget

- `...`:

  Any other arguments, passed to the superclass initializer.

------------------------------------------------------------------------

### Method [`display()`](https://melff.github.io/RKernel/reference/display.md)

A variant of
[`display`](https://melff.github.io/RKernel/reference/display.md) for
output within a display widget.

#### Usage

    OutputWidgetClass$display(...)

#### Arguments

- `...`:

  Arguments passed to the function
  [`display_data`](https://melff.github.io/RKernel/reference/display_data.md).

------------------------------------------------------------------------

### Method `clear()`

Clear the output

#### Usage

    OutputWidgetClass$clear(wait = FALSE)

#### Arguments

- `wait`:

  Logical, whether to wait for the frontend to clear the output.

------------------------------------------------------------------------

### Method [`stdout()`](https://rdrr.io/r/base/showConnections.html)

Handle output via stdout stream

#### Usage

    OutputWidgetClass$stdout(text)

#### Arguments

- `text`:

  A character string being output

------------------------------------------------------------------------

### Method [`stderr()`](https://rdrr.io/r/base/showConnections.html)

Handle output via stderr stream

#### Usage

    OutputWidgetClass$stderr(text)

#### Arguments

- `text`:

  A character string being output

------------------------------------------------------------------------

### Method `handle_msg()`

Handle a (JSON) message sent to the output

#### Usage

    OutputWidgetClass$handle_msg(msg)

#### Arguments

- `msg`:

  The message, a list

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputWidgetClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
