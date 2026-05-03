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

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
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

- [`OutputWidget$new()`](#method-OutputWidget-initialize)

- [`OutputWidget$display()`](#method-OutputWidget-display)

- [`OutputWidget$clear()`](#method-OutputWidget-clear)

- [`OutputWidget$stdout()`](#method-OutputWidget-stdout)

- [`OutputWidget$stderr()`](#method-OutputWidget-stderr)

- [`OutputWidget$handle_msg()`](#method-OutputWidget-handle_msg)

- [`OutputWidget$clone()`](#method-OutputWidget-clone)

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

### `OutputWidget$new()`

Initializing function

#### Usage

    OutputWidget$new(append_output = TRUE, graphics = FALSE, ...)

#### Arguments

- `append_output`:

  Logical, whether existing output should be appended to or overwritten.

- `graphics`:

  Logical, whether graphics should be captured and processed within the
  widget

- `...`:

  Any other arguments, passed to the superclass initializer.

------------------------------------------------------------------------

### `OutputWidget$display()`

A variant of
[`display`](https://melff.github.io/RKernel/reference/display.md) for
output within a display widget.

#### Usage

    OutputWidget$display(...)

#### Arguments

- `...`:

  Arguments passed to the function
  [`display_data`](https://melff.github.io/RKernel/reference/display_data.md).

------------------------------------------------------------------------

### `OutputWidget$clear()`

Clear the output

#### Usage

    OutputWidget$clear(wait = FALSE)

#### Arguments

- `wait`:

  Logical, whether to wait for the frontend to clear the output.

------------------------------------------------------------------------

### `OutputWidget$stdout()`

Handle output via stdout stream

#### Usage

    OutputWidget$stdout(text)

#### Arguments

- `text`:

  A character string being output

------------------------------------------------------------------------

### `OutputWidget$stderr()`

Handle output via stderr stream

#### Usage

    OutputWidget$stderr(text)

#### Arguments

- `text`:

  A character string being output

------------------------------------------------------------------------

### `OutputWidget$handle_msg()`

Handle a (JSON) message sent to the output

#### Usage

    OutputWidget$handle_msg(msg)

#### Arguments

- `msg`:

  The message, a list

------------------------------------------------------------------------

### `OutputWidget$clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputWidget$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
