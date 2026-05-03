# Sidecar widgets

Sidecar widgets - works only with Jupyter Lab

## Usage

``` r
Sidecar(...)
```

## Arguments

- ...:

  Arguments passed to the inializer

## Details

Note that these widget need the sidecar extension for Jupyter Lab. See
<https://github.com/jupyter-widgets/jupyterlab-sidecar>

## Functions

- `Sidecar()`: A constructor for sidebar widgets

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`OutputWidget`](https://melff.github.io/RKernel/reference/OutputWidget.md)
-\> `Sidecar`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend

- `_model_module`:

  Name of the Javascript frontend module

- `_model_module_version`:

  Version of the Javascript frontend module

- `_view_name`:

  Name of the Javascript view in the frontend

- `_view_module`:

  Name of the Javascript frontend view module

- `_view_module_version`:

  Version of the the Javascript view module

- `title`:

  A unicode string, the title of the widget.

- `anchor`:

  A string that specifies where the widget s to appear: one of
  "split-right", "split-left", "split-top", "split-bottom",
  "tab-before", "tab-after", or "right".

## Methods

### Public methods

- [`Sidecar$clone()`](#method-Sidecar-clone)

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
- [`OutputWidget$clear()`](https://melff.github.io/RKernel/reference/OutputWidget.html#method-clear)
- [`OutputWidget$display()`](https://melff.github.io/RKernel/reference/OutputWidget.html#method-display)
- [`OutputWidget$handle_msg()`](https://melff.github.io/RKernel/reference/OutputWidget.html#method-handle_msg)
- [`OutputWidget$initialize()`](https://melff.github.io/RKernel/reference/OutputWidget.html#method-initialize)
- [`OutputWidget$stderr()`](https://melff.github.io/RKernel/reference/OutputWidget.html#method-stderr)
- [`OutputWidget$stdout()`](https://melff.github.io/RKernel/reference/OutputWidget.html#method-stdout)

------------------------------------------------------------------------

### `Sidecar$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Sidecar$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
