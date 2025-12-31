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

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\>
[`RKernel::Widget`](https://melff.github.io/RKernel/reference/Widgets.md)
-\>
[`RKernel::DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`RKernel::OutputWidget`](https://melff.github.io/RKernel/reference/OutputWidget.md)
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

- [`SidecarClass$clone()`](#method-Sidecar-clone)

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
- [`RKernel::OutputWidget$clear()`](https://melff.github.io/RKernel/reference/OutputWidget.html#method-clear)
- [`RKernel::OutputWidget$display()`](https://melff.github.io/RKernel/reference/OutputWidget.html#method-display)
- [`RKernel::OutputWidget$handle_msg()`](https://melff.github.io/RKernel/reference/OutputWidget.html#method-handle_msg)
- [`RKernel::OutputWidget$initialize()`](https://melff.github.io/RKernel/reference/OutputWidget.html#method-initialize)
- [`RKernel::OutputWidget$stderr()`](https://melff.github.io/RKernel/reference/OutputWidget.html#method-stderr)
- [`RKernel::OutputWidget$stdout()`](https://melff.github.io/RKernel/reference/OutputWidget.html#method-stdout)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SidecarClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
