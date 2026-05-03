# A Base Class for DOM Widgets

This is a base class for all widgets that are supposed to be part of the
document object model

## Usage

``` r
DOMWidget(...)
```

## Arguments

- ...:

  Arguments passed to the inializer

## Functions

- `DOMWidget()`: The DOM widget constructor function

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
`DOMWidget`

## Public fields

- `_model_module`:

  Name of the Javascript module with the model

- `_model_module_version`:

  Version of the module where the model is defined

- `_model_name`:

  Name of the Javascript model in the frontend

- `_dom_classes`:

  A set of character strings that indicate the DOM classes the widget is
  assigned to

- `layout`:

  The layout, a "LayoutClass" Widget

## Methods

### Public methods

- [`DOMWidget$add_class()`](#method-DOMWidget-add_class)

- [`DOMWidget$remove_class()`](#method-DOMWidget-remove_class)

- [`DOMWidget$has_class()`](#method-DOMWidget-has_class)

- [`DOMWidget$clone()`](#method-DOMWidget-clone)

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

### `DOMWidget$add_class()`

Add a class attribute to the DOM element

#### Usage

    DOMWidget$add_class(className)

#### Arguments

- `className`:

  Name of the class attribute

------------------------------------------------------------------------

### `DOMWidget$remove_class()`

Remove a class attribute to the DOM element

#### Usage

    DOMWidget$remove_class(className)

#### Arguments

- `className`:

  Name of the class attribute

------------------------------------------------------------------------

### `DOMWidget$has_class()`

Check whether the DOM element has a class attribute

#### Usage

    DOMWidget$has_class(className)

#### Arguments

- `className`:

  Name of the class attribute

------------------------------------------------------------------------

### `DOMWidget$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DOMWidget$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
