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

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\>
[`RKernel::Widget`](https://melff.github.io/RKernel/reference/Widgets.md)
-\> `DOMWidget`

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

- [`DOMWidgetClass$add_class()`](#method-DOMWidget-add_class)

- [`DOMWidgetClass$remove_class()`](#method-DOMWidget-remove_class)

- [`DOMWidgetClass$has_class()`](#method-DOMWidget-has_class)

- [`DOMWidgetClass$clone()`](#method-DOMWidget-clone)

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
- [`RKernel::Widget$initialize()`](https://melff.github.io/RKernel/reference/Widget.html#method-initialize)
- [`RKernel::Widget$on_displayed()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_displayed)
- [`RKernel::Widget$on_event()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_event)
- [`RKernel::Widget$on_msg()`](https://melff.github.io/RKernel/reference/Widget.html#method-on_msg)
- [`RKernel::Widget$open()`](https://melff.github.io/RKernel/reference/Widget.html#method-open)
- [`RKernel::Widget$send()`](https://melff.github.io/RKernel/reference/Widget.html#method-send)
- [`RKernel::Widget$send_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-send_state)
- [`RKernel::Widget$set_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-set_state)

------------------------------------------------------------------------

### Method `add_class()`

Add a class attribute to the DOM element

#### Usage

    DOMWidgetClass$add_class(className)

#### Arguments

- `className`:

  Name of the class attribute

------------------------------------------------------------------------

### Method `remove_class()`

Remove a class attribute to the DOM element

#### Usage

    DOMWidgetClass$remove_class(className)

#### Arguments

- `className`:

  Name of the class attribute

------------------------------------------------------------------------

### Method `has_class()`

Check whether the DOM element has a class attribute

#### Usage

    DOMWidgetClass$has_class(className)

#### Arguments

- `className`:

  Name of the class attribute

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DOMWidgetClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
