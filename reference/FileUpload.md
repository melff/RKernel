# File Upload Widgets

Class and constructor for file upload widgets

## Usage

``` r
FileUpload(...)
```

## Arguments

- ...:

  Any arguments used to initialize the fields of the object

## Functions

- `FileUpload()`: The FileUpload constructor function

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `ValueWidget` -\> `FileUpload`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend.

- `_view_name`:

  Name of the Javascript view in the frontend.

- `accept`:

  A character string that defineds the accepted file type. If empty, all
  files are accepted.

- `multiple`:

  A Boolean traitlet, whether multiple files are accepted.

- `disabled`:

  A [Boolean](https://melff.github.io/RKernel/reference/Boolean.md)
  traitlet, whether the widget is disabled.

- `icon`:

  A character string, the font-awesome without the 'fa-' prefix.

- `button_style`:

  The string that describes the button style

- `style`:

  The button style, an object of class "ButtonStyleClass".

- `error`:

  A string with an error message, if applicable.

- `value`:

  The uploaded data.

## Active bindings

- `filename`:

  A character string, the name of the uploaded file.

- `contents`:

  A raw vector with the contents of the uploaded file

## Methods

### Public methods

- [`FileUpload$new()`](#method-FileUpload-initialize)

- [`FileUpload$handle_buffers()`](#method-FileUpload-handle_buffers)

- [`FileUpload$clone()`](#method-FileUpload-clone)

Inherited methods

- [`HasTraits$notify()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-notify)
- [`HasTraits$observe()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-observe)
- [`HasTraits$validate()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-validate)
- [`Widget$_send()`](https://melff.github.io/RKernel/reference/Widget.html#method-_send)
- [`Widget$check_version()`](https://melff.github.io/RKernel/reference/Widget.html#method-check_version)
- [`Widget$close()`](https://melff.github.io/RKernel/reference/Widget.html#method-close)
- [`Widget$display_data()`](https://melff.github.io/RKernel/reference/Widget.html#method-display_data)
- [`Widget$get_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-get_state)
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

### `FileUpload$new()`

A generic initializer function

#### Usage

    FileUpload$new(description = "Upload", ...)

#### Arguments

- `description`:

  The button description

- `...`:

  Any arguments used to initialize the fields of the object

------------------------------------------------------------------------

### `FileUpload$handle_buffers()`

Handle buffers in message

#### Usage

    FileUpload$handle_buffers(msg)

#### Arguments

- `msg`:

  A comm message

------------------------------------------------------------------------

### `FileUpload$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FileUpload$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
