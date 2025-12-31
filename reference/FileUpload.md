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

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\>
[`RKernel::Widget`](https://melff.github.io/RKernel/reference/Widgets.md)
-\>
[`RKernel::DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`RKernel::DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `RKernel::ValueWidget` -\> `FileUpload`

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

- [`FileUploadClass$new()`](#method-FileUpload-new)

- [`FileUploadClass$handle_buffers()`](#method-FileUpload-handle_buffers)

- [`FileUploadClass$clone()`](#method-FileUpload-clone)

Inherited methods

- [`RKernel::HasTraits$notify()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-notify)
- [`RKernel::HasTraits$observe()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-observe)
- [`RKernel::HasTraits$validate()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-validate)
- [`RKernel::Widget$_send()`](https://melff.github.io/RKernel/reference/Widget.html#method-_send)
- [`RKernel::Widget$check_version()`](https://melff.github.io/RKernel/reference/Widget.html#method-check_version)
- [`RKernel::Widget$close()`](https://melff.github.io/RKernel/reference/Widget.html#method-close)
- [`RKernel::Widget$display_data()`](https://melff.github.io/RKernel/reference/Widget.html#method-display_data)
- [`RKernel::Widget$get_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-get_state)
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

A generic initializer function

#### Usage

    FileUploadClass$new(description = "Upload", ...)

#### Arguments

- `description`:

  The button description

- `...`:

  Any arguments used to initialize the fields of the object

------------------------------------------------------------------------

### Method `handle_buffers()`

Handle buffers in message

#### Usage

    FileUploadClass$handle_buffers(msg)

#### Arguments

- `msg`:

  A comm message

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FileUploadClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
