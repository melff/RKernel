# A Widget Base Class

The base class from which all widget classes are derived

## Usage

``` r
Widget(...)
```

## Arguments

- ...:

  Arguments passed to the inializer

## Functions

- `Widget()`: A Widget Constructor Function

## Super class

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> `Widget`

## Public fields

- `_model_id`:

  Identifyer of the frontend Javascript object

- `_model_name`:

  Name of the Javascript model in the frontend

- `_model_module`:

  Name of the Javascript module with the model

- `_model_module_version`:

  Version of the module where the model is defined

- `_view_name`:

  Name of the Javascript model view in the frontend

- `_view_module`:

  Version of the module where the view is defined

- `_view_module_version`:

  Version of the module where the view is defined

- `_view_count`:

  Number of views that refer to the same frontend model object

- `traits_to_sync`:

  Names of the traits to be synchronized with the frontend

- `sync_suspended`:

  Logical value, whether synchronization is suspended

- `custom_msg_callbacks`:

  A list of functions to be called on receiving a message

- `event_callbacks`:

  A list of functions to be called on an event

- `displayed_callbacks`:

  A list of functions to be called when the widget

- `_comm`:

  The 'comm' connecting to the frontend or NULL

- `required_version`:

  Minimum required ipywidgets version in which the current widget class
  is supported.

## Active bindings

- `comm`:

  The 'comm' connecting the frontend (as an active binding)

## Methods

### Public methods

- [`WidgetClass$new()`](#method-Widget-new)

- [`WidgetClass$open()`](#method-Widget-open)

- [`WidgetClass$close()`](#method-Widget-close)

- [`WidgetClass$get_state()`](#method-Widget-get_state)

- [`WidgetClass$set_state()`](#method-Widget-set_state)

- [`WidgetClass$send_state()`](#method-Widget-send_state)

- [`WidgetClass$send()`](#method-Widget-send)

- [`WidgetClass$display_data()`](#method-Widget-display_data)

- [`WidgetClass$handle_comm_opened()`](#method-Widget-handle_comm_opened)

- [`WidgetClass$handle_comm_msg()`](#method-Widget-handle_comm_msg)

- [`WidgetClass$handle_buffers()`](#method-Widget-handle_buffers)

- [`WidgetClass$handle_custom_msg()`](#method-Widget-handle_custom_msg)

- [`WidgetClass$on_msg()`](#method-Widget-on_msg)

- [`WidgetClass$on_event()`](#method-Widget-on_event)

- [`WidgetClass$handle_event()`](#method-Widget-handle_event)

- [`WidgetClass$on_displayed()`](#method-Widget-on_displayed)

- [`WidgetClass$handle_displayed()`](#method-Widget-handle_displayed)

- [`WidgetClass$_send()`](#method-Widget-_send)

- [`WidgetClass$check_version()`](#method-Widget-check_version)

- [`WidgetClass$clone()`](#method-Widget-clone)

Inherited methods

- [`RKernel::HasTraits$notify()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-notify)
- [`RKernel::HasTraits$observe()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-observe)
- [`RKernel::HasTraits$validate()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Initialize an object

#### Usage

    WidgetClass$new(..., open = TRUE)

#### Arguments

- `...`:

  Values used for initialization

- `open`:

  Logical, whether a connection with the frontend should be opened

------------------------------------------------------------------------

### Method [`open()`](https://rdrr.io/r/base/connections.html)

Open a connection to the frontend

#### Usage

    WidgetClass$open()

------------------------------------------------------------------------

### Method [`close()`](https://rdrr.io/r/base/connections.html)

Close the connection to the frontend

#### Usage

    WidgetClass$close()

------------------------------------------------------------------------

### Method `get_state()`

Prepare synchronized traits for sending them to the frontend

#### Usage

    WidgetClass$get_state(keys = NULL)

#### Arguments

- `keys`:

  Keys/names of the traits to be updated in the frontend

------------------------------------------------------------------------

### Method `set_state()`

Update the synchronized states, usually with information from the
frontend

#### Usage

    WidgetClass$set_state(state)

#### Arguments

- `state`:

  A list of values for the synchronized traits

------------------------------------------------------------------------

### Method `send_state()`

Send updated traits to the frontend

#### Usage

    WidgetClass$send_state(keys = NULL, drop_defaults = FALSE)

#### Arguments

- `keys`:

  Keys/names of the traits to be updated in the frontend

- `drop_defaults`:

  Logical value, not yet used

------------------------------------------------------------------------

### Method `send()`

Send content and binary buffers to the fronend

#### Usage

    WidgetClass$send(content, buffers = NULL)

#### Arguments

- `content`:

  Some user-defined information to be send to the frontend

- `buffers`:

  Some raw vector buffers

------------------------------------------------------------------------

### Method [`display_data()`](https://melff.github.io/RKernel/reference/display_data.md)

Send display-data of the widget to the frontend

#### Usage

    WidgetClass$display_data()

------------------------------------------------------------------------

### Method `handle_comm_opened()`

Handle a 'comm' opened in the frontend

#### Usage

    WidgetClass$handle_comm_opened(comm, data)

#### Arguments

- `comm`:

  The 'comm' object that is opened

- `data`:

  Data sent by the frontend

------------------------------------------------------------------------

### Method `handle_comm_msg()`

Handle a message from the frontend

#### Usage

    WidgetClass$handle_comm_msg(comm, msg)

#### Arguments

- `comm`:

  The 'comm' object via which the message is received

- `msg`:

  Message sent by the frontend

------------------------------------------------------------------------

### Method `handle_buffers()`

Handle buffers in message. This method should be overwritten by
inherting classes that actually process data in buffer components of
messages.

#### Usage

    WidgetClass$handle_buffers(msg)

#### Arguments

- `msg`:

  A comm message

------------------------------------------------------------------------

### Method `handle_custom_msg()`

Call the custom message handlers

#### Usage

    WidgetClass$handle_custom_msg(content)

#### Arguments

- `content`:

  The data received

------------------------------------------------------------------------

### Method `on_msg()`

Install a handler for messages being received

#### Usage

    WidgetClass$on_msg(handler, remove = FALSE)

#### Arguments

- `handler`:

  A handler function

- `remove`:

  Logical, should the handler be removed?

------------------------------------------------------------------------

### Method `on_event()`

Install a handler for events in the frontend

#### Usage

    WidgetClass$on_event(event, handler, remove = FALSE)

#### Arguments

- `event`:

  A character that describes the event

- `handler`:

  A handler function

- `remove`:

  Logical, should the handler be removed?

------------------------------------------------------------------------

### Method `handle_event()`

Call the installed event handlers

#### Usage

    WidgetClass$handle_event(event, args)

#### Arguments

- `event`:

  A string that describes the event

- `args`:

  A list of argument passed on with the event

------------------------------------------------------------------------

### Method `on_displayed()`

Install a handler to be called when the widget is displayed

#### Usage

    WidgetClass$on_displayed(handler, remove = FALSE)

#### Arguments

- `handler`:

  A handler function

- `remove`:

  Logical, should the handler be removed?

------------------------------------------------------------------------

### Method `handle_displayed()`

Call the installed display handlers

#### Usage

    WidgetClass$handle_displayed()

------------------------------------------------------------------------

### Method `_send()`

The internal function to send messages to the frontend

#### Usage

    WidgetClass$_send(msg, buffers = NULL)

#### Arguments

- `msg`:

  The message

- `buffers`:

  Raw data buffers or NULL

------------------------------------------------------------------------

### Method `check_version()`

Check whether current widget class is supported by ipywidgets

#### Usage

    WidgetClass$check_version()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    WidgetClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
