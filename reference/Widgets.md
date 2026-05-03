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

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
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

- [`Widget$new()`](#method-Widget-initialize)

- [`Widget$open()`](#method-Widget-open)

- [`Widget$close()`](#method-Widget-close)

- [`Widget$get_state()`](#method-Widget-get_state)

- [`Widget$set_state()`](#method-Widget-set_state)

- [`Widget$send_state()`](#method-Widget-send_state)

- [`Widget$send()`](#method-Widget-send)

- [`Widget$display_data()`](#method-Widget-display_data)

- [`Widget$handle_comm_opened()`](#method-Widget-handle_comm_opened)

- [`Widget$handle_comm_msg()`](#method-Widget-handle_comm_msg)

- [`Widget$handle_buffers()`](#method-Widget-handle_buffers)

- [`Widget$handle_custom_msg()`](#method-Widget-handle_custom_msg)

- [`Widget$on_msg()`](#method-Widget-on_msg)

- [`Widget$on_event()`](#method-Widget-on_event)

- [`Widget$handle_event()`](#method-Widget-handle_event)

- [`Widget$on_displayed()`](#method-Widget-on_displayed)

- [`Widget$handle_displayed()`](#method-Widget-handle_displayed)

- [`Widget$_send()`](#method-Widget-_send)

- [`Widget$check_version()`](#method-Widget-check_version)

- [`Widget$clone()`](#method-Widget-clone)

Inherited methods

- [`HasTraits$notify()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-notify)
- [`HasTraits$observe()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-observe)
- [`HasTraits$validate()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-validate)

------------------------------------------------------------------------

### `Widget$new()`

Initialize an object

#### Usage

    Widget$new(..., open = TRUE)

#### Arguments

- `...`:

  Values used for initialization

- `open`:

  Logical, whether a connection with the frontend should be opened

------------------------------------------------------------------------

### `Widget$open()`

Open a connection to the frontend

#### Usage

    Widget$open()

------------------------------------------------------------------------

### `Widget$close()`

Close the connection to the frontend

#### Usage

    Widget$close()

------------------------------------------------------------------------

### `Widget$get_state()`

Prepare synchronized traits for sending them to the frontend

#### Usage

    Widget$get_state(keys = NULL)

#### Arguments

- `keys`:

  Keys/names of the traits to be updated in the frontend

------------------------------------------------------------------------

### `Widget$set_state()`

Update the synchronized states, usually with information from the
frontend

#### Usage

    Widget$set_state(state)

#### Arguments

- `state`:

  A list of values for the synchronized traits

------------------------------------------------------------------------

### `Widget$send_state()`

Send updated traits to the frontend

#### Usage

    Widget$send_state(keys = NULL, drop_defaults = FALSE)

#### Arguments

- `keys`:

  Keys/names of the traits to be updated in the frontend

- `drop_defaults`:

  Logical value, not yet used

------------------------------------------------------------------------

### `Widget$send()`

Send content and binary buffers to the fronend

#### Usage

    Widget$send(content, buffers = NULL)

#### Arguments

- `content`:

  Some user-defined information to be send to the frontend

- `buffers`:

  Some raw vector buffers

------------------------------------------------------------------------

### `Widget$display_data()`

Send display-data of the widget to the frontend

#### Usage

    Widget$display_data()

------------------------------------------------------------------------

### `Widget$handle_comm_opened()`

Handle a 'comm' opened in the frontend

#### Usage

    Widget$handle_comm_opened(comm, data)

#### Arguments

- `comm`:

  The 'comm' object that is opened

- `data`:

  Data sent by the frontend

------------------------------------------------------------------------

### `Widget$handle_comm_msg()`

Handle a message from the frontend

#### Usage

    Widget$handle_comm_msg(comm, msg)

#### Arguments

- `comm`:

  The 'comm' object via which the message is received

- `msg`:

  Message sent by the frontend

------------------------------------------------------------------------

### `Widget$handle_buffers()`

Handle buffers in message. This method should be overwritten by
inherting classes that actually process data in buffer components of
messages.

#### Usage

    Widget$handle_buffers(msg)

#### Arguments

- `msg`:

  A comm message

------------------------------------------------------------------------

### `Widget$handle_custom_msg()`

Call the custom message handlers

#### Usage

    Widget$handle_custom_msg(content)

#### Arguments

- `content`:

  The data received

------------------------------------------------------------------------

### `Widget$on_msg()`

Install a handler for messages being received

#### Usage

    Widget$on_msg(handler, remove = FALSE)

#### Arguments

- `handler`:

  A handler function

- `remove`:

  Logical, should the handler be removed?

------------------------------------------------------------------------

### `Widget$on_event()`

Install a handler for events in the frontend

#### Usage

    Widget$on_event(event, handler, remove = FALSE)

#### Arguments

- `event`:

  A character that describes the event

- `handler`:

  A handler function

- `remove`:

  Logical, should the handler be removed?

------------------------------------------------------------------------

### `Widget$handle_event()`

Call the installed event handlers

#### Usage

    Widget$handle_event(event, args)

#### Arguments

- `event`:

  A string that describes the event

- `args`:

  A list of argument passed on with the event

------------------------------------------------------------------------

### `Widget$on_displayed()`

Install a handler to be called when the widget is displayed

#### Usage

    Widget$on_displayed(handler, remove = FALSE)

#### Arguments

- `handler`:

  A handler function

- `remove`:

  Logical, should the handler be removed?

------------------------------------------------------------------------

### `Widget$handle_displayed()`

Call the installed display handlers

#### Usage

    Widget$handle_displayed()

------------------------------------------------------------------------

### `Widget$_send()`

The internal function to send messages to the frontend

#### Usage

    Widget$_send(msg, buffers = NULL)

#### Arguments

- `msg`:

  The message

- `buffers`:

  Raw data buffers or NULL

------------------------------------------------------------------------

### `Widget$check_version()`

Check whether current widget class is supported by ipywidgets

#### Usage

    Widget$check_version()

------------------------------------------------------------------------

### `Widget$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Widget$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
