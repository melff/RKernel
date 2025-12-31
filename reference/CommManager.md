# A Manager for Comms

Objects of this class are used internally to manage comms, they are not
meant to be used by end-users.

## Usage

``` r
CommManager(...)
```

## Arguments

- ...:

  Arguments passed to the inializer

## Details

See the documentation of [Jupyter custom
messages](https://jupyter-client.readthedocs.io/en/latest/messaging.html#custom-messages).

## Functions

- `CommManager()`: A constructor for objects in the 'CommManagerClass'

## Public fields

- `comms`:

  A list of Comms.

## Methods

### Public methods

- [`CommManagerClass$add_handlers()`](#method-CommManager-add_handlers)

- [`CommManagerClass$remove_handlers()`](#method-CommManager-remove_handlers)

- [`CommManagerClass$has_handlers()`](#method-CommManager-has_handlers)

- [`CommManagerClass$get_handlers()`](#method-CommManager-get_handlers)

- [`CommManagerClass$get_comms()`](#method-CommManager-get_comms)

- [`CommManagerClass$new_comm()`](#method-CommManager-new_comm)

- [`CommManagerClass$handle_open()`](#method-CommManager-handle_open)

- [`CommManagerClass$handle_close()`](#method-CommManager-handle_close)

- [`CommManagerClass$handle_msg()`](#method-CommManager-handle_msg)

- [`CommManagerClass$has()`](#method-CommManager-has)

- [`CommManagerClass$send()`](#method-CommManager-send)

- [`CommManagerClass$send_open()`](#method-CommManager-send_open)

- [`CommManagerClass$send_close()`](#method-CommManager-send_close)

- [`CommManagerClass$list_targets()`](#method-CommManager-list_targets)

- [`CommManagerClass$clone()`](#method-CommManager-clone)

------------------------------------------------------------------------

### Method `add_handlers()`

Add a handler for a comm target

#### Usage

    CommManagerClass$add_handlers(target_name, handlers)

#### Arguments

- `target_name`:

  A string, the name of the target.

- `handlers`:

  A named list of handlers

------------------------------------------------------------------------

### Method `remove_handlers()`

Remove the handlers of a comm target

#### Usage

    CommManagerClass$remove_handlers(target_name)

#### Arguments

- `target_name`:

  A string, the name of the target

------------------------------------------------------------------------

### Method `has_handlers()`

Check if handlers for a target exist

#### Usage

    CommManagerClass$has_handlers(target_name)

#### Arguments

- `target_name`:

  A string, the name of the target

------------------------------------------------------------------------

### Method `get_handlers()`

Get the handlers for a target

#### Usage

    CommManagerClass$get_handlers(target_name)

#### Arguments

- `target_name`:

  A string, the name of the target

------------------------------------------------------------------------

### Method `get_comms()`

Get all comms or all comms related to a target

#### Usage

    CommManagerClass$get_comms(target_name = NULL)

#### Arguments

- `target_name`:

  A string, the name of the target or NULL. If NULL,

------------------------------------------------------------------------

### Method `new_comm()`

Create a new comm related to a target

#### Usage

    CommManagerClass$new_comm(target_name)

#### Arguments

- `target_name`:

  A string, the name of the target

------------------------------------------------------------------------

### Method `handle_open()`

Handle a 'comm open' request from the frontend

#### Usage

    CommManagerClass$handle_open(target_name, id, data)

#### Arguments

- `target_name`:

  A string, the name of the target

- `id`:

  A string, the comm id

- `data`:

  Data sent by the frontend

------------------------------------------------------------------------

### Method `handle_close()`

Handle a 'comm close' request from the frontend

#### Usage

    CommManagerClass$handle_close(id, data)

#### Arguments

- `id`:

  A string, the comm id

- `data`:

  Data sent by the frontend

------------------------------------------------------------------------

### Method `handle_msg()`

Handle a comm message from the frontend

#### Usage

    CommManagerClass$handle_msg(id, data)

#### Arguments

- `id`:

  A string, the comm id

- `data`:

  Data sent by the frontend

------------------------------------------------------------------------

### Method `has()`

Check whether the comm manager has info about comm with an id

#### Usage

    CommManagerClass$has(id)

#### Arguments

- `id`:

  A string, possibly a comm id

------------------------------------------------------------------------

### Method `send()`

Send data to the frontend

#### Usage

    CommManagerClass$send(id, data, metadata = emptyNamedList, buffers = NULL)

#### Arguments

- `id`:

  A string, the comm id

- `data`:

  A named list

- `metadata`:

  A named list

- `buffers`:

  A list of raw vectors or NULL

------------------------------------------------------------------------

### Method `send_open()`

Send an 'open' request to the frontend

#### Usage

    CommManagerClass$send_open(
      id,
      target_name,
      data,
      metadata = emptyNamedList,
      buffers = NULL
    )

#### Arguments

- `id`:

  A string, the comm id

- `target_name`:

  A string, the name of the target

- `data`:

  A named list

- `metadata`:

  A named list

- `buffers`:

  A list of raw vectors or NULL

------------------------------------------------------------------------

### Method `send_close()`

Send an 'close' request to the frontend

#### Usage

    CommManagerClass$send_close(
      id,
      data = emptyNamedList,
      metadata = emptyNamedList,
      buffers = NULL
    )

#### Arguments

- `id`:

  A string, the comm id

- `data`:

  A named list

- `metadata`:

  A named list

- `buffers`:

  A list of raw vectors or NULL

------------------------------------------------------------------------

### Method `list_targets()`

Return a list of targets

#### Usage

    CommManagerClass$list_targets()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CommManagerClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
