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

- [`CommManager$add_handlers()`](#method-CommManager-add_handlers)

- [`CommManager$remove_handlers()`](#method-CommManager-remove_handlers)

- [`CommManager$has_handlers()`](#method-CommManager-has_handlers)

- [`CommManager$get_handlers()`](#method-CommManager-get_handlers)

- [`CommManager$get_comms()`](#method-CommManager-get_comms)

- [`CommManager$new_comm()`](#method-CommManager-new_comm)

- [`CommManager$handle_open()`](#method-CommManager-handle_open)

- [`CommManager$handle_close()`](#method-CommManager-handle_close)

- [`CommManager$handle_msg()`](#method-CommManager-handle_msg)

- [`CommManager$has()`](#method-CommManager-has)

- [`CommManager$send()`](#method-CommManager-send)

- [`CommManager$send_open()`](#method-CommManager-send_open)

- [`CommManager$send_close()`](#method-CommManager-send_close)

- [`CommManager$list_targets()`](#method-CommManager-list_targets)

- [`CommManager$clone()`](#method-CommManager-clone)

------------------------------------------------------------------------

### `CommManager$add_handlers()`

Add a handler for a comm target

#### Usage

    CommManager$add_handlers(target_name, handlers)

#### Arguments

- `target_name`:

  A string, the name of the target.

- `handlers`:

  A named list of handlers

------------------------------------------------------------------------

### `CommManager$remove_handlers()`

Remove the handlers of a comm target

#### Usage

    CommManager$remove_handlers(target_name)

#### Arguments

- `target_name`:

  A string, the name of the target

------------------------------------------------------------------------

### `CommManager$has_handlers()`

Check if handlers for a target exist

#### Usage

    CommManager$has_handlers(target_name)

#### Arguments

- `target_name`:

  A string, the name of the target

------------------------------------------------------------------------

### `CommManager$get_handlers()`

Get the handlers for a target

#### Usage

    CommManager$get_handlers(target_name)

#### Arguments

- `target_name`:

  A string, the name of the target

------------------------------------------------------------------------

### `CommManager$get_comms()`

Get all comms or all comms related to a target

#### Usage

    CommManager$get_comms(target_name = NULL)

#### Arguments

- `target_name`:

  A string, the name of the target or NULL. If NULL,

------------------------------------------------------------------------

### `CommManager$new_comm()`

Create a new comm related to a target

#### Usage

    CommManager$new_comm(target_name)

#### Arguments

- `target_name`:

  A string, the name of the target

------------------------------------------------------------------------

### `CommManager$handle_open()`

Handle a 'comm open' request from the frontend

#### Usage

    CommManager$handle_open(target_name, id, data)

#### Arguments

- `target_name`:

  A string, the name of the target

- `id`:

  A string, the comm id

- `data`:

  Data sent by the frontend

------------------------------------------------------------------------

### `CommManager$handle_close()`

Handle a 'comm close' request from the frontend

#### Usage

    CommManager$handle_close(id, data)

#### Arguments

- `id`:

  A string, the comm id

- `data`:

  Data sent by the frontend

------------------------------------------------------------------------

### `CommManager$handle_msg()`

Handle a comm message from the frontend

#### Usage

    CommManager$handle_msg(id, data)

#### Arguments

- `id`:

  A string, the comm id

- `data`:

  Data sent by the frontend

------------------------------------------------------------------------

### `CommManager$has()`

Check whether the comm manager has info about comm with an id

#### Usage

    CommManager$has(id)

#### Arguments

- `id`:

  A string, possibly a comm id

------------------------------------------------------------------------

### `CommManager$send()`

Send data to the frontend

#### Usage

    CommManager$send(id, data, metadata = emptyNamedList, buffers = NULL)

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

### `CommManager$send_open()`

Send an 'open' request to the frontend

#### Usage

    CommManager$send_open(
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

### `CommManager$send_close()`

Send an 'close' request to the frontend

#### Usage

    CommManager$send_close(
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

### `CommManager$list_targets()`

Return a list of targets

#### Usage

    CommManager$list_targets()

------------------------------------------------------------------------

### `CommManager$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CommManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
