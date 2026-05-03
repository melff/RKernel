# A Manager for Comms

Objects of this class are used internally to manage events, they are not
meant to be used by end-users.

## Usage

``` r
EventManager(...)
```

## Arguments

- ...:

  Arguments passed to the inializer

## Functions

- `EventManager()`: The constructor function, returns an Object of Class
  "EventManagerClass"

## Methods

### Public methods

- [`EventManager$new()`](#method-EventManager-initialize)

- [`EventManager$send()`](#method-EventManager-send)

- [`EventManager$on()`](#method-EventManager-on)

- [`EventManager$activate()`](#method-EventManager-activate)

- [`EventManager$suspend()`](#method-EventManager-suspend)

- [`EventManager$clear()`](#method-EventManager-clear)

- [`EventManager$resume()`](#method-EventManager-resume)

- [`EventManager$has()`](#method-EventManager-has)

- [`EventManager$clone()`](#method-EventManager-clone)

------------------------------------------------------------------------

### `EventManager$new()`

Initialize an event manager

#### Usage

    EventManager$new(type)

#### Arguments

- `type`:

  A string, the type of event (e.g. "print")

------------------------------------------------------------------------

### `EventManager$send()`

Send an event

#### Usage

    EventManager$send(event, ...)

#### Arguments

- `event`:

  A string, the name of an event

- `...`:

  Other arguments, sent to the event handler(s)

------------------------------------------------------------------------

### `EventManager$on()`

Install a handler for an event

#### Usage

    EventManager$on(event, handler, remove = FALSE)

#### Arguments

- `event`:

  A string, the name of an event

- `handler`:

  A function

- `remove`:

  A logical value, whether the handler is to be removed

------------------------------------------------------------------------

### `EventManager$activate()`

Activate handlers

#### Usage

    EventManager$activate(event = NULL, all = TRUE)

#### Arguments

- `event`:

  A string, the name of an event, ignored if 'all' is TRUE.

- `all`:

  A logical value, if TRUE, all handlers that belong to the event type
  of the event manager are activated.

------------------------------------------------------------------------

### `EventManager$suspend()`

Suspend handlers

#### Usage

    EventManager$suspend(event = NULL, all = TRUE)

#### Arguments

- `event`:

  A string, the name of an event, ignored if 'all' is TRUE.

- `all`:

  A logical value, if TRUE, all handlers that belong to the event type
  of the event manager are suspended.

------------------------------------------------------------------------

### `EventManager$clear()`

Clear (i.e. remove) handlers for an event

#### Usage

    EventManager$clear(event)

#### Arguments

- `event`:

  A string, the name of an event

------------------------------------------------------------------------

### `EventManager$resume()`

Resume (i.e. reactivate) an event handler

#### Usage

    EventManager$resume()

------------------------------------------------------------------------

### `EventManager$has()`

Check whether the event manager has handlers for the given events

#### Usage

    EventManager$has(events)

#### Arguments

- `events`:

  A character vector with names of events

------------------------------------------------------------------------

### `EventManager$clone()`

The objects of this class are cloneable with this method.

#### Usage

    EventManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
