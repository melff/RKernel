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

- [`EventManagerClass$new()`](#method-EventManager-new)

- [`EventManagerClass$send()`](#method-EventManager-send)

- [`EventManagerClass$on()`](#method-EventManager-on)

- [`EventManagerClass$activate()`](#method-EventManager-activate)

- [`EventManagerClass$suspend()`](#method-EventManager-suspend)

- [`EventManagerClass$clear()`](#method-EventManager-clear)

- [`EventManagerClass$resume()`](#method-EventManager-resume)

- [`EventManagerClass$has()`](#method-EventManager-has)

- [`EventManagerClass$clone()`](#method-EventManager-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize an event manager

#### Usage

    EventManagerClass$new(type)

#### Arguments

- `type`:

  A string, the type of event (e.g. "print")

------------------------------------------------------------------------

### Method `send()`

Send an event

#### Usage

    EventManagerClass$send(event, ...)

#### Arguments

- `event`:

  A string, the name of an event

- `...`:

  Other arguments, sent to the event handler(s)

------------------------------------------------------------------------

### Method `on()`

Install a handler for an event

#### Usage

    EventManagerClass$on(event, handler, remove = FALSE)

#### Arguments

- `event`:

  A string, the name of an event

- `handler`:

  A function

- `remove`:

  A logical value, whether the handler is to be removed

------------------------------------------------------------------------

### Method `activate()`

Activate handlers

#### Usage

    EventManagerClass$activate(event = NULL, all = TRUE)

#### Arguments

- `event`:

  A string, the name of an event, ignored if 'all' is TRUE.

- `all`:

  A logical value, if TRUE, all handlers that belong to the event type
  of the event manager are activated.

------------------------------------------------------------------------

### Method `suspend()`

Suspend handlers

#### Usage

    EventManagerClass$suspend(event = NULL, all = TRUE)

#### Arguments

- `event`:

  A string, the name of an event, ignored if 'all' is TRUE.

- `all`:

  A logical value, if TRUE, all handlers that belong to the event type
  of the event manager are suspended.

------------------------------------------------------------------------

### Method `clear()`

Clear (i.e. remove) handlers for an event

#### Usage

    EventManagerClass$clear(event)

#### Arguments

- `event`:

  A string, the name of an event

------------------------------------------------------------------------

### Method `resume()`

Resume (i.e. reactivate) an event handler

#### Usage

    EventManagerClass$resume()

------------------------------------------------------------------------

### Method `has()`

Check whether the event manager has handlers for the given events

#### Usage

    EventManagerClass$has(events)

#### Arguments

- `events`:

  A character vector with names of events

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    EventManagerClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
