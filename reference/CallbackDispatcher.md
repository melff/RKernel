# A Dispatcher for Callbacks

Objects in this class are collections of callbacks functions usually
related to certain events. The function `CallbackDispachter` can be used
as an constructor

## Usage

``` r
CallbackDispatcher(...)
```

## Arguments

- ...:

  Arguments passed to the inializer

## Functions

- `CallbackDispatcher()`: The constructor function, returns an Object of
  Class "CallbackDispatcherClass"

## Methods

### Public methods

- [`CallbackDispatcher$register()`](#method-CallbackDispatcher-register)

- [`CallbackDispatcher$clear()`](#method-CallbackDispatcher-clear)

- [`CallbackDispatcher$suspend_handlers()`](#method-CallbackDispatcher-suspend_handlers)

- [`CallbackDispatcher$activate_handlers()`](#method-CallbackDispatcher-activate_handlers)

- [`CallbackDispatcher$run()`](#method-CallbackDispatcher-run)

- [`CallbackDispatcher$clone()`](#method-CallbackDispatcher-clone)

------------------------------------------------------------------------

### `CallbackDispatcher$register()`

Register a function as a callback

#### Usage

    CallbackDispatcher$register(handler, remove)

#### Arguments

- `handler`:

  A function

- `remove`:

  A logical value; whether the function is added or removed from the
  list of callbacks

------------------------------------------------------------------------

### `CallbackDispatcher$clear()`

Remove all callback functions

#### Usage

    CallbackDispatcher$clear()

------------------------------------------------------------------------

### `CallbackDispatcher$suspend_handlers()`

Suspend registered callback functions

#### Usage

    CallbackDispatcher$suspend_handlers()

------------------------------------------------------------------------

### `CallbackDispatcher$activate_handlers()`

(Re-)activate registered callback functions

#### Usage

    CallbackDispatcher$activate_handlers()

------------------------------------------------------------------------

### `CallbackDispatcher$run()`

Run all registered callback functions

#### Usage

    CallbackDispatcher$run(...)

#### Arguments

- `...`:

  Aruments passed on to the handler functions

------------------------------------------------------------------------

### `CallbackDispatcher$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CallbackDispatcher$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
