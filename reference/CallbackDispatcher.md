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

- [`CallbackDispatcherClass$register()`](#method-CallbackDispatcher-register)

- [`CallbackDispatcherClass$clear()`](#method-CallbackDispatcher-clear)

- [`CallbackDispatcherClass$suspend_handlers()`](#method-CallbackDispatcher-suspend_handlers)

- [`CallbackDispatcherClass$activate_handlers()`](#method-CallbackDispatcher-activate_handlers)

- [`CallbackDispatcherClass$run()`](#method-CallbackDispatcher-run)

- [`CallbackDispatcherClass$clone()`](#method-CallbackDispatcher-clone)

------------------------------------------------------------------------

### Method `register()`

Register a function as a callback

#### Usage

    CallbackDispatcherClass$register(handler, remove)

#### Arguments

- `handler`:

  A function

- `remove`:

  A logical value; whether the function is added or removed from the
  list of callbacks

------------------------------------------------------------------------

### Method `clear()`

Remove all callback functions

#### Usage

    CallbackDispatcherClass$clear()

------------------------------------------------------------------------

### Method `suspend_handlers()`

Suspend registered callback functions

#### Usage

    CallbackDispatcherClass$suspend_handlers()

------------------------------------------------------------------------

### Method `activate_handlers()`

(Re-)activate registered callback functions

#### Usage

    CallbackDispatcherClass$activate_handlers()

------------------------------------------------------------------------

### Method `run()`

Run all registered callback functions

#### Usage

    CallbackDispatcherClass$run(...)

#### Arguments

- `...`:

  Aruments passed on to the handler functions

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CallbackDispatcherClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
