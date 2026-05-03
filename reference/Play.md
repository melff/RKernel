# A Player Widget

An R6 class and a constructor function for the creation of a player
widget, which automatically increases its value-

## Usage

``` r
Play(
  value = 0L,
  min = 0L,
  max = 100L,
  interval = 100L,
  step = 1L,
  show_repeat = TRUE,
  ...
)
```

## Arguments

- value:

  Integer, an initial value.

- min:

  Integer, the minimum value.

- max:

  Integer, the maximum value.

- interval:

  Integer, the maximum value of the intrval .

- step:

  The maximum value for the play control.

- show_repeat:

  Logical, whether to show a repeat toggle button.

- ...:

  Further arguments, passed to the superclass constructor.

## Functions

- `Play()`: The player widget constructor function.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `ValueWidget` -\>
[`BoundedIntWidget`](https://melff.github.io/RKernel/reference/BoundedIntWidget.md)
-\> `Play`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend

- `_view_name`:

  Name of the Javascript model view in the frontend

- `interval`:

  An Integer traitlet, the time interval between between two steps.

- `step`:

  An Integer traitlet, the step size.

- `_playing`:

  A Boolean traitlet, indicates wether the player widget is running.

- `playing`:

  A Boolean traitlet, indicates wether the player widget is running.

- `_repeat`:

  A Boolean traitlet, indicates wether the the repeat toggle is on.

- `repeat`:

  A Boolean traitlet, indicates wether the the repeat toggle is on.

- `show_repeat`:

  A Boolean traitlet, determines whether to show a repeat toggle button.

## Methods

### Public methods

- [`Play$clone()`](#method-Play-clone)

Inherited methods

- [`HasTraits$notify()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-notify)
- [`HasTraits$observe()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-observe)
- [`HasTraits$validate()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-validate)
- [`Widget$_send()`](https://melff.github.io/RKernel/reference/Widget.html#method-_send)
- [`Widget$check_version()`](https://melff.github.io/RKernel/reference/Widget.html#method-check_version)
- [`Widget$close()`](https://melff.github.io/RKernel/reference/Widget.html#method-close)
- [`Widget$display_data()`](https://melff.github.io/RKernel/reference/Widget.html#method-display_data)
- [`Widget$get_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-get_state)
- [`Widget$handle_buffers()`](https://melff.github.io/RKernel/reference/Widget.html#method-handle_buffers)
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
- [`BoundedIntWidget$initialize()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-initialize)
- [`BoundedIntWidget$validate_max()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-validate_max)
- [`BoundedIntWidget$validate_min()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-validate_min)
- [`BoundedIntWidget$validate_value()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-validate_value)

------------------------------------------------------------------------

### `Play$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Play$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
