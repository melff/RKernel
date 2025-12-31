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

[`RKernel::HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\>
[`RKernel::Widget`](https://melff.github.io/RKernel/reference/Widgets.md)
-\>
[`RKernel::DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`RKernel::DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `RKernel::ValueWidget` -\>
[`RKernel::BoundedIntWidget`](https://melff.github.io/RKernel/reference/BoundedIntWidget.md)
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

- [`PlayClass$clone()`](#method-Play-clone)

Inherited methods

- [`RKernel::HasTraits$notify()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-notify)
- [`RKernel::HasTraits$observe()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-observe)
- [`RKernel::HasTraits$validate()`](https://melff.github.io/RKernel/reference/HasTraits.html#method-validate)
- [`RKernel::Widget$_send()`](https://melff.github.io/RKernel/reference/Widget.html#method-_send)
- [`RKernel::Widget$check_version()`](https://melff.github.io/RKernel/reference/Widget.html#method-check_version)
- [`RKernel::Widget$close()`](https://melff.github.io/RKernel/reference/Widget.html#method-close)
- [`RKernel::Widget$display_data()`](https://melff.github.io/RKernel/reference/Widget.html#method-display_data)
- [`RKernel::Widget$get_state()`](https://melff.github.io/RKernel/reference/Widget.html#method-get_state)
- [`RKernel::Widget$handle_buffers()`](https://melff.github.io/RKernel/reference/Widget.html#method-handle_buffers)
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
- [`RKernel::BoundedIntWidget$initialize()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-initialize)
- [`RKernel::BoundedIntWidget$validate_max()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-validate_max)
- [`RKernel::BoundedIntWidget$validate_min()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-validate_min)
- [`RKernel::BoundedIntWidget$validate_value()`](https://melff.github.io/RKernel/reference/BoundedIntWidget.html#method-validate_value)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PlayClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
