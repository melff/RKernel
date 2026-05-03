# Widgets for Text Elements with Floating Point Numbers Bounded within an Interval

An R6 class and a constructor function for the creation of widgets that
can be used to manipulate floating point numbers.

## Usage

``` r
BoundedFloatText(value = 0, min = 0, max = 100, step = 0.1, ...)
```

## Arguments

- value:

  Initial value of the floating point number.

- min:

  Lower limit of the enclosing interval.

- max:

  Upper limit of the enclosing interval.

- step:

  Increment by which the number is increased or decreased by the text
  field controls.

- ...:

  Other arguments.

## Details

The function `BoundedFloatText` creates objects of the R6 Class
"BoundedFloatTextClass", which in turn have the S3 class attribute
"BoundedFloatText".

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\>
[`DescriptionWidget`](https://melff.github.io/RKernel/reference/DescriptionWidget.md)
-\> `ValueWidget` -\>
[`BoundedFloatWidget`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.md)
-\> `BoundedFloatText`

## Public fields

- `_model_name`:

  Name of the Javascript model in the frontend.

- `_view_name`:

  Name of the Javascript view in the frontend.

- `disabled`:

  A [Boolean](https://melff.github.io/RKernel/reference/Boolean.md)
  traitlet, whether the text widget is disabled.

- `continuous_update`:

  A [Boolean](https://melff.github.io/RKernel/reference/Boolean.md)
  traitlet, whether the text widget is continuously updated upon change
  in the frontend.

- `step`:

  A [Float](https://melff.github.io/RKernel/reference/Float.md)
  traitlet, a step size by which the value is incremented or decremented
  if the arrows are clicked.

## Methods

### Public methods

- [`BoundedFloatText$clone()`](#method-BoundedFloatText-clone)

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
- [`BoundedFloatWidget$initialize()`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.html#method-initialize)
- [`BoundedFloatWidget$validate_max()`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.html#method-validate_max)
- [`BoundedFloatWidget$validate_min()`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.html#method-validate_min)
- [`BoundedFloatWidget$validate_value()`](https://melff.github.io/RKernel/reference/BoundedFloatWidget.html#method-validate_value)

------------------------------------------------------------------------

### `BoundedFloatText$clone()`

The objects of this class are cloneable with this method.

#### Usage

    BoundedFloatText$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
