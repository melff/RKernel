# Widget Layout Templates

R6 classes and constructor functions for widget layout templates

## Usage

``` r
AppLayout(...)

GridspecLayout(...)

# S3 method for class 'GridspecLayout'
x[i, j, ..., drop = TRUE]

# S3 method for class 'GridspecLayout'
x[i, j] <- value

TwoByTwoLayout(...)
```

## Arguments

- ...:

  Arguments used to initialize the fields

- x:

  A GridspecLayout object

- i:

  Integer value(s) referring to the row(s)

- j:

  Integer value(s) referring to the column(s)

- drop:

  Logical, whether the result is a widget or a list with one element if
  both i an j select a single element

- value:

  One or more widgets put at the idicated positions in the grid

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\> [`Box`](https://melff.github.io/RKernel/reference/Boxes.md) -\>
[`GridBox`](https://melff.github.io/RKernel/reference/Boxes.md) -\>
`TemplateBase`

## Public fields

- `grid_gap`:

  The grid-gap CSS attribute

- `justify_content`:

  The justify-content CSS attribute

- `align_items`:

  The align-items CSS attribute

- `width`:

  The width CSS attribute

- `height`:

  The height CSS attribute

## Methods

### Public methods

- [`TemplateBase$new()`](#method-TemplateBase-initialize)

- [`TemplateBase$clone()`](#method-TemplateBase-clone)

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
- [`Box$notify_children_displayed()`](https://melff.github.io/RKernel/reference/Box.html#method-notify_children_displayed)

------------------------------------------------------------------------

### `TemplateBase$new()`

Initializer

#### Usage

    TemplateBase$new(...)

#### Arguments

- `...`:

  Arguments used to initialize the fields

------------------------------------------------------------------------

### `TemplateBase$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TemplateBase$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\> [`Box`](https://melff.github.io/RKernel/reference/Boxes.md) -\>
[`GridBox`](https://melff.github.io/RKernel/reference/Boxes.md) -\>
`TemplateBase` -\> `AppLayout`

## Public fields

- `header`:

  Widget that appears in the header section

- `footer`:

  Widget that appears in the footer section

- `left_sidebar`:

  Widget that appears as left sidebar

- `right_sidebar`:

  Widget that appears as right sidebar

- `center`:

  Widget that appears in the center section

- `pane_widths`:

  Unicode string with CSS widths for the app panes

- `pane_heights`:

  Unicode string with CSS heights for the app panes

- `merge`:

  Boolean, whether space of missing widgets chould be merged

## Methods

### Public methods

- [`AppLayout$new()`](#method-AppLayout-initialize)

- [`AppLayout$clone()`](#method-AppLayout-clone)

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
- [`Box$notify_children_displayed()`](https://melff.github.io/RKernel/reference/Box.html#method-notify_children_displayed)

------------------------------------------------------------------------

### `AppLayout$new()`

Initializer method

#### Usage

    AppLayout$new(...)

#### Arguments

- `...`:

  Arguments, passed on to the superclass initializer

------------------------------------------------------------------------

### `AppLayout$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AppLayout$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\> [`Box`](https://melff.github.io/RKernel/reference/Boxes.md) -\>
[`GridBox`](https://melff.github.io/RKernel/reference/Boxes.md) -\>
`TemplateBase` -\> `GridspecLayout`

## Methods

### Public methods

- [`GridspecLayout$new()`](#method-GridspecLayout-initialize)

- [`GridspecLayout$set_item()`](#method-GridspecLayout-set_item)

- [`GridspecLayout$get_item()`](#method-GridspecLayout-get_item)

- [`GridspecLayout$clone()`](#method-GridspecLayout-clone)

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
- [`Box$notify_children_displayed()`](https://melff.github.io/RKernel/reference/Box.html#method-notify_children_displayed)

------------------------------------------------------------------------

### `GridspecLayout$new()`

Initializer function

#### Usage

    GridspecLayout$new(nrow, ncol, ...)

#### Arguments

- `nrow`:

  A positive integer, the number of rows

- `ncol`:

  A positive integer, the number of columns

- `...`:

  Other arguments, passed to the superclass initializer

------------------------------------------------------------------------

### `GridspecLayout$set_item()`

Set widget in grid cells

#### Usage

    GridspecLayout$set_item(i, j, value)

#### Arguments

- `i`:

  The rows into which the widget is to be placed

- `j`:

  The columns into which the widget is to be placed

- `value`:

  A widget

------------------------------------------------------------------------

### `GridspecLayout$get_item()`

Get widget from grid cells

#### Usage

    GridspecLayout$get_item(i, j)

#### Arguments

- `i`:

  The rows where the widget is located

- `j`:

  The columns where the widget is located

------------------------------------------------------------------------

### `GridspecLayout$clone()`

The objects of this class are cloneable with this method.

#### Usage

    GridspecLayout$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`HasTraits`](https://melff.github.io/RKernel/reference/HasTraits.md)
-\> [`Widget`](https://melff.github.io/RKernel/reference/Widgets.md) -\>
[`DOMWidget`](https://melff.github.io/RKernel/reference/DOMWidgetClass.md)
-\> [`Box`](https://melff.github.io/RKernel/reference/Boxes.md) -\>
[`GridBox`](https://melff.github.io/RKernel/reference/Boxes.md) -\>
`TemplateBase` -\> `TwoByTwoLayout`

## Public fields

- `top_left`:

  Widget that appears on the top left

- `top_right`:

  Widget that appears on the top right

- `bottom_left`:

  Widget that appears on the bottom left

- `bottom_right`:

  Widget that appears on the bottom right

- `merge`:

  Boolean, whether space of missing widgets chould be merged

## Methods

### Public methods

- [`TwoByTwoLayout$new()`](#method-TwoByTwoLayout-initialize)

- [`TwoByTwoLayout$clone()`](#method-TwoByTwoLayout-clone)

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
- [`Box$notify_children_displayed()`](https://melff.github.io/RKernel/reference/Box.html#method-notify_children_displayed)

------------------------------------------------------------------------

### `TwoByTwoLayout$new()`

Initializer method

#### Usage

    TwoByTwoLayout$new(...)

#### Arguments

- `...`:

  Arguments, passed on to the superclass initializer

------------------------------------------------------------------------

### `TwoByTwoLayout$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TwoByTwoLayout$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
