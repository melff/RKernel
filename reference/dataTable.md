# HTML Tables with Interactive Controls

Objects of class "dataTable" provide HTML tables with interactive
controls powered by the DataTable Javascript library.

## Usage

``` r
dataTable(x, ...)

# Default S3 method
dataTable(x, ...)

# S3 method for class 'data.frame'
dataTable(x, ...)

# S3 method for class 'data.set'
dataTable(x, ...)

# S3 method for class 'importer'
dataTable(x, ...)

# S3 method for class 'dataTable'
display_data(
  x,
  ...,
  metadata = emptyNamedList,
  id = attr(x, "id"),
  update = FALSE
)
```

## Arguments

- x:

  A "dataTable" object

- ...:

  Other arguments passed to the initialization method of
  'dataTableClass' R6 objects

- metadata:

  A list of metadata strings

- id:

  An ID string

- update:

  A logical value; whether an existing display item will be updated

## Methods (by class)

- `dataTable(default)`: Default method

- `dataTable(data.frame)`: data.frame method

- `dataTable(data.set)`: data.set method

- `dataTable(importer)`: importer method

## Methods (by generic)

- `display_data(dataTable)`: dataTable method for display_data

## Functions

- `dataTable()`: A dataTable constructor

## Public fields

- `w`:

  A container widget or NULL

- `page`:

  Number of the current page

- `m`:

  Width of the object divided by 'size'

- `r`:

  Remainder of the widht of the object divided by 'size'

- `size`:

  Number of columns in each group for horizontal paging

- `iframe`:

  An \<iframe\> container or NULL

- `b_left`:

  Button to scroll left

- `b_right`:

  Button to scroll right

- `b_first`:

  Button to scroll to the first group of columns

- `b_last`:

  Button to scroll to the last group of columns

- `dt`:

  HTML code for the visible table

- `obj`:

  The tabular object being dispayed

- `label`:

  A string label that shows the columns being displayed

- `style`:

  A string with CSS styling

- `navigator`:

  A container widget that contains the navigator buttons

## Methods

### Public methods

- [`dataTableClass$new()`](#method-dataTable-new)

- [`dataTableClass$show_columns()`](#method-dataTable-show_columns)

- [`dataTableClass$page_left()`](#method-dataTable-page_left)

- [`dataTableClass$page_right()`](#method-dataTable-page_right)

- [`dataTableClass$page_first()`](#method-dataTable-page_first)

- [`dataTableClass$page_last()`](#method-dataTable-page_last)

- [`dataTableClass$draw()`](#method-dataTable-draw)

- [`dataTableClass$clone()`](#method-dataTable-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the DataTable

#### Usage

    dataTableClass$new(obj, size = 50, ...)

#### Arguments

- `obj`:

  The object to be displayed

- `size`:

  An integer, the number of columns pre-formatted on each page.

- `...`:

  Other arguments, ignored

------------------------------------------------------------------------

### Method `show_columns()`

Show which columns are displayed

#### Usage

    dataTableClass$show_columns()

------------------------------------------------------------------------

### Method `page_left()`

Go one page to the left

#### Usage

    dataTableClass$page_left()

------------------------------------------------------------------------

### Method `page_right()`

Go one page to the right

#### Usage

    dataTableClass$page_right()

------------------------------------------------------------------------

### Method `page_first()`

Go to the first page (to the left)

#### Usage

    dataTableClass$page_first()

------------------------------------------------------------------------

### Method `page_last()`

Go to the last page (to the right)

#### Usage

    dataTableClass$page_last()

------------------------------------------------------------------------

### Method `draw()`

Draw the iframe with the data table contents

#### Usage

    dataTableClass$draw()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    dataTableClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
