# Prepare an R Object for Being Displayed

A generic function that prepares R objects for display using
[`display()`](https://melff.github.io/RKernel/reference/display.md)

## Usage

``` r
# S3 method for class 'Widget'
display_data(x, ..., metadata = emptyNamedList, id = uuid(), update = FALSE)

display_data(x, ...)

# Default S3 method
display_data(
  x,
  ...,
  data,
  metadata = emptyNamedList,
  id = UUIDgenerate(),
  update = FALSE
)

# S3 method for class 'htmlwidget'
display_data(
  x,
  ...,
  metadata = emptyNamedList,
  id = UUIDgenerate(),
  update = FALSE
)

# S3 method for class 'display_data'
display_data(x, ...)

# S3 method for class 'update_display_data'
display_data(x, ...)

# S3 method for class 'svg'
display_data(x, ...)

# S3 method for class 'display_data'
update(object, ...)

# S3 method for class 'data.frame'
display_data(
  x,
  ...,
  metadata = emptyNamedList,
  id = UUIDgenerate(),
  update = FALSE
)

# S3 method for class 'data.set'
display_data(
  x,
  ...,
  metadata = emptyNamedList,
  id = UUIDgenerate(),
  update = FALSE
)

# S3 method for class 'importer'
display_data(
  x,
  ...,
  metadata = emptyNamedList,
  id = UUIDgenerate(),
  update = FALSE
)

# S3 method for class 'descriptions'
display_data(
  x,
  ...,
  metadata = emptyNamedList,
  id = UUIDgenerate(),
  update = FALSE
)

# S3 method for class 'matrix'
display_data(
  x,
  ...,
  metadata = emptyNamedList,
  id = UUIDgenerate(),
  update = FALSE
)

# S3 method for class 'html_elem'
display_data(
  x,
  ...,
  metadata = emptyNamedList,
  id = UUIDgenerate(),
  update = FALSE
)

# S3 method for class 'shiny.tag'
display_data(
  x,
  ...,
  metadata = emptyNamedList,
  id = UUIDgenerate(),
  update = FALSE
)

# S3 method for class 'shiny.tag.list'
display_data(
  x,
  ...,
  metadata = emptyNamedList,
  id = UUIDgenerate(),
  update = FALSE
)

# S3 method for class 'iframe'
display_data(
  x,
  ...,
  metadata = emptyNamedList,
  id = attr(x, "id"),
  update = FALSE
)

# S3 method for class 'htmlTable'
display_data(
  x,
  ...,
  metadata = emptyNamedList,
  id = UUIDgenerate(),
  update = FALSE
)

# S3 method for class 'tableHTML'
display_data(
  x,
  ...,
  metadata = emptyNamedList,
  id = UUIDgenerate(),
  update = FALSE
)

# S3 method for class 'help_files_with_topic'
display_data(
  x,
  ...,
  id = UUIDgenerate(),
  update = FALSE,
  embedded = FALSE,
  include_button = TRUE
)

# S3 method for class 'hsearch'
display_data(x, ..., id = UUIDgenerate(), update = FALSE)
```

## Arguments

- x:

  An object

- ...:

  Optional arguments tagged by mime types with mime data

- metadata:

  A list with named elements, containing metadata

- id:

  An identifier string

- update:

  A logical value, whether a new display item should be created or an
  existing one should be updated

- data:

  A list with named elements, containing mime data

- object:

  An object of class "display_data"

- embedded:

  A logical value, whether the help page should be shown embedded in the
  Jupyter notebook

- include_button:

  A logical value, whether to include a button that opens a new tab for
  the help page.

## Value

An object of class "display_data"

## Methods (by class)

- `display_data(Widget)`: Method for jupyter widgets

- `display_data(default)`: Default method

- `display_data(htmlwidget)`: S3 method for html widgets

- `display_data(display_data)`: S3 method for "display_data" objects

- `display_data(update_display_data)`: S3 method for
  "update_display_data" objects

- `display_data(svg)`: S3 method for "svg" objects

- `display_data(data.frame)`: S3 method for class 'data.frame'

- `display_data(data.set)`: S3 method for class 'data.set'

- `display_data(importer)`: S3 method for class 'importer'

- `display_data(descriptions)`: S3 method for class 'descriptions'

- `display_data(matrix)`: S3 method for matrices

- `display_data(html_elem)`: S3 method for "html_elem" objects (see
  [`html`](https://melff.github.io/memisc/reference/html.html))

- `display_data(shiny.tag)`: S3 methods for "shiny.tab" objects

- `display_data(shiny.tag.list)`: S3 methods for "shiny.tab.list"
  objects

- `display_data(iframe)`: S3 methods for "iframe" objects

- `display_data(htmlTable)`: S3 methods for "htmlTable" objects

- `display_data(tableHTML)`: S3 methods for "tableHTML" objects

- `display_data(help_files_with_topic)`: S3 method for help pages

- `display_data(hsearch)`: S3 method for results of 'help.search()'

## Methods (by generic)

- `update(display_data)`: "update" method for "display_data" objects
