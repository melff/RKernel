# R6 objects that run a help server

An Object of class "sharedHelper" serves HTML help pages for one or
several RKernel processes

## Public fields

- `port`:

  Integer, the port number

- `url`:

  Character string, the base URL of the served help pages

- `orig_httpd`:

  A function, set to the original HTTP server function

## Methods

### Public methods

- [`sharedHelpServer$new()`](#method-sharedHelpServer-new)

- [`sharedHelpServer$httpd()`](#method-sharedHelpServer-httpd)

- [`sharedHelpServer$run()`](#method-sharedHelpServer-run)

- [`sharedHelpServer$publish_port()`](#method-sharedHelpServer-publish_port)

- [`sharedHelpServer$log()`](#method-sharedHelpServer-log)

- [`sharedHelpServer$clone()`](#method-sharedHelpServer-clone)

------------------------------------------------------------------------

### Method `new()`

Intialize the object

#### Usage

    sharedHelpServer$new(port = 0, prefix = "", use_proxy = FALSE)

#### Arguments

- `port`:

  Integer, a port number

- `prefix`:

  A character string, the URL prefix

- `use_proxy`:

  A logical value, whether the help server is supposed to be run behind
  a jupyter proxy

------------------------------------------------------------------------

### Method `httpd()`

The function that serves paths and queries

#### Usage

    sharedHelpServer$httpd(path, query, ...)

#### Arguments

- `path`:

  A character string, the path part of an URL

- `query`:

  An optional HTTP query string

- `...`:

  Any other arguments, passed on to the original 'httpd' function.

------------------------------------------------------------------------

### Method `run()`

The server loop

#### Usage

    sharedHelpServer$run()

------------------------------------------------------------------------

### Method `publish_port()`

Put a port number into a temporary file, for other processes to find

#### Usage

    sharedHelpServer$publish_port(port)

#### Arguments

- `port`:

  An integer, the port number

------------------------------------------------------------------------

### Method [`log()`](https://rdrr.io/r/base/Log.html)

Put log text into a temporary file, for other processes to read

#### Usage

    sharedHelpServer$log(text)

#### Arguments

- `text`:

  A character string to be added to the log file

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    sharedHelpServer$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
