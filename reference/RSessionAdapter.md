# A Rich R Session Interface

Objects from this class handle the "higher-level" interaction between
the frontend and the R session. There can be more such interfaces to a
session. For example, for the main REPL and for a REPL created by a call
to browser().

## Public fields

- `session`:

  An RKernelSession object or \`NULL\`

- `prompt`:

  The R console prompt or NULL

- `browse_prompt`:

  The prompt created by a call to \`browser()\`

- `io_timeout`:

  An integer number a timeout in microseconds

- `stdout`:

  Accumulated output via the stdout channel.

- `stderr`:

  Accumulated output via the stderr channel.

- `stdout_callback`:

  A function to be called with stdout text or NULL

- `stderr_callback`:

  A function to be called with stderr text or NULL

- `browser_callback`:

  A function to be called when a browser prompt is encountered or NULL

- `prompt_callback`:

  A function to be called when a command prompt is encountered or NULL

- `input_callback`:

  A function to be called when input is required or NULL

- `echo`:

  A logical value, if TRUE code sent to the R process will be echoed

- `found_prompt`:

  A logical value, whether a prompt has been found in the output of the
  R process

- `found_browse_prompt`:

  The latest instance of the browser prompt pattern found in the R
  process output

- `errored`:

  A logical value, whether an error occurred in the R session

## Methods

### Public methods

- [`RSessionAdapter$aggreg_stdout()`](#method-RSessionAdapter-aggreg_stdout)

- [`RSessionAdapter$aggreg_stderr()`](#method-RSessionAdapter-aggreg_stderr)

- [`RSessionAdapter$collect()`](#method-RSessionAdapter-collect)

- [`RSessionAdapter$new()`](#method-RSessionAdapter-new)

- [`RSessionAdapter$run_code()`](#method-RSessionAdapter-run_code)

- [`RSessionAdapter$interrupt()`](#method-RSessionAdapter-interrupt)

- [`RSessionAdapter$process_output()`](#method-RSessionAdapter-process_output)

- [`RSessionAdapter$run_cmd()`](#method-RSessionAdapter-run_cmd)

- [`RSessionAdapter$getOption()`](#method-RSessionAdapter-getOption)

- [`RSessionAdapter$eval()`](#method-RSessionAdapter-eval)

- [`RSessionAdapter$eval_code()`](#method-RSessionAdapter-eval_code)

- [`RSessionAdapter$ls()`](#method-RSessionAdapter-ls)

- [`RSessionAdapter$get()`](#method-RSessionAdapter-get)

- [`RSessionAdapter$assign()`](#method-RSessionAdapter-assign)

- [`RSessionAdapter$setOption()`](#method-RSessionAdapter-setOption)

- [`RSessionAdapter$importOption()`](#method-RSessionAdapter-importOption)

- [`RSessionAdapter$handle_BEL()`](#method-RSessionAdapter-handle_BEL)

- [`RSessionAdapter$handle_readline()`](#method-RSessionAdapter-handle_readline)

- [`RSessionAdapter$handle_scan()`](#method-RSessionAdapter-handle_scan)

- [`RSessionAdapter$clone()`](#method-RSessionAdapter-clone)

------------------------------------------------------------------------

### Method `aggreg_stdout()`

A potential "stdout_callback" function that aggregates output sent from
the R process via "stdout" channel to the eponymous "stdout" field

#### Usage

    RSessionAdapter$aggreg_stdout(txt, ...)

#### Arguments

- `txt`:

  A character string

- `...`:

  Other arguments, ignored

------------------------------------------------------------------------

### Method `aggreg_stderr()`

A potential "stderr_callback" function that aggregates output sent from
the R process via "stderr" channel to the eponymous "stderr" field

#### Usage

    RSessionAdapter$aggreg_stderr(txt, ...)

#### Arguments

- `txt`:

  A character string

- `...`:

  Other arguments, ignored

------------------------------------------------------------------------

### Method `collect()`

Collect the accumulated output from fields "stdout" and "stderr" into a
list with two elements named "stdout" and "stderr".

#### Usage

    RSessionAdapter$collect(clear = TRUE)

#### Arguments

- `clear`:

  A logical value, whether accumulated output should be cleared after
  being returned.

------------------------------------------------------------------------

### Method `new()`

Initialize an object

#### Usage

    RSessionAdapter$new(
      session,
      stdout_callback = self$aggreg_stdout,
      stderr_callback = self$aggreg_stderr,
      browser_callback = NULL,
      prompt_callback = NULL,
      input_callback = NULL,
      prompt = "> ",
      echo = FALSE
    )

#### Arguments

- `session`:

  An object from class "RKernelSession"

- `stdout_callback`:

  A callback function for "stdout" output

- `stderr_callback`:

  A callback function for "stderr" output

- `browser_callback`:

  A callback function for browser prompts (optional)

- `prompt_callback`:

  A callback function for command prompts encountered

- `input_callback`:

  A callback function for input requests (optional)

- `prompt`:

  A character string, the expected command prompt

- `echo`:

  A logical value, whether to echo input

------------------------------------------------------------------------

### Method `run_code()`

Run code and pass output to callback functions

#### Usage

    RSessionAdapter$run_code(
      code,
      io_timeout = 1,
      stdout_callback = self$stdout_callback,
      stderr_callback = self$stderr_callback,
      browser_callback = self$browser_callback,
      prompt_callback = self$prompt_callback,
      input_callback = self$input_callback,
      until_prompt = TRUE,
      echo = self$echo,
      debug = FALSE
    )

#### Arguments

- `code`:

  A character string or character vector with code lines

- `io_timeout`:

  An integer value, the timeout of waiting for output

- `stdout_callback`:

  A callback function for "stdout" output

- `stderr_callback`:

  A callback function for "stderr" output

- `browser_callback`:

  A callback function for browser prompts (optional)

- `prompt_callback`:

  A callback function for command prompts encountered

- `input_callback`:

  A callback function for input requests (optional)

- `until_prompt`:

  A logical value, whether process and wait output until a command
  prompt is encountered.

- `echo`:

  A logical value, whether to echo input

------------------------------------------------------------------------

### Method `interrupt()`

Send an interrupt signal (SIGINT) to the R process. This should stop
what the R process is doing without killing it.

#### Usage

    RSessionAdapter$interrupt()

------------------------------------------------------------------------

### Method `process_output()`

Process output created by commands sent to the R process

#### Usage

    RSessionAdapter$process_output(
      io_timeout = 1,
      stdout_callback = self$stdout_callback,
      stderr_callback = self$stderr_callback,
      browser_callback = self$browser_callback,
      input_callback = self$input_callback,
      prompt_callback = self$prompt_callback,
      drop_echo = FALSE,
      debug = FALSE
    )

#### Arguments

- `io_timeout`:

  An integer value, the timeout of waiting for output

- `stdout_callback`:

  A callback function for "stdout" output

- `stderr_callback`:

  A callback function for "stderr" output

- `browser_callback`:

  A callback function for browser prompts (optional)

- `input_callback`:

  A callback function for input requests (optional)

- `prompt_callback`:

  A callback function for command prompts encountered

- `drop_echo`:

  A logical value, whether input echo be dropped

- `until_prompt`:

  A logical value, whether process and wait output until a command
  prompt is encountered.

------------------------------------------------------------------------

### Method `run_cmd()`

Run a one-line command without checking and return the output

#### Usage

    RSessionAdapter$run_cmd(cmd, debug = FALSE)

#### Arguments

- `cmd`:

  A command string

------------------------------------------------------------------------

### Method [`getOption()`](https://rdrr.io/r/base/options.html)

Get an option value from the R session

#### Usage

    RSessionAdapter$getOption(n, default = NULL)

#### Arguments

- `n`:

  A character string, the name of the requested option value

- `default`:

  A default value

------------------------------------------------------------------------

### Method [`eval()`](https://rdrr.io/r/base/eval.html)

Evaluate an expression in the R session and return the result.

#### Usage

    RSessionAdapter$eval(expr, safe = FALSE)

#### Arguments

- `expr`:

  An expression

- `safe`:

  A logical value, whether errors should be caught

------------------------------------------------------------------------

### Method `eval_code()`

Evaluate some code in the R session and return the result.

#### Usage

    RSessionAdapter$eval_code(code, safe = FALSE)

#### Arguments

- `code`:

  A character string of code

- `safe`:

  A logical value, whether errors should be caught

------------------------------------------------------------------------

### Method [`ls()`](https://rdrr.io/r/base/ls.html)

Run \`ls()\` in the R session and return the result.

#### Usage

    RSessionAdapter$ls()

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get the value of a variable (named object) from the R session and return
it.

#### Usage

    RSessionAdapter$get(n)

#### Arguments

- `n`:

  The name of the variable

------------------------------------------------------------------------

### Method [`assign()`](https://rdrr.io/r/base/assign.html)

Assign a value to a variable in the R session

#### Usage

    RSessionAdapter$assign(n, value)

#### Arguments

- `n`:

  A variable name

- `value`:

  The value that is assigned to the variable

------------------------------------------------------------------------

### Method `setOption()`

Set an option in the R session

#### Usage

    RSessionAdapter$setOption(n, value)

#### Arguments

- `n`:

  The name of the option

- `value`:

  The intended option value

------------------------------------------------------------------------

### Method `importOption()`

Import an option value from the R session to the kernel

#### Usage

    RSessionAdapter$importOption(n)

#### Arguments

- `n`:

  The option value

------------------------------------------------------------------------

### Method `handle_BEL()`

Handle special output from the R session that starts with BEL

#### Usage

    RSessionAdapter$handle_BEL(
      txt,
      input_callback,
      stdout_callback,
      stderr_callback
    )

#### Arguments

- `txt`:

  Output string containing BEL

- `input_callback`:

  A function to request input from the frontend

- `stdout_callback`:

  A function to process output obtained from the R session via "stdout"
  channel

- `stderr_callback`:

  A function to process output obtained from the R session via "stderr"
  channel

------------------------------------------------------------------------

### Method `handle_readline()`

Handle special an input request obtained from the R session via output
indicated with a special output string

#### Usage

    RSessionAdapter$handle_readline(
      txt,
      input_callback,
      stdout_callback,
      stderr_callback
    )

#### Arguments

- `txt`:

  Output string containing a special readline prompt

- `input_callback`:

  A function to request input from the frontend

- `stdout_callback`:

  A function to process output obtained from the R session via "stdout"
  channel

- `stderr_callback`:

  A function to process output obtained from the R session via "stderr"
  channel

------------------------------------------------------------------------

### Method `handle_scan()`

Handle input request created by the function \`scan\` the in the R
session

#### Usage

    RSessionAdapter$handle_scan(
      txt,
      input_callback,
      stdout_callback,
      stderr_callback
    )

#### Arguments

- `txt`:

  Output string containing a special readline prompt

- `input_callback`:

  A function to request input from the frontend

- `stdout_callback`:

  A function to process output obtained from the R session via "stdout"
  channel

- `stderr_callback`:

  A function to process output obtained from the R session via "stderr"
  channel

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RSessionAdapter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
