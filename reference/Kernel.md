# The Kernel Class

An object of this class handles the low-level communication with the
Jupyter frontend or kernel manager. There should only be one object of
this class in existence.

## Public fields

- `session`:

  See
  [`RKernelSession`](https://melff.github.io/RKernel/reference/RKernelSession.md).

- `repl`:

  An RSessionAdapter for handling input and output

- `runner`:

  An RSessionRunner

- `DAPServer`:

  The current DAP server

- `errored`:

  Logical, whether an error occurred in the R session

- `stop_on_error`:

  Logical, whether running a notebook should be stopped in case of an
  error in the R session.

## Methods

### Public methods

- [`Kernel$new()`](#method-Kernel-new)

- [`Kernel$start()`](#method-Kernel-start)

- [`Kernel$run_code()`](#method-Kernel-run_code)

- [`Kernel$run()`](#method-Kernel-run)

- [`Kernel$poll_and_respond()`](#method-Kernel-poll_and_respond)

- [`Kernel$clear_output()`](#method-Kernel-clear_output)

- [`Kernel$stream()`](#method-Kernel-stream)

- [`Kernel$stdout()`](#method-Kernel-stdout)

- [`Kernel$stderr()`](#method-Kernel-stderr)

- [`Kernel$readline()`](#method-Kernel-readline)

- [`Kernel$execute_result()`](#method-Kernel-execute_result)

- [`Kernel$display_send()`](#method-Kernel-display_send)

- [`Kernel$send_error()`](#method-Kernel-send_error)

- [`Kernel$send_comm()`](#method-Kernel-send_comm)

- [`Kernel$get_parent()`](#method-Kernel-get_parent)

- [`Kernel$get_conn_info()`](#method-Kernel-get_conn_info)

- [`Kernel$is_child()`](#method-Kernel-is_child)

- [`Kernel$input_request()`](#method-Kernel-input_request)

- [`Kernel$read_stdin()`](#method-Kernel-read_stdin)

- [`Kernel$send_debug_event()`](#method-Kernel-send_debug_event)

- [`Kernel$save_shell_parent()`](#method-Kernel-save_shell_parent)

- [`Kernel$restore_shell_parent()`](#method-Kernel-restore_shell_parent)

- [`Kernel$shutdown()`](#method-Kernel-shutdown)

- [`Kernel$restart()`](#method-Kernel-restart)

- [`Kernel$restore_execute_parent()`](#method-Kernel-restore_execute_parent)

- [`Kernel$handle_yield()`](#method-Kernel-handle_yield)

- [`Kernel$add_service()`](#method-Kernel-add_service)

- [`Kernel$clone()`](#method-Kernel-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the kernel

#### Usage

    Kernel$new(conn_info)

#### Arguments

- `conn_info`:

  A list with the connection info from the front-end

------------------------------------------------------------------------

### Method [`start()`](https://rdrr.io/r/stats/start.html)

Start the R session and other components

#### Usage

    Kernel$start()

------------------------------------------------------------------------

### Method `run_code()`

Run some code (for testing purposes)

#### Usage

    Kernel$run_code(code, debug = FALSE)

#### Arguments

- `code`:

  Some code

- `debug`:

  Logical, whether running a code cell should occur under debugging
  conditions

------------------------------------------------------------------------

### Method `run()`

Run the kernel.

#### Usage

    Kernel$run()

------------------------------------------------------------------------

### Method `poll_and_respond()`

A single iteration of the kernel loop

#### Usage

    Kernel$poll_and_respond(
      poll_timeout = getOption("rkernel_poll_timeout", 10L),
      drop = NULL
    )

#### Arguments

- `poll_timeout`:

  An integer

- `drop`:

  Logical, whether execute requests should be dropped

------------------------------------------------------------------------

### Method `clear_output()`

Clear the current output cell in the frontend.

#### Usage

    Kernel$clear_output(wait)

#### Arguments

- `wait`:

  Logical value, whether to wait until output is cleared.

------------------------------------------------------------------------

### Method `stream()`

Stream text to the frontend.

#### Usage

    Kernel$stream(text, stream)

#### Arguments

- `text`:

  Text to be sent to the frontend

- `stream`:

  A string to select the stream – either "stout" or "stderr"

------------------------------------------------------------------------

### Method [`stdout()`](https://rdrr.io/r/base/showConnections.html)

Stream text to the frontend via 'stdout' stream.

#### Usage

    Kernel$stdout(text)

#### Arguments

- `text`:

  Text to be sent to the frontend.

------------------------------------------------------------------------

### Method [`stderr()`](https://rdrr.io/r/base/showConnections.html)

Stream text to the frontend via 'stderr' stream.

#### Usage

    Kernel$stderr(text)

#### Arguments

- `text`:

  Text to be sent to the frontend.

------------------------------------------------------------------------

### Method [`readline()`](https://rdrr.io/r/base/readline.html)

Ask the frontend for a line of text input.

#### Usage

    Kernel$readline(prompt = "")

#### Arguments

- `prompt`:

  The prompt, a character string.

------------------------------------------------------------------------

### Method `execute_result()`

Send execution results to the frontend

#### Usage

    Kernel$execute_result(data, metadata = emptyNamedList)

#### Arguments

- `data`:

  Execution result in rich format

- `metadata`:

  A list with metadata

------------------------------------------------------------------------

### Method `display_send()`

Send rich format data to the frontend

#### Usage

    Kernel$display_send(msg)

#### Arguments

- `msg`:

  A list with the appropriate structure. \[TODO\]

------------------------------------------------------------------------

### Method `send_error()`

Send an error message and traceback to the frontend.

#### Usage

    Kernel$send_error(name, value, traceback)

#### Arguments

- `name`:

  A string, the error name.

- `value`:

  A string, the value of the error message.

- `traceback`:

  A character vector with the traceback.

------------------------------------------------------------------------

### Method `send_comm()`

Send a message via a comm.

#### Usage

    Kernel$send_comm(msg)

#### Arguments

- `msg`:

  A list containing a comm message.

------------------------------------------------------------------------

### Method `get_parent()`

The parent of the message currently sent.

#### Usage

    Kernel$get_parent(channel = "shell")

#### Arguments

- `channel`:

  A string, the relevant input channel.

------------------------------------------------------------------------

### Method `get_conn_info()`

Return the current connection info.

#### Usage

    Kernel$get_conn_info()

------------------------------------------------------------------------

### Method `is_child()`

Check if the current process is a fork from the original kernel process

#### Usage

    Kernel$is_child()

------------------------------------------------------------------------

### Method `input_request()`

Send an input request to the frontend

#### Usage

    Kernel$input_request(prompt = "", password = FALSE)

#### Arguments

- `prompt`:

  A prompt string

- `password`:

  Logical value; whether the input should be hidden like in a password
  dialog

------------------------------------------------------------------------

### Method `read_stdin()`

Read a line from the frontend

#### Usage

    Kernel$read_stdin()

------------------------------------------------------------------------

### Method `send_debug_event()`

Send a debug event to the frontend

#### Usage

    Kernel$send_debug_event(content)

#### Arguments

- `content`:

  A list, content provided by the debug adapter

------------------------------------------------------------------------

### Method `save_shell_parent()`

Save the parent message on the shell channel

#### Usage

    Kernel$save_shell_parent()

------------------------------------------------------------------------

### Method `restore_shell_parent()`

Restore a saved parent message on the shell channel.

#### Usage

    Kernel$restore_shell_parent(saved_parent)

#### Arguments

- `saved_parent`:

  A saved parent message, a list.

------------------------------------------------------------------------

### Method `shutdown()`

Shut down the kernel and all associated sub-process i.e. the R session
and all detached-cell processes.

#### Usage

    Kernel$shutdown()

------------------------------------------------------------------------

### Method `restart()`

Restart the R session

#### Usage

    Kernel$restart()

------------------------------------------------------------------------

### Method `restore_execute_parent()`

Restore parent message of last execute_request.

#### Usage

    Kernel$restore_execute_parent()

------------------------------------------------------------------------

### Method `handle_yield()`

Handle R session's yielding to allow servicing frontend requests

#### Usage

    Kernel$handle_yield(timeout)

#### Arguments

- `timeout`:

  Number of milliseconds to wait for a frontend request

------------------------------------------------------------------------

### Method `add_service()`

Add a function as a service to be run each iteration of the kernel loop.

#### Usage

    Kernel$add_service(FUN)

#### Arguments

- `FUN`:

  A function.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Kernel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
