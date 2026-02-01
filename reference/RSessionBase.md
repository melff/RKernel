# The R Session Base Class

An object of this class handles the lower-level communication with an R
process. Objects of class
[`RKernelSession`](https://melff.github.io/RKernel/reference/RKernelSession.md)
inherit from this class.

## Super classes

[`processx::process`](http://processx.r-lib.org/reference/process.md)
-\>
[`callr::r_session`](https://callr.r-lib.org/reference/r_session.html)
-\> `RSessionBase`

## Public fields

- `prompt`:

  The command prompt

- `banner`:

  The R startup message used as a session banner in the terminal and
  info box.

- `waiting`:

  A logical value, whether the R session is waiting for input.

## Methods

### Public methods

- [`RSessionBase$new()`](#method-RSessionBase-new)

- [`RSessionBase$sleeping()`](#method-RSessionBase-sleeping)

- [`RSessionBase$send_input()`](#method-RSessionBase-send_input)

- [`RSessionBase$read_output()`](#method-RSessionBase-read_output)

- [`RSessionBase$receive_output()`](#method-RSessionBase-receive_output)

- [`RSessionBase$receive_all_output()`](#method-RSessionBase-receive_all_output)

- [`RSessionBase$send_receive()`](#method-RSessionBase-send_receive)

- [`RSessionBase$clone()`](#method-RSessionBase-clone)

Inherited methods

- [`processx::process$as_ps_handle()`](http://processx.r-lib.org/reference/process.html#method-as_ps_handle)
- [`processx::process$format()`](http://processx.r-lib.org/reference/process.html#method-format)
- [`processx::process$get_cmdline()`](http://processx.r-lib.org/reference/process.html#method-get_cmdline)
- [`processx::process$get_cpu_times()`](http://processx.r-lib.org/reference/process.html#method-get_cpu_times)
- [`processx::process$get_error_connection()`](http://processx.r-lib.org/reference/process.html#method-get_error_connection)
- [`processx::process$get_error_file()`](http://processx.r-lib.org/reference/process.html#method-get_error_file)
- [`processx::process$get_exe()`](http://processx.r-lib.org/reference/process.html#method-get_exe)
- [`processx::process$get_exit_status()`](http://processx.r-lib.org/reference/process.html#method-get_exit_status)
- [`processx::process$get_input_connection()`](http://processx.r-lib.org/reference/process.html#method-get_input_connection)
- [`processx::process$get_input_file()`](http://processx.r-lib.org/reference/process.html#method-get_input_file)
- [`processx::process$get_memory_info()`](http://processx.r-lib.org/reference/process.html#method-get_memory_info)
- [`processx::process$get_name()`](http://processx.r-lib.org/reference/process.html#method-get_name)
- [`processx::process$get_output_connection()`](http://processx.r-lib.org/reference/process.html#method-get_output_connection)
- [`processx::process$get_output_file()`](http://processx.r-lib.org/reference/process.html#method-get_output_file)
- [`processx::process$get_pid()`](http://processx.r-lib.org/reference/process.html#method-get_pid)
- [`processx::process$get_poll_connection()`](http://processx.r-lib.org/reference/process.html#method-get_poll_connection)
- [`processx::process$get_result()`](http://processx.r-lib.org/reference/process.html#method-get_result)
- [`processx::process$get_start_time()`](http://processx.r-lib.org/reference/process.html#method-get_start_time)
- [`processx::process$get_status()`](http://processx.r-lib.org/reference/process.html#method-get_status)
- [`processx::process$get_username()`](http://processx.r-lib.org/reference/process.html#method-get_username)
- [`processx::process$get_wd()`](http://processx.r-lib.org/reference/process.html#method-get_wd)
- [`processx::process$has_error_connection()`](http://processx.r-lib.org/reference/process.html#method-has_error_connection)
- [`processx::process$has_input_connection()`](http://processx.r-lib.org/reference/process.html#method-has_input_connection)
- [`processx::process$has_output_connection()`](http://processx.r-lib.org/reference/process.html#method-has_output_connection)
- [`processx::process$has_poll_connection()`](http://processx.r-lib.org/reference/process.html#method-has_poll_connection)
- [`processx::process$interrupt()`](http://processx.r-lib.org/reference/process.html#method-interrupt)
- [`processx::process$is_alive()`](http://processx.r-lib.org/reference/process.html#method-is_alive)
- [`processx::process$is_incomplete_error()`](http://processx.r-lib.org/reference/process.html#method-is_incomplete_error)
- [`processx::process$is_incomplete_output()`](http://processx.r-lib.org/reference/process.html#method-is_incomplete_output)
- [`processx::process$is_supervised()`](http://processx.r-lib.org/reference/process.html#method-is_supervised)
- [`processx::process$kill()`](http://processx.r-lib.org/reference/process.html#method-kill)
- [`processx::process$kill_tree()`](http://processx.r-lib.org/reference/process.html#method-kill_tree)
- [`processx::process$poll_io()`](http://processx.r-lib.org/reference/process.html#method-poll_io)
- [`processx::process$read_all_error()`](http://processx.r-lib.org/reference/process.html#method-read_all_error)
- [`processx::process$read_all_error_lines()`](http://processx.r-lib.org/reference/process.html#method-read_all_error_lines)
- [`processx::process$read_all_output()`](http://processx.r-lib.org/reference/process.html#method-read_all_output)
- [`processx::process$read_all_output_lines()`](http://processx.r-lib.org/reference/process.html#method-read_all_output_lines)
- [`processx::process$read_error()`](http://processx.r-lib.org/reference/process.html#method-read_error)
- [`processx::process$read_error_lines()`](http://processx.r-lib.org/reference/process.html#method-read_error_lines)
- [`processx::process$read_output_lines()`](http://processx.r-lib.org/reference/process.html#method-read_output_lines)
- [`processx::process$resume()`](http://processx.r-lib.org/reference/process.html#method-resume)
- [`processx::process$signal()`](http://processx.r-lib.org/reference/process.html#method-signal)
- [`processx::process$supervise()`](http://processx.r-lib.org/reference/process.html#method-supervise)
- [`processx::process$suspend()`](http://processx.r-lib.org/reference/process.html#method-suspend)
- [`processx::process$wait()`](http://processx.r-lib.org/reference/process.html#method-wait)
- [`processx::process$write_input()`](http://processx.r-lib.org/reference/process.html#method-write_input)
- [`callr::r_session$attach()`](https://callr.r-lib.org/reference/r_session.html#method-attach)
- [`callr::r_session$call()`](https://callr.r-lib.org/reference/r_session.html#method-call)
- [`callr::r_session$close()`](https://callr.r-lib.org/reference/r_session.html#method-close)
- [`callr::r_session$debug()`](https://callr.r-lib.org/reference/r_session.html#method-debug)
- [`callr::r_session$finalize()`](https://callr.r-lib.org/reference/r_session.html#method-finalize)
- [`callr::r_session$get_running_time()`](https://callr.r-lib.org/reference/r_session.html#method-get_running_time)
- [`callr::r_session$get_state()`](https://callr.r-lib.org/reference/r_session.html#method-get_state)
- [`callr::r_session$poll_process()`](https://callr.r-lib.org/reference/r_session.html#method-poll_process)
- [`callr::r_session$print()`](https://callr.r-lib.org/reference/r_session.html#method-print)
- [`callr::r_session$read()`](https://callr.r-lib.org/reference/r_session.html#method-read)
- [`callr::r_session$run()`](https://callr.r-lib.org/reference/r_session.html#method-run)
- [`callr::r_session$run_with_output()`](https://callr.r-lib.org/reference/r_session.html#method-run_with_output)
- [`callr::r_session$traceback()`](https://callr.r-lib.org/reference/r_session.html#method-traceback)

------------------------------------------------------------------------

### Method `new()`

Initialize the object and start the session

#### Usage

    RSessionBase$new(
      options = r_session_options(stdout = "|", stderr = "|", cmdargs = c("--interactive",
        "--no-readline", "--no-save", "--no-restore"), env = c(R_CLI_NUM_COLORS =
        "16777216")),
      prompt = "> "
    )

#### Arguments

- `options`:

  R session objects, see
  [`r_session_options`](https://callr.r-lib.org/reference/r_session_options.html).

- `prompt`:

  The expected prompt string of the R session

- `env`:

  A character vector with environment variables for the R process

------------------------------------------------------------------------

### Method `sleeping()`

Returns a logical value, indicating whether the R process is sleeping.

#### Usage

    RSessionBase$sleeping()

------------------------------------------------------------------------

### Method `send_input()`

Send input text to the R process

#### Usage

    RSessionBase$send_input(text, drop_echo = FALSE)

#### Arguments

- `text`:

  A character string

- `drop_echo`:

  A logical value, whether to drop the echo from stdout.

------------------------------------------------------------------------

### Method `read_output()`

Read output from the R session and drop input echo if so requested

#### Usage

    RSessionBase$read_output(n = -1)

#### Arguments

- `n`:

  The number of characters to read

------------------------------------------------------------------------

### Method `receive_output()`

Poll R process for output and read it

#### Usage

    RSessionBase$receive_output(timeout = 1)

#### Arguments

- `timeout`:

  A number, the polling timeout in microseconds

------------------------------------------------------------------------

### Method `receive_all_output()`

Receive all output that is available

#### Usage

    RSessionBase$receive_all_output(timeout = 1)

#### Arguments

- `timeout`:

  A number, the polling timeout in microseconds

------------------------------------------------------------------------

### Method `send_receive()`

Send text to R process and receive all output from the process

#### Usage

    RSessionBase$send_receive(text, timeout = 100)

#### Arguments

- `text`:

  A character string

- `timeout`:

  An integer number, the polling timeout

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RSessionBase$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
