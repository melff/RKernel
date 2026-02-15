# The KernelSession Class

An object of this class handles the communication with an R process.
There is usually one main session, but there may also sessions for
detached code cells.

## Super classes

[`processx::process`](http://processx.r-lib.org/reference/process.md)
-\>
[`callr::r_session`](https://callr.r-lib.org/reference/r_session.html)
-\>
[`RKernel::RSessionBase`](https://melff.github.io/RKernel/reference/RSessionBase.md)
-\> `RKernelSession`

## Public fields

- `http_port`:

  The port number of HTML help.

- `hostname`:

  The hostname.

- `yield`:

  An optional function to service kernel requests

- `kernel`:

  The reference to the controlling kernel object

## Methods

### Public methods

- [`RKernelSession$connect()`](#method-RKernelSession-connect)

- [`RKernelSession$setup()`](#method-RKernelSession-setup)

- [`RKernelSession$start_graphics()`](#method-RKernelSession-start_graphics)

- [`RKernelSession$dev_new()`](#method-RKernelSession-dev_new)

- [`RKernelSession$graphics_details()`](#method-RKernelSession-graphics_details)

- [`RKernelSession$http_get()`](#method-RKernelSession-http_get)

- [`RKernelSession$clone()`](#method-RKernelSession-clone)

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
- [`RKernel::RSessionBase$initialize()`](https://melff.github.io/RKernel/reference/RSessionBase.html#method-initialize)
- [`RKernel::RSessionBase$read_output()`](https://melff.github.io/RKernel/reference/RSessionBase.html#method-read_output)
- [`RKernel::RSessionBase$receive_all_output()`](https://melff.github.io/RKernel/reference/RSessionBase.html#method-receive_all_output)
- [`RKernel::RSessionBase$receive_output()`](https://melff.github.io/RKernel/reference/RSessionBase.html#method-receive_output)
- [`RKernel::RSessionBase$send_input()`](https://melff.github.io/RKernel/reference/RSessionBase.html#method-send_input)
- [`RKernel::RSessionBase$send_receive()`](https://melff.github.io/RKernel/reference/RSessionBase.html#method-send_receive)
- [`RKernel::RSessionBase$sleeping()`](https://melff.github.io/RKernel/reference/RSessionBase.html#method-sleeping)

------------------------------------------------------------------------

### Method `connect()`

Connect the session with the main Kernel object

#### Usage

    RKernelSession$connect(yield, kernel)

#### Arguments

- `yield`:

  The function that is called when the session objects yields back
  control to the kernel object, for example in order to wait for comm
  message when a widget is active.

- `kernel`:

  The main kernel objectd

------------------------------------------------------------------------

### Method `setup()`

Set up the R session, by installing hooks etc.

#### Usage

    RKernelSession$setup()

------------------------------------------------------------------------

### Method [`start_graphics()`](https://melff.github.io/RKernel/reference/start_graphics.md)

Initialize graphics, start device and return details

#### Usage

    RKernelSession$start_graphics()

------------------------------------------------------------------------

### Method `dev_new()`

Start a httpgd device and return the graphics details.

#### Usage

    RKernelSession$dev_new()

------------------------------------------------------------------------

### Method `graphics_details()`

Return the graphics details.

#### Usage

    RKernelSession$graphics_details()

------------------------------------------------------------------------

### Method [`http_get()`](https://melff.github.io/RKernel/reference/http_get.md)

#### Usage

    RKernelSession$http_get(slug = "", query = "")

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RKernelSession$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
