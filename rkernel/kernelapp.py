# Restricted variant of IPKernelApp, which does not rely on python debugger

import zmq
from zmq.eventloop.zmqstream import ZMQStream


from ipykernel.kernelapp import IPKernelApp

from ipykernel.control import ControlThread


class KernelApp(IPKernelApp):
    def init_control(self, context):
        """Initialize the control channel."""
        self.control_socket = context.socket(zmq.ROUTER)
        self.control_socket.linger = 1000
        self.control_port = self._bind_socket(self.control_socket, self.control_port)
        self.log.debug("control ROUTER Channel on port: %i" % self.control_port)

        self.debug_shell_socket = context.socket(zmq.DEALER)
        self.debug_shell_socket.linger = 1000
        if self.shell_socket.getsockopt(zmq.LAST_ENDPOINT):
            self.debug_shell_socket.connect(self.shell_socket.getsockopt(zmq.LAST_ENDPOINT))

        if hasattr(zmq, "ROUTER_HANDOVER"):
            # set router-handover to workaround zeromq reconnect problems
            # in certain rare circumstances
            # see ipython/ipykernel#270 and zeromq/libzmq#2892
            self.control_socket.router_handover = 1

        self.control_thread = ControlThread(daemon=True)


    def init_kernel(self):
        """Create the Kernel object itself"""
        shell_stream = ZMQStream(self.shell_socket)
        control_stream = ZMQStream(self.control_socket, self.control_thread.io_loop)
        self.control_thread.start()
        kernel_factory = self.kernel_class.instance  # type:ignore[attr-defined]

        kernel = kernel_factory(
            parent=self,
            session=self.session,
            control_stream=control_stream,
            debug_shell_socket=self.debug_shell_socket,
            shell_stream=shell_stream,
            control_thread=self.control_thread,
            iopub_thread=self.iopub_thread,
            iopub_socket=self.iopub_socket,
            stdin_socket=self.stdin_socket,
            log=self.log,
            profile_dir=self.profile_dir,
            user_ns=self.user_ns,
        )
        kernel.record_ports({name + "_port": port for name, port in self._ports.items()})
        self.kernel = kernel

        # Allow the displayhook to get the execution count
        self.displayhook.get_execution_count = lambda: kernel.execution_count
