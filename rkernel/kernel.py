import os
import json
import zmq
from ipykernel.kernelbase import Kernel
from .rsession import RSession
from .utils import *
from pprint import pprint, pformat
from threading import Thread
from termcolor import colored
from datetime import datetime

DLE = '\x10'
DISPLAY_START = '[!display]'

class RKernelSession(RSession):
    "Subclass of RSession to handle interaction"

    kernel = None

    def start(self):
        super().start()
        self.kernel.banner += self.find_prompt(timeout=1)
        
    def handle_stdout(self,text):
        self.kernel.handle_stdout(text)

    def handle_stderr(self,text):
        self.kernel.handle_stderr(text)

    def source(self,filename):
        path = os.path.join(R_files_path(),filename)
        code = "source('%s')" % path
        return self.cmd(code)

    def source_env(self,filename):
        path = os.path.join(R_files_path(),filename)
        code = "source('%s',local=as.environment('tools:rsession'))" % path
        return self.cmd(code)
        
class RKernel(Kernel):

    implementation = 'RKernel-py'
    implementation_version = '1.0'
    language = 'R'
    language_version = '4.3'
    language_info = {
        'name': 'R',
        'mimetype': 'text/x-r-source',
        'file_extension': '.R',
    }

    rsession = None
    banner = ''

    banner_suffix = ''
    
    debug = False

    log_file = None
    
    def __init__(self, **kwargs):
        """Initialize the kernel."""
        super().__init__(**kwargs)
        self.rsession = RKernelSession()
        self.rsession.kernel = self

        if "VIRTUAL_ENV" in os.environ:
            VIRTUAL_ENV = os.getenv("VIRTUAL_ENV")
            self.banner += "VIRTUAL_ENV = %s\n" % VIRTUAL_ENV
            R_LIBS_USER = os.path.join(VIRTUAL_ENV,"lib","R","library")
        elif "JUPYTER_CONFIG_DIR" in os.environ:
            JUPYTER_CONFIG_DIR = os.getenv("JUPYTER_CONFIG_DIR")
            R_LIBS_USER = os.path.join(JUPYTER_CONFIG_DIR,"R","library")
        else:
            JUPYTER_CONFIG_DIR = os.path.join(os.path.expanduser("~"),".jupyter")
            R_LIBS_USER = os.path.join(JUPYTER_CONFIG_DIR,"R","library")
        if not os.path.isdir(R_LIBS_USER):
            os.makedirs(R_LIBS_USER)
        self.rsession.env["R_LIBS_USER"] = R_LIBS_USER
        self.banner_suffix += "User-installed packages are in '%s'\n" % R_LIBS_USER

        self.log_file = open("/tmp/RKernel.log","a")

        # self.rsession.env["_R_HELP_ENABLE_ENHANCED_HTML_"] = "FALSE"
        
    def start(self):
        """Start the kernel."""
        # self.log_out("======== Starting kernel ========")
        self.rsession.start()
        self.banner += self.banner_suffix
        self.r_zmq_init() 
        self.r_zmq_setup_sender() 
        self.r_zmq_setup_receiver()
        self.r_install_hooks()
        self.r_start_graphics()
        self.r_set_help_port()
        self.r_set_help_displayed()
        super().start()
        # self.log_out("Kernel started")

    def stream(self,text,stream='stdout'):
        if not self.debug:
            stream_content = {'name': stream, 'text': text}
            self.send_response(self.iopub_socket, 'stream', stream_content)
        else:
            if stream=='stdout':
                text = colored(text,self.rsession.stdout_color)
            elif stream=='stderr':
                text = colored(text,self.rsession.stderr_color)
            print(text,end='')

    def display(self,display_request):
        msg_type = display_request['type']    
        display_content = display_request['content']
        self.send_response(self.iopub_socket, msg_type, display_content)
        
    def do_execute(self, code, silent, store_history=True, user_expressions=None,
                   allow_stdin=False):

        # self.log_out("do_execute")

        self.r_run_cell_begin_hooks()
        self.rsession.run(code)
        self.r_run_cell_end_hooks()

        # self.log_out("run complete - sending reply")
        
        return {'status': 'ok',
                # The base class increments the execution count
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {},
               }

    def do_is_complete(self, code):
        content = dict(code = code)
        msg = dict(type = 'is_complete_request', content = content)
        response = self.r_zmq_request(msg)
        return {"status": response['status'],
                "indent": ""}

    def r_zmq_init(self):
        # print("r_zmq_init")
        res = self.rsession.cmd("RKernel::zmq_init()")
        self.zmq_context = zmq.Context()

    def r_zmq_setup_sender(self):
        port = random_port()
        res = self.rsession.cmd("RKernel::zmq_new_receiver(%d)" % port)
        self.r_zmq_send_port = port
        context = self.zmq_context
        socket = context.socket(zmq.PUSH)
        url = "tcp://*:%d" % port
        socket.bind(url)
        self.r_zmq_send_so = socket

    def r_zmq_setup_receiver(self):
        port = random_port()
        res = self.rsession.cmd("RKernel::zmq_new_sender(%d)" % port)
        self.r_zmq_recv_port = port
        context = self.zmq_context
        socket = context.socket(zmq.PULL)
        url = "tcp://*:%d" % port
        socket.bind(url)
        self.r_zmq_recv_so = socket

    def r_zmq_request(self,req):
        # print("r_zmq_request")
        res = self.rsession.cmd_nowait("RKernel::zmq_reply()")
        self.r_zmq_send(req)
        resp = self.r_zmq_receive()
        self.rsession.find_prompt()
        return resp

    def r_zmq_send(self,msg):
        # print("r_zmq_send")
        msg = json.dumps(msg,separators=(',', ':')).encode("utf-8")
        self.r_zmq_send_so.send_multipart([msg])

    def r_zmq_receive(self):
        # print("r_zmq_receive")
        req = self.r_zmq_recv_so.recv_multipart()
        req = json.loads(req[0].decode("utf-8"))
        return req

    def r_zmq_flush(self):
        while self.r_zmq_poll(timeout=1):
            self.r_zmq_recv_so.recv_multipart()

    def r_zmq_poll(self,timeout=1):
        msk = self.r_zmq_recv_so.poll(timeout=timeout)
        return msk != 0

    def r_start_graphics(self):
        self.rsession.cmd_nowait("RKernel::start_graphics()")

    def r_run_cell_begin_hooks(self):
        self.rsession.run("RKernel::runHooks('cell-begin')")

    def r_run_cell_end_hooks(self):
        self.rsession.run("RKernel::runHooks('cell-end')")

    def handle_stdout(self,text):
        # self.log_out("handle_stdout")
        if DLE in text:
            chunks = text.split(DLE)
            for chunk in chunks:
                if chunk.startswith(DISPLAY_START):
                    resp = self.r_zmq_receive()
                    self.display(resp)
                else:
                    if len(chunk) > 0:
                        self.stream(chunk,stream='stdout')
        else:
            if len(text) > 0:
                self.stream(text,stream='stdout')

        # self.log_out("handle_stdout - done")

    def handle_stderr(self,text):
        self.stream(text,stream='stderr')

    def log_out(self,text):
        now = format(datetime.now())
        text = 'KERNEL: ' + now + ' ' + text
        self.log_file.write(text + '\n')
        self.log_file.flush()

    async def interrupt_request(self, stream, ident, parent):
        """Handle an interrupt request."""
        # self.log_out("interrupt_request received")
        if not self.session:
            return
        content: dict[str, t.Any] = {"status": "ok"}
        # self.log_out("trying to interrupt session")
        try:
            self.rsession.interrupt()
        except OSError as err:
            import traceback
        
            content = {
                "status": "error",
                "traceback": traceback.format_stack(),
                "ename": str(type(err).__name__),
                "evalue": str(err),
            }
        # self.log_out("Setting self.rsession.interrupted = True")
        # self.log_out("interrupt_request completed")
        self.session.send(stream, "interrupt_reply", content, parent, ident=ident)
        return

    def r_install_hooks(self):
        self.rsession.cmd("RKernel::install_output_hooks()")

    def r_set_help_port(self):
        port = random_port()
        self.rsession.cmd("RKernel::set_help_port(%d)" % port)

    def r_set_help_displayed(self):
        self.rsession.cmd("RKernel::set_help_displayed(TRUE)")
        
class JSONerror(Exception):
    pass
