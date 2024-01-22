import json
import zmq
from ipykernel.kernelbase import Kernel
from .rsession import RSession
from .utils import *
from pprint import pprint, pformat
from threading import Thread
from termcolor import colored
from datetime import datetime

ETB = '\x17'
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

    def start(self):
        """Start the kernel."""
        self.rsession.start()
        self.banner += self.banner_suffix
        self.r_zmq_init() 
        self.r_zmq_setup_sender() 
        self.r_zmq_setup_receiver() 
        super().start()

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

    def display(self,data,metadata=dict(),transient=dict(),update=False):
        msg_type = 'update_display_data'
        if not update:
            msg_type = 'display_data'
            
        display_content = dict(
            data = data,
            metadata = metadata,
            transient = transient
        )
        self.send_response(self.iopub_socket, msg_type, display_content)
        
    def do_execute(self, code, silent, store_history=True, user_expressions=None,
                   allow_stdin=False):

        self.rsession.run(code)

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

    def handle_stdout(self,text):
        chunks = text.split(JSON_SEP)
        for chunk in chunks:
            if chunk.startswith(JSON_START) or chunk.endswith(JSON_END):
                if not chunk.startswith(JSON_START) or not chunk.endswith(JSON_END):
                    raise JSONerror
                chunk = chunk.lstrip(JSON_START).rstrip(JSON_END)
                display_data = json.loads(chunk)
                self.display(**display_data)
            else:
                self.stream(chunk,stream='stdout')

    def handle_stderr(self,text):
        self.stream(text,stream='stdout')

class JSONerror(Exception):
    pass
