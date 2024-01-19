import json
from ipykernel.kernelbase import Kernel

from traitlets import Any, Bool, HasTraits, Instance, List, Type, observe, observe_compat

from .rsession import RSession

from .utils import *

class RKernelSession(RSession,HasTraits):
    "Subclass of RSession to handle interaction"

    kernel = None

    def start(self):
        super().start()
        self.kernel.banner = self.find_prompt(timeout=1)
        self.cmd("attach(new.env(),name='tools:rsession')")
        
    def handle_stdout(self,text):
        self.kernel.stream(text,stream='stdout')

    def handle_stderr(self,text):
        self.kernel.stream(text,stream='stderr')

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
    
    def __init__(self, **kwargs):
        """Initialize the kernel."""
        super().__init__(**kwargs)
        self.rsession = RKernelSession()
        self.rsession.kernel = self

    def start(self):
        """Start the kernel."""
        self.rsession.start()
        super().start()
        self.rsession.source_env("complete.R")

    def stream(self,text,stream='stdout'):
            stream_content = {'name': stream, 'text': text}
            self.send_response(self.iopub_socket, 'stream', stream_content)

    def display(self,display_id,data,metadata=dict(),update=False):
        msg_type = 'update_display_data'
        if not update:
            msg_type = 'display_data'
            
        display_content = dict(
            data = data,
            metadata = metadata,
            transient = dict(display_id=display_id)
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

    def clear_output(self):
        self.current_line = ''
    
