import rsession
from rsession import RSession
import json
from traitlets import Any, Bool, HasTraits, Instance, List, Type, observe, observe_compat

class RKernelSession(RSession,HasTraits):
    banner = ''
    kernel = Instance("RKernel",allow_none=TRUE)

    def handle_stdout(self,text):
        self.kernel.stream(text,stream='stdout')

    def handle_stderr(self,text):
        self.kernel.stream(text,stream='stderr')
    

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

    session = Instance("RKernelSession",allow_none=True)
    
    @property
    def banner(self):
        if self.session:
            self.session.banner
        return None
    
    def __init__(self, **kwargs):
        """Initialize the kernel."""
        super().__init__(**kwargs)
        self.session = RKernelSession()

    def start(self):
        """Start the kernel."""
        super().start()
        self.session.start()
        self.session.banner = R.find_prompt()

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
        
    def do_execute(self, text, silent, store_history=True, user_expressions=None,
                   allow_stdin=False):

        return {'status': 'ok',
                # The base class increments the execution count
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {},
               }

    def clear_output(self):
        self.current_line = ''
    
