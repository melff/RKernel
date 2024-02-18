import os
import json
import zmq
from ipykernel.kernelbase import Kernel
from .rsession import RSession
from .utils import *
from pprint import pprint, pformat
from threading import Thread, Lock, Event
from queue import Queue, Empty
from termcolor import colored
from datetime import datetime
import shutil

EOT = '\x04'
DLE = '\x10'
ETB = '\x17'
ZMQ_PUSH = '[!ZMQ_PUSH]'
JSON_MSG = '[!JSON]'
CMD_PROMPT = '> '

class RKernelSession(RSession):
    "Subclass of RSession to handle interaction"

    kernel = None

    temp_dir = ''

    def start(self):
        super().start()
        session_banner = self.find_prompt()
        self.kernel.banner += ''.join(session_banner)
        self.temp_dir = self.cmd("cat(tempdir())")[0]
        
    def handle_stdout(self,text):
        self.kernel.handle_stdout(text)

    def handle_stderr(self,text):
        self.kernel.handle_stderr(text)

    def input(self,text):
        try:
            inp = self.kernel.raw_input(paste0(text))
        except EOFError:
            inp = ''
        return inp
        
    def quit(self):
        self.flush(stream='stdout')
        self.flush(stream='stderr')
        self.sendline("RKernel::q_orig()")
        self.proc.wait()


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

    r_handlers = dict()

    dbg_handlers = dict()

    zmq_timeout = 0.1

    _allow_stdin = True

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
        self.rsession.env["R_CLI_NUM_COLORS"] = "16777216"
        
        self.banner_suffix += "User-installed packages are in '%s'\n" % R_LIBS_USER

        self.log_file = open("/tmp/RKernel.log","a")

        comm_msg_types = ["comm_open", "comm_msg", "comm_close"]
        for msg_type in comm_msg_types:
            self.shell_handlers[msg_type] = getattr(self, msg_type)

        self.r_handlers['display_data'] = getattr(self,'display')
        self.r_handlers['update_display_data'] = getattr(self,'display')

        self.r_zmq_watcher_enabled = Event()

        self.dbg_handlers['debugInfo']  = self.debugInfo
        self.dbg_handlers['initialize'] = self.debug_init
        self.dbg_handlers['attach']     = self.debug_attach
        self.dbg_handlers['disconnect'] = self.debug_disconnect
        self.dbg_handlers['inspectVariables'] = self.debug_passthru
        self.dbg_handlers['richInspectVariables'] = self.debug_passthru

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
        msg_content = display_request['content']
        self.send_response(self.iopub_socket, 
                           msg_type, 
                           content = msg_content)
        
    def handle_comm(self,msg):
        # self.log_out("handle_comm")
        # self.log_out(pformat(msg))
        msg_type = msg['type']    
        msg_content = msg['content']
        if 'metadata' in msg:
            metadata = msg['metadata']
        else:
            metadata = dict()
        # self.stream(pformat(msg),stream='stderr')
        if 'buffers' in msg:
            msg_buffers = msg['buffers']
            self.send_response(self.iopub_socket, 
                               msg_type, 
                               content = msg_content,
                               buffers = msg_buffers,
                               metadata = metadata)
        else:
            self.send_response(self.iopub_socket, 
                               msg_type, 
                               content = msg_content,
                               metadata = metadata)
        # self.log_out("done")
            
    def do_execute(self, code, silent, store_history=True, user_expressions=None,
                   allow_stdin=False):

        # self.log_out("======== do_execute ===================")
        # self.log_out(code)

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
        return response['content']

    
    def do_complete(self, code, cursor_pos):
        content = dict(code = code,
                       cursor_pos = cursor_pos)
        msg = dict(type = 'complete_request',
                   content = content)
        response = self.r_zmq_request(msg)
        return response['content']
    
    def do_inspect(self, code, cursor_pos, detail_level=0, omit_sections=()):
        content = dict(code = code,
                       cursor_pos = cursor_pos,
                       detail_level = detail_level)
        msg = dict(type = 'inspect_request',
                   content = content)
        response = self.r_zmq_request(msg)
        return response['content']
        
    def do_shutdown(self, restart):
        self.rsession.quit()
        return {"status": "ok", "restart": restart}

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

    def r_zmq_request(self,req):
        # self.log_out("r_zmq_request")
        # self.log_out(pformat(req))
        # self.r_zmq_watcher_enabled.clear()
        res = self.rsession.cmd_nowait("RKernel::zmq_request()")
        self.r_zmq_send(req)
        # self.log_out("message sent")
        # self.log_out(pformat(req))
        resp = self.r_zmq_receive(timeout = self.zmq_timeout)
        # self.log_out("response received")
        # self.r_zmq_watcher_enabled.set()
        # self.log_out(pformat(resp))
        self.rsession.find_prompt()
        # self.log_out("done")
        return resp

    def r_zmq_request_noreply(self,req):
        # self.log_out("r_zmq_request_noreply")
        # self.log_out(pformat(req))
        self.rsession.cmd_nowait("RKernel::zmq_request_noreply()")
        # self.log_out(pformat(res))
        self.r_zmq_send(req)
        self.rsession.find_prompt()
        # self.log_out("done")

    def r_zmq_send(self,msg):
        # self.log_out("r_zmq_send")
        msg = json.dumps(msg,separators=(',', ':')).encode("utf-8")
        self.r_zmq_send_so.send_multipart([msg])
        # self.log_out("done")

    def r_zmq_receive(self,timeout = None):
        msg = self.zmq_reply_q.get(timeout = timeout)
        return msg

    zmq_reply_q = None
    zmq_comm_q = None
    zmq_debug_q = None
    zmq_display_q = None

    r_zmq_watcher = None
    r_comm_thread = None
    r_debug_thread = None

    def r_zmq_setup_receiver(self):

        port = random_port()
        res = self.rsession.cmd("RKernel::zmq_new_sender(%d)" % port)
        self.r_zmq_recv_port = port
        context = self.zmq_context

        self.zmq_reply_q = Queue()
        self.zmq_comm_q = Queue()
        self.zmq_debug_q = Queue()
        self.zmq_display_q = Queue()

        def r_zmq_watcher(context,port,reply_q,comm_q,debug_q,display_q):
            socket = context.socket(zmq.PULL)
            url = "tcp://*:%d" % port
            socket.bind(url)
            while True:
                msk = socket.poll()
                msg = socket.recv_multipart()
                msg = json.loads(msg[0].decode("utf-8"))
                msg_type = msg['type']
                if msg_type in ('display_data', 'update_display_data'):
                    display_q.put(msg)
                elif msg_type in ("comm_open","comm_msg","comm_close"):
                    comm_q.put(msg)
                elif msg_type in ("debug_event"):
                    debug_q.put(msg)
                else:
                    reply_q.put(msg)

        self.r_zmq_watcher = Thread(target = r_zmq_watcher,
                                    args = (self.zmq_context,
                                            self.r_zmq_recv_port,
                                            self.zmq_reply_q,
                                            self.zmq_comm_q,
                                            self.zmq_debug_q,
                                            self.zmq_display_q))
        self.r_zmq_watcher.daemon = True
        self.r_zmq_watcher.start()

        def r_msg_dispatcher(queue,handler):
            while True:
                msg = queue.get()
                handler(msg)

        self.r_comm_thread = Thread(target = r_msg_dispatcher,
                                    args = (self.zmq_comm_q, 
                                            self.handle_comm))
        self.r_comm_thread.daemon = True
        self.r_comm_thread.start()

        # self.r_debug_thread = Thread(target = r_msg_dispatcher,
        #                              args = (self.zmq_debug_q,
        #                                      self.handle_debug_event))
        # self.r_debug_thread.daemon = True
        # self.r_debug_thread.start()

    def r_start_graphics(self):
        self.rsession.cmd_nowait("RKernel::start_graphics()")

    def r_run_cell_begin_hooks(self):
        self.rsession.run("RKernel::runHooks('cell-begin')")

    def r_run_cell_end_hooks(self):
        self.rsession.run("RKernel::runHooks('cell-end')")

    json_incomplete = False
    json_frag = ''

    def handle_stdout(self,text):
        # self.log_out("handle_stdout")
        # self.log_out(pformat(text))
        for line in text:
            if DLE in line:
                chunks = line.split(DLE)
                for chunk in chunks:
                    if chunk.startswith(ZMQ_PUSH):
                        msg = self.r_zmq_receive()
                        # self.log_out(pformat(msg))
                        self.r_handle_zmq(msg)
                    elif chunk.startswith(JSON_MSG):
                        # if len(chunk) > 70:
                        #     self.log_out(repr((chunk[:70]+'...'+text[-10:])))
                        # else:
                        #     self.log_out(repr(chunk))
                        if chunk.endswith(ETB):
                            msg = chunk.removeprefix(JSON_MSG).removesuffix(ETB)
                            self.r_handle_json(msg)
                        else:
                            self.json_incomplete = True
                            self.json_frag = chunk.removeprefix(JSON_MSG)
                    elif chunk.endswith(ETB):
                        msg = self.json_frag + chunk.removesuffix(ETB)
                        self.json_frag = ''
                        self.json_incomplete = False
                        self.r_handle_json(msg)
                    else:
                        if len(chunk) > 0:
                            self.stream(chunk,stream='stdout')
            else:
                if len(line) > 0:
                    self.stream(line,stream='stdout')

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
        self.rsession.cmd("RKernel::install_save_q()")

    def r_set_help_port(self):
        port = random_port()
        self.rsession.cmd("RKernel::set_help_port(%d)" % port)

    def r_set_help_displayed(self):
        self.rsession.cmd("RKernel::set_help_displayed(TRUE)")
        
    def r_handle_json(self,jmsg):
        # self.log_out("r_handle_json")
        # self.log_out(pformat(jmsg))
        msg = json.loads(jmsg)
        msg_type = msg['type']
        r_handler = self.r_handlers.get(msg_type,None)
        if r_handler is None:
            message = 'Unknown message type "%s"' % msg_type
            self.stream(message,stream='stderr')
            # self.log_out(message)
        else:
            r_handler(msg)
        
    async def comm_info_request(self, stream, ident, parent):
        """Handle comm_info_request """
        if not self.session:
            return
        content = parent["content"]
        target_name = content.get("target_name", None)

        request = dict(target_name = target_name)
        msg = dict(type = 'comm_info_request',
                   content = request)
        response = self.r_zmq_request(msg)

        reply_content = response['content']
        msg = self.session.send(stream, "comm_info_reply", reply_content, parent, ident)

    async def comm_open(self, stream, ident, parent):
        """ Handle comm_open """
        if not self.session:
            return
        content = parent["content"]
        comm_id = content['comm_id']
        target_name = content['target_name']
        data = content['data']
        request = dict(comm_id = comm_id,
                       target_name = target_name,
                       data = data)
        msg = dict(type = 'comm_open',
                   content = request)
        self.r_zmq_request_noreply(msg)

    async def comm_msg(self, stream, ident, parent):
        """Handle comm_msg """
        if not self.session:
            return
        content = parent["content"]
        comm_id = content['comm_id']
        data = content['data']
        buffers = parent['buffers']
        request = dict(comm_id = comm_id,
                       data = data,
                       buffers = buffers)
        msg = dict(type = 'comm_msg',
                   content = request)
        self.r_zmq_request_noreply(msg)

    async def comm_close(self, stream, ident, parent):
        """Handle comm_close """
        if not self.session:
            return
        content = parent["content"]
        comm_id = content['comm_id']
        data = content['data']
        request = dict(comm_id = comm_id,
                       data = data)
        msg = dict(type = 'comm_close',
                   content = request)
        self.r_zmq_request_noreply(msg)

    @property
    def kernel_info(self):
        return {
            "protocol_version": "5.3",
            "implementation": self.implementation,
            "implementation_version": self.implementation_version,
            "language_info": self.language_info,
            "banner": self.banner,
            "help_links": self.help_links,
            "debugger": True
        }

    async def do_debug_request(self, msg_content):
        # self.log_out('=========== do_debug_request ===========')
        command = msg_content['command']
        # self.log_out('Command: %s' % command)
        # self.log_out(pformat(msg_content))
        handler = self.dbg_handlers.get(command,None)
        if handler is None:
            reply_body = dict()
            success = False
        else:
            reply_body = handler(msg_content)
            success = True
        # self.log_out('Handler complete')
        # self.log_out('Response:')
        # self.log_out(pformat(reply_body))
        reply_content = dict(
            type = 'response',
            command = command,
            request_seq = msg_content['seq'],
            body = reply_body,
            success = success
        )
        return reply_content

    event_seq = 1
    
    def debug_event(self, event, body = None):
        # self.log_out('=========== debug_event ===========')
        # self.log_out('Event: %s' % event)
        # self.log_out('Body:')
        # self.log_out(pformat(body))
        msg_type = 'debug_event'
        msg_content = dict(
            seq = self.event_seq,
            type = 'event',
            event = event,
            body = body
        )
        self.send_response(self.iopub_socket,
                           msg_type,
                           content = msg_content)
        self.event_seq += 1

    dbg_started = False
    dbg_client_info = None

    def debugInfo(self, request):
        reply = dict(
            isStarted = self.dbg_started,
            hashMethod = 'Murmur2',
            hashSeed = 3339675911,
            tmpFilePrefix = self.rsession.temp_dir,
            tmpFileSuffix = '.R',
            breakpoints = [],
            stoppedThreads = [],
            richRendering = True,
            exceptionPaths = []
        ) 
        return reply

    def debug_init(self, request):
        self.dbg_client_info = request['arguments']
        self.debug_event('initialized')
        self.debug_event('process',
                         body = dict(
                             systemProcessId = self.rsession.pid(),
                             name = shutil.which('R'),
                             isLocalProcess = True,
                             startMethod = 'attach'
                         ))
        self.debug_event('thread',
                         body = dict(
                             reason = 'started',
                             threadId = 1
                         ))
        reply = dict(
            supportsSetVariable = True,
            supportsConfigurationDoneRequest = True,
            supportsFunctionBreakpoints = True,
            supportsConditionalBreakpoints = True,
            supportsLogPoints = True,
            supportsStepInTargetsRequest = True,
            exceptionBreakpointFilters = [dict(
                filter = 'stop',
                label = 'Runtime error',
                default = False,
                description = 'Break when an error occurs that is not caught by a try() or tryCactch() expression'
            )]
        )
        return reply

    def debug_attach(self, request):
        self.dbg_started = True
        return dict()

    def debug_disconnect(self, request):
        self.dbg_started = False
        return dict()

    def debug_passthru(self, request_content):
        # self.log_out('=== debug_passthru ================')
        # self.log_out(pformat(request_content))
        request = dict(
            type = 'debug_request',
            content = request_content
        )
        r_reply = self.r_zmq_request(request)
        reply_body = r_reply['content']['body']
        # self.log_out(pformat(reply_body))
        return reply_body
    
class JSONerror(Exception):
    pass

class ZMQtimeout(Exception):
    pass


def paste0(l):
    return ''.join(l)
