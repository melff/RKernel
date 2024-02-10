import os
from termcolor import colored
from subprocess import *
from signal import SIGINT
import threading
from threading import Thread, Event
from queue import Queue, Empty
from datetime import datetime
from pprint import pprint, pformat


class RSession(object):

    env = dict()
    log_file = None

    def start(self):
        
        args = ["R",
                "--no-save",
                "--no-restore",
                "--no-readline",
                "--interactive"]

        if len(self.env) > 0:
            env = os.environ.copy()
            env.update(self.env)
        else:
            env = None
            
        self.proc = Popen(args,
                          stdin=PIPE,
                          stdout=PIPE,
                          stderr=PIPE,
                          encoding='utf8',
                          text=True,
                          env=env,
                          bufsize=0)

        self.stdout_queue = Queue()
        self.stderr_queue = Queue()

        # See github/pexpect/popen_spawn.py
        def _read(stream, queue):
            '''
            Collect bytes from 'stream' and put them in 'quque'.
            '''
            fileno = stream.fileno()
            while True:
                buf = b''
                try:
                    buf = os.read(fileno,1024)
                except OSError as e:
                    pass
                if not buf:
                    queue.put(None)
                    return
                queue.put(buf)

        self.thread_stdout = Thread(target = _read,
                                    args = (self.proc.stdout,
                                            self.stdout_queue))
        self.thread_stdout.daemon = True
        self.thread_stdout.start()

        self.thread_stderr = Thread(target = _read,
                                    args = (self.proc.stderr,
                                            self.stderr_queue))
        self.thread_stderr.daemon = True
        self.thread_stderr.start()

        # self.log_file = open("/tmp/RKernel.log","a")
        # self.log_out("-- Session started ---")

    def quit(self):
        self.flush(stream='stdout')
        self.flush(stream='stderr')
        self.sendline("q()")
        self.proc.wait()

    def interrupt(self):
        self.proc.send_signal(SIGINT)

    def terminate(self):
        self.flush(stream='stdout')
        self.flush(stream='stderr')
        self.proc.terminate()

    def kill(self):
        self.flush(stream='stdout')
        self.flush(stream='stderr')
        self.proc.kill()
        
    def sendline(self,line):
        if '\n' in line:
            raise MultipleLines
        self.proc.stdin.writelines([line + '\n'])
        
    def sendall(self,line):
        self.proc.stdin.writelines([line + '\n'])

    def read1(self, stream='stdout', timeout = None):
        if stream == 'stdout':
            q = self.stdout_queue
        elif stream == 'stderr':
            q = self.stderr_queue
        try:
            b = q.get(block = timeout is not None,
                      timeout = timeout)
        except Empty:
            return None
        return b

    last_output = ''
    
    def read(self, stream='stdout', timeout = None, break_on = None):
        r = b''
        while True:
            b = self.read1(stream = stream, timeout = timeout)
            if b == None:
                break
            else:
                r = r + b
                if break_on is not None and break_on in b:
                    break
        if len(r) > 0:
            r = r.decode('utf-8')
            if stream == 'stdout':
                self.last_output = r
        else:
            r = None
        return r
    
    def readline(self, stream='stdout', timeout = None):
        return self.read(stream = stream, timeout = timeout, break_on = b'\n')
        
    def found_prompt(self,prompt = '> '):
        if len(self.last_output) < 1:
            return False
        return self.last_output.endswith(prompt)

    def find_prompt(self,prompt = '> ', pop = True, timeout = None):
        while not self.found_prompt(prompt):
            self.read(timeout = timeout)
        res = self.last_output.rstrip(prompt)
        if pop:
            self.last_output = prompt
        return res
    
    def running(self):
        r = self.proc.poll()
        return r == None

    def flush(self, stream='stdout'):
        if stream == 'stdout':
            q = self.stdout_queue
        elif stream == 'stderr':
            q = self.stderr_queue
        while not q.empty():
            try:
                line = q.get_nowait()
            except EndOfStream:
                break
            
    def run(self,text,prompt = '> ', coprompt = '+ ', timeout = 0.001):
        if not self.found_prompt(prompt) and not self.found_prompt(coprompt):
            raise NotAtPrompt
        if not isinstance(text,list):
            text = text.split('\n')
        for line in text:
            self.sendline(line)
            while True:
                stderr = self.read(stream='stderr',timeout=timeout)
                stdout = self.read(timeout=timeout)
                if stderr is not None:
                    self.handle_stderr(stderr)
                if stdout is not None:
                    if self.found_prompt(coprompt):
                        break
                    if self.found_prompt(prompt):
                        stdout = stdout.strip(prompt)
                        self.handle_stdout(stdout)
                        self.handle_prompt()
                        break
                    else:
                        stdout = stdout.strip(prompt)
                        self.handle_stdout(stdout)
        if self.found_prompt(coprompt):
            self.interrupt()
            self.find_prompt(prompt)
            raise InputIncomplete
        else:
            self.find_prompt(prompt)

    def cmd_nowait(self,text,prompt = '> ', coprompt = '+ '):
        if not self.found_prompt(prompt):
            # self.log_out("cmd_nowait: Not at promt")
            # self.log_out(pformat(self.last_output))
            raise NotAtPrompt
        if not isinstance(text,str):
            raise TypeError

        self.sendline(text)

    def process_output(self,prompt = '> ', coprompt = '+ ', timeout = 0.001):
        # self.log_out("RSession.process_output")
        while True:
            stderr = self.read(stream='stderr',timeout=timeout)
            stdout = self.read(timeout=timeout)
            
            if stderr is not None:
                # self.log_out(pformat(stderr))
                self.handle_stderr(stderr)
            if stdout is not None:
                # self.log_out(pformat(stdout))
                if self.found_prompt(coprompt):
                    break
                if self.found_prompt(prompt):
                    stdout = stdout.strip(prompt)
                    self.handle_stdout(stdout)
                    break
                else:
                    stdout = stdout.strip(prompt)
                    self.handle_stdout(stdout)
        if self.found_prompt(coprompt):
            self.interrupt()
            self.find_prompt(prompt)
            raise InputIncomplete
        else:
            self.find_prompt(prompt)
        

    def cmd(self,text,prompt = '> ', coprompt = '+ ', timeout = 0.001):
        if not self.found_prompt(prompt):
            raise NotAtPrompt
        if not isinstance(text,str):
            raise TypeError
            
        stdout = ''
        stderr = ''

        self.sendline(text)
        while True:
            stderr1 = self.read(stream='stderr',timeout=timeout)
            if stderr1 is not None:
                stderr += stderr1
            stdout1 = self.read(timeout=timeout)
            if stdout1 is not None:
                stdout += stdout1
                if self.found_prompt(coprompt):
                    break
                if self.found_prompt(prompt):
                    break
        if self.found_prompt(coprompt):
            self.interrupt()
            self.find_prompt(prompt)
            raise InputIncomplete
        else:
            stdout = stdout.strip(prompt)
            self.find_prompt(prompt)
            return (stdout,stderr)

    def handle_stdout(self,text):
        text = colored(text,self.stdout_color)
        print(text,end='')
        #print("<update graphics>")

    def handle_stderr(self,text):
        text = colored(text,self.stderr_color)
        print(text,end='')

    def handle_prompt(self):
        pass
        
    stdout_color = "green"
    stderr_color = "red"

    def source(filename):
        srclines = source(filename)
        
    def log_out(self,text):
        now = format(datetime.now())
        text = 'SESSION: ' + now + ' ' + text
        self.log_file.write(text + '\n')
        self.log_file.flush()


class EndOfStream(Exception): pass
class NotAtPrompt(Exception): pass
class MultipleLines(Exception): pass
class InputIncomplete(Exception): pass

def source(filename):
    srcfile = open(filename,"r")
    lines = [line.rstrip(os.linesep) for line in srcfile.readlines()]
    srcfile.close()
    return lines
