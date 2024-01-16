import os

from termcolor import colored

from subprocess import *

from signal import SIGINT

import threading
from threading import Thread, Event
from queue import Queue, Empty


class RSession(object):

    def start(self):
        
        args = ["R",
                     "--no-save",
                     "--no-restore",
                     "--no-readline",
                     "--interactive"]

        self.proc = Popen(args,
                          stdin=PIPE,
                          stdout=PIPE,
                          stderr=PIPE,
                          encoding='utf8',
                          text=True,
                          bufsize=0)

        self.stdout_queue = Queue()
        self.stderr_queue = Queue()

        # See github/pexpect/popen_spawn.py
        def _read(stream, queue):
            '''
            Collect lines from 'stream' and put them in 'quque'.
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
    
    def read(self, stream='stdout', timeout = None):
        r = b''
        while True:
            b = self.read1(stream = stream, timeout = timeout)
            if b == None:
                break
            else:
                r = r + b
        if len(r) > 0:
            r = r.decode('utf-8')
            if stream == 'stdout':
                self.last_output = r
        else:
            r = None
        return r

    def found_prompt(self,prompt = '> '):
        if len(self.last_output) < 1:
            return False
        return self.last_output.endswith(prompt)

    def find_prompt(self,prompt = '> ', pop = True):
        while not self.found_prompt(prompt):
            self.read(timeout = .1)
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

    def run(self,text,prompt = '> ', coprompt = '+ '):
        if not self.found_prompt(prompt) and not self.found_prompt(coprompt):
            raise NotAtPrompt
        if not isinstance(text,list):
            text = text.split('\n')
        stdout = []
        stderr = []
        numlines = len(text)
        for i in range(numlines):
            line = text.pop(0)
            self.sendline(line)
            while True:
                stderr1 = self.read(stream='stderr',timeout=.1)
                stdout1 = self.read(timeout=.1)
                if stderr1 is not None:
                    stderr.append(stderr1)
                if stdout1 is not None:
                    if self.found_prompt(coprompt):
                        break
                    if self.found_prompt(prompt):
                        stdout1 = stdout1.rstrip(prompt)
                        stdout.append(stdout1)
                        break
                    else:
                        stdout.append(stdout1)
            if self.found_prompt(prompt):
                stdout = ''.join(stdout)
                if len(stdout) == 0:
                    stdout = None
                stderr = ''.join(stderr)
                if len(stderr) == 0:
                    stderr = None
                rest = ''.join(text)
                if len(rest) == 0:
                    rest = None
                return dict(stdout=stdout,
                            stderr=stderr,
                            rest=rest)
        if self.found_prompt(coprompt):
            self.interrupt()
            self.find_prompt(prompt)
            raise InputIncomplete
        else:
            return None # This should never be reached.
                    
            
class EndOfStream(Exception): pass
class NotAtPrompt(Exception): pass
class MultipleLines(Exception): pass
class InputIncomplete(Exception): pass
