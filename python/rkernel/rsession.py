import os

from termcolor import colored

from subprocess import *

from signal import SIGINT

import threading
from threading import Thread, Event
from queue import Queue, Empty


class RSession(object):

    def start(self,silent = True):
        
        args = ["R",
                     "--no-save",
                     "--no-restore",
                     "--no-readline",
                     "--interactive"]

        if silent:
            args += ["--no-echo"]

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
        self.writeline("q()")
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
        
    def writeline(self,line):
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

    def read(self, stream='stdout', timeout = None, colorize = True):
        r = b''
        while True:
            b = self.read1(stream = stream, timeout = timeout)
            if b == None:
                break
            else:
                r = r + b
        return r.decode('utf-8')
            
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

    
            
class EndOfStream(Exception): pass
