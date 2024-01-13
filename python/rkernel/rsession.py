import os

from termcolor import colored

from subprocess import *

import threading
from threading import Thread, Event
from queue import Queue, Empty

class Thread2(Thread):

    def __init__(self, *args, **kwargs):
        super(Thread2,self).__init__(*args,**kwargs)
        self._stop_event = Event()

    def stop(self):
        self._stop_event.set()

    def stopped(self):
        self._stop_event.is_set()
        

class RSession(object):

    def start(self,silent = False):
        
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

        def _readline(stream, queue):
            '''
            Collect lines from 'stream' and put them in 'quque'.
            '''
            thread = threading.current_thread()
            while not thread.stopped():
                line = stream.readline()
                if line:
                    queue.put(line)
                else:
                    break

        self.thread_stdout = Thread2(target = _readline,
                                    args = (self.proc.stdout,
                                            self.stdout_queue))
        self.thread_stdout.daemon = True
        self.thread_stdout.start()

        self.thread_stderr = Thread2(target = _readline,
                                    args = (self.proc.stderr,
                                            self.stderr_queue))
        self.thread_stderr.daemon = True
        self.thread_stderr.start()

    def quit(self):
        self.flush(stream='stdout')
        self.flush(stream='stderr')
        self.thread_stdout.stop()
        self.thread_stderr.stop()
        #self.proc.terminate()
        self.writeline("q()")
        self.proc.wait()

    def writeline(self,line):
        self.proc.stdin.writelines([line + '\n'])

    def readline(self, stream='stdout', timeout= None):
        if stream == 'stdout':
            q = self.stdout_queue
        elif stream == 'stderr':
            q = self.stderr_queue
        try:
            line = q.get(block = timeout is not None,
                         timeout = timeout)
        except Empty:
            return None
        return line.rstrip(os.linesep)

    def printlines(self):
        while True:
            e = self.readline('stderr')
            o = self.readline('stdout')
            if e == None and o == None:
                break
            else:
                if e:
                    e = colored(e,"red")
                    print(e)
                if o:
                    print(o)
                    
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
