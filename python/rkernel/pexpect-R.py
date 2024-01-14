import pexpect
from pexpect import fdpexpect

child = pexpect.spawn("R --no-save --interactive")

child_err = fdpexpect.fdspawn(child.stderr)


def read_line(this):
    r = b''
    while True:
        try:
            b = this.read_nonblocking(size=1,timeout=.1)
            if b == b'\n':
                break
            if b != b'\r':
                r = r + b
        except pexpect.TIMEOUT:
            break
    return r.decode('utf-8')

for i in range(80):
    line = read_line(child)
    print(line)
    if line == '':
        break

child.terminate()


args = ["R","--no-save","--interactive","--no-echo"]

child_silent = pexpect.spawn("R --no-save --interactive --no-echo",echo=False)
child_err = fdpexpect.fdspawn(child_silent.stderr)
child_silent.sendline("ls()")
read_line(child_silent)
read_line(child_silent)
child_silent.sendline("stop('Oopsie!')")
read_line(child_silent)
read_line(child_err)


child_silent.terminate()

import subprocess
from pexpect import fdpexpect

args = ["R","--no-save","--interactive"]
p = subprocess.Popen(args, 
                     stdout=subprocess.PIPE,
                     stdin=subprocess.PIPE, 
                     stderr=subprocess.PIPE)
child = fdpexpect.fdspawn(p.stdout)
child_err = fdpexpect.fdspawn(p.stderr)

def read_line(this):
    r = b''
    while True:
        try:
            b = this.read_nonblocking(size=1,timeout=0.1)
            if b == b'\n':
                break
            if b != b'\r':
                r = r + b
        except pexpect.TIMEOUT:
            break
    return r.decode('utf-8')


for i in range(15):
    line = read_line(child)
    if len(line) > 0:
        print(line)
    #if line == '':
    #    break


def writeline(p,line):
    line = line + '\n'
    line = line.encode('utf-8')
    p.stdin.writelines([line])

writeline(p,'print("Hello World!")')

p.terminate()
