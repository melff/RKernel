import os

import rsession
from rsession import RSession

import json

def repl(self):
    if self.running():
        self.printlines()
    while self.running():
        line = input('> ')
        try:
            self.writeline(line)
            self.writeline(" ")
            self.printlines()
        except BrokenPipeError:
            pass



R = RSession()
R.start(silent=True)
#repl(R)

R.writeline("library(httpgd)")
R.writeline("library(jsonlite)")
R.writeline("hgd(silent=TRUE)")

def json_command(self,command):
    jsoncmd = "toJSON(" + command + ")"
    self.writeline(jsoncmd)
    out = None
    out = self.readline()
    return out

from pprint import pprint

def get_hgd_state(self):
    return json_command(self,"hgd_state()")

def get_hgd_info(self):
    return json_command(self,"hgd_info()")

    
pprint(get_hgd_state(R))

pprint(get_hgd_info(R))

R.writeline("plot(rnorm(20))")
R.writeline("abline(h=0)")
R.writeline("dev.size()")


#R.quit()

RCode = """cat("Hello World!")
plot(rnorm(20))
abline(h=0)
dev.size()
print("Hello again!")
"""

RLines = RCode.split("\n")

def source(filename):
    srcfile = open(filename,"r")
    lines = [line.rstrip(os.linesep) for line in srcfile.readlines()]
    srcfile.close()
    return lines

def run_lines(lines):
    res = []
    for i in range(len(RLines)):
        R.writeline(RLines[i])
        while True:
            out = R.readline(stream='stderr',timeout=0.1)
            if out == None:
                break
            else:
                res.append(out)
        
        while True:
            out = R.readline(timeout=0.1)
            if out == None:
                break
            else:
                res.append(out)
    return res

res = run_lines(RLines)
print('\n'.join(res))

srclines = source("dummy-script.R")
res = run_lines(RLines)
print('\n'.join(res))


def run_code(code):
    res = []
    R.writeline(code)
    while True:
        std_err = R.readline(stream='stderr',timeout=0.1)
        std_out = R.readline(stream='stdout',timeout=0.1)
        if std_err != None:
            res.append(std_err)
        if std_out != None:
            res.append(std_out)
        if std_err == None and std_out == None:
            break
    return res

res = run_code(RCode)
print('\n'.join(res))
