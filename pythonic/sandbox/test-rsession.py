import os
import sys
import inspect

currentdir = os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))
parentdir = os.path.dirname(currentdir)
sys.path.insert(0, parentdir) 
from pprint import pformat
from rkernel.rsession import RSession
import json

R = RSession()

R.start()
intro = R.find_prompt()
print(intro)

R.run("cat('Hello World!')")
R.run("example(ls)")

R.run("stop('Oopsie!')")


ml = """for(i in 1:3)
"""
R.run(ml)

R.run("print(i)")


ml = """for(i in 1:3)
    print(i)"""
R.run(ml)

R.cmd("ls()")

R.cmd("ls(")

R.cmd("ls(]")


R.cmd("library(httpgd)")
R.cmd("library(jsonlite)")
R.cmd("hgd(silent=TRUE,width=700,height=700)")

R.run("example(points)")



def json_command(self,command):
    jsoncmd = "jsonlite::toJSON({" + command + "})"
    stdout, stderr = self.cmd(jsoncmd)
    if(len(stdout) > 0):
        result = json.loads(stdout)
    else:
        result = None
    return result

def get_hgd_state(self):
    out = json_command(self,"hgd_state()")
    res = dict()
    if isinstance(out,dict):
        for key,value in out.items():
            res[key] = value[0]
    else:
        res = out
    return res

def get_hgd_info(self):
    try:
        out = json_command(self,"hgd_info()")
    except:
        return None
    res = dict()
    for key,value in out.items():
        if isinstance(value,list) and len(value) == 1:
            res[key] = value[0]
        else:
            res[key] = value
    return res
    
get_hgd_state(R)

get_hgd_info(R)

hgd_desc = get_hgd_state(R)

import urllib
import urllib.request

def poll_hgd_state(h):
    url = "http://{host}:{port}/state?token={token}".format(**h)
    f = urllib.request.urlopen(url)
    data = f.read().decode("utf-8")
    return json.loads(data)

def get_hgd_graphics(h,format='svg'):
    d = h.copy()
    d['id'] = d['hsize'] - 1
    d['renderer'] = format
    url = "http://{host}:{port}/plot?token={token}&id={id}&renderer={renderer}".format(**d)
    f = urllib.request.urlopen(url)
    data = f.read()
    return data



def cmp_hstate(state1,state2):
    try:
        server_eq = state1['host'] == state2['host']
        server_eq = server_eq and state1['port'] == state2['port']
        server_eq = server_eq and state1['token'] == state2['token']
    except:
        server_eq = None
    try:
        state_eq = state1['upid'] == state2['upid']
    except:
        state_eq = None
    return (server_eq, state_eq)

#R.quit()

RCode = """cat("Hello World!\n")
plot(rnorm(20))
abline(h=0)
dev.size()
print("Hello again!")
"""




srclines = source("dummy-script.R")
R.run(srclines)


json.loads(R.cmd("toJSON(ls())")[0])

json.loads(R.cmd("toJSON(list(a=1,b=2))")[0])


