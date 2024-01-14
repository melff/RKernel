import os
from pprint import pformat
import rsession
from rsession import RSession
import json

R = RSession()
R.start(silent=True)

R.writeline("cat('Hello World!')")
R.read()


R.writeline("library(httpgd)")
R.writeline("library(jsonlite)")
R.writeline("hgd(silent=TRUE,width=700,height=700)")

def json_command(self,command):
    jsoncmd = "toJSON(" + command + ")"
    self.writeline(jsoncmd)
    out = self.read(timeout=0.1)
    if(len(out) > 0):
        out = json.loads(out)
    else:
        out = None
    return out

def get_hgd_state(self):
    try:
        out = json_command(self,"hgd_state()")
    except:
        return None
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



def run_lines(lines,echo=False):
    res = []
    h_desc = get_hgd_state(R)
    h_state = poll_hgd_state(h_desc)
    for i in range(len(lines)):
        if echo:
            res.append("%d. %s"% (i,lines[i]))
        R.writeline(lines[i])
        while True:
            out = R.read(stream='stderr',timeout=0.1)
            if len(out) == 0:
                break
            else:
                out = "%d. %s"% (i,out)
                res.append(out)
        while True:
            out = R.read(timeout=0.1)
            if len(out) == 0:
                break
            else:
                out = "%d. %s"% (i,out)
                res.append(out)
        h_state_old = h_state
        h_state = poll_hgd_state(h_desc)
        if h_state != None and isinstance(h_state,dict) and ('upid' in h_state):
            res.append('%d. <graphics uip %d>' % (i,h_state['upid']))
        else:
            res.append('%d. <graphics is down?>' % i)
            res.append(pformat(h_state))
        (server_changed, device_changed) = cmp_hstate(h_state,h_state_old)
    R.writeline("cat('-- Done!--\n')")
    done = R.read()
    res.append(done)
    return res

res = run_lines(RLines,echo=True)
print('\n'.join(res))

srclines = source("dummy-script.R")
res = run_lines(srclines,echo=True)
print('\n'.join(res))

res = run_lines(["example(points)"],echo=True)
print('\n'.join(res))





