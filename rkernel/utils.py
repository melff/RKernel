import os

def sourcefile(filename):
    path = os.path.join(R_files_path(),filename)
    srcfile = open(path,"r")
    lines = [line.rstrip(os.linesep) for line in srcfile.readlines()]
    srcfile.close()
    return lines

def R_files_path():
    cur_path = os.path.dirname(os.path.realpath(__file__))
    return os.path.join(cur_path,"R")

# https://stackoverflow.com/questions/1365265/on-localhost-how-do-i-pick-a-free-port-number
import socket
from contextlib import closing

def random_port():
    with closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as s:
        s.bind(('', 0))
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        return s.getsockname()[1]

def dQuote(string):
    return '"' + string + '"'

def sQuote(string):
    return "'" + string + "'"

def R_c_wrap(strings):
    string = ', '.join(strings)
    return "c(" + string + ")"
