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
