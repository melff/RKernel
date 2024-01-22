import os
import subprocess

def set_libpaths():

    if "VIRTUAL_ENV" in os.environ:
        VIRTUAL_ENV = os.getenv("VIRTUAL_ENV")
        R_LIBS_USER = os.path.join(VIRTUAL_ENV,"lib","R","library")
    elif "JUPYTER_CONFIG_DIR" in os.environ:
        JUPYTER_CONFIG_DIR = os.getenv("JUPYTER_CONFIG_DIR")
        R_LIBS_USER = os.path.join(JUPYTER_CONFIG_DIR,"R","library")
    else:
        JUPYTER_CONFIG_DIR = os.path.join(os.path.expanduser("~"),".jupyter")
        R_LIBS_USER = os.path.join(JUPYTER_CONFIG_DIR,"R","library")
    if not os.path.isdir(R_LIBS_USER):
        os.makedirs(R_LIBS_USER)
    env = os.environ.copy()
    env["R_LIBS_USER"] = R_LIBS_USER
    return env

def rcall(expressions):

    if isinstance(expressions,str):
        expressions = [expressions]
    eargs = list()
    for expr in expressions:
        e = "-e '%s'" % expr
        eargs.append(e)
    eargs = ' '.join(eargs)
    cmd = "Rscript --no-save --no-restore"
    call = cmd + ' ' + eargs

    env = set_libpaths()
    
    res = subprocess.run(call, capture_output = True, shell = True,
                         env = env)

    return (res.returncode,
            res.stdout.decode("utf-8"),
            res.stderr.decode("utf-8"))

