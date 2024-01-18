### Hello world client as in the ZeroMQ guide.
# SHELL> Rscript hwserver.r &
# SHELL> Rscript hwclient.r
# from demo file 'hwclient.r'

suppressMessages(library(pbdZMQ, quietly = TRUE))

### Initial.
context <- zmq.ctx.new()
requester <- zmq.socket(context, ZMQ.ST()$REQ)
zmq.connect(requester, "tcp://localhost:5555")

### Send and receive 4 times.
for(i.req in 1:4){
  cat("Sending 'Hello' ", i.req, "\n")
  zmq.send(requester, "Hello")
  buf <- zmq.recv(requester, 10L)
  cat("Received 'World' ", i.req, "\n")
} 

### Send and receive 6 times.
for(i.req in 1:6){
  cat("Sending 'Hello' ", i.req, "\n")
  zmq.send(requester, "Hello")
  buf <- zmq.recv(requester, 10L)
  cat("Received 'World' ", i.req, "\n")
} 

### Finish.
zmq.close(requester)
zmq.ctx.destroy(context)
