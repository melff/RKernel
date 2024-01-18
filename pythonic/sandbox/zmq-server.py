# Source: https://zguide.zeromq.org/docs/chapter1/
#
#   Hello World server in Python
#   Binds REP socket to tcp://*:5555
#   Expects b"Hello" from client, replies with b"World"
#

import time
import zmq

context = zmq.Context()
socket = context.socket(zmq.REP)
socket.bind("tcp://*:5555")

for req_num in range(5):
    #  Wait for next request from client
    message = socket.recv()
    print(f"Received request no {req_num}: {message}")

    #  Do some 'work'
    time.sleep(1)

    #  Send reply back to client
    socket.send_string("World")

for req_num in range(5):
    #  Wait for next request from client
    message = socket.recv()
    print(f"Received request no {req_num}: {message}")

    #  Do some 'work'
    time.sleep(1)

    #  Send reply back to client
    socket.send_string("World")

    
print("That's all, folks!")
