import socketserver
import time

class MyHandler(socketserver.BaseRequestHandler):
    def handle(self):
        self.data = self.request.recv(1024)
        print("{} wrote:".format(self.client_address[0]))
        print(self.data)

res = []

class MyStreamHandler(socketserver.StreamRequestHandler):

    def handle(self):
        # self.rfile is a file-like object created by the handler;
        # we can now use e.g. readline() instead of raw recv() calls
        data = self.rfile.readline()
        data = data.decode('utf-8')
        data = data.replace('\x00','')
        print("{} wrote:".format(self.client_address[0]))
        print(data)
        res.append(data)
        #time.sleep(5)
        response = "Recieved ..\n"
        response = response.encode('utf-8')
        self.wfile.write(response)
        print("Sent response")

def serve(host,port):
    socketserver.TCPServer.allow_reuse_address = True
    with socketserver.TCPServer((host, port), MyStreamHandler) as server:
        server.serve_forever()

# import threading
# from threading import Thread
# 
# server_thread = Thread(target=serve,args=("localhost",26011))
# server_thread.daemon = True
# server_thread.start()
# 
# import time
# 
# while True:
#     print("Tick")
#     time.sleep(1)
#     print("Tock")
#     time.sleep(1)
#     print(res)

serve("localhost",26011)
