import socketserver
import time


class MyStreamHandler(socketserver.StreamRequestHandler):

    response = """a <- 42
ls()
"""

    def handle(self):
        response = self.response.encode('utf-8')
        self.wfile.write(response)
        print("Sent response")

def serve(host,port):
    socketserver.TCPServer.allow_reuse_address = True
    with socketserver.TCPServer((host, port), MyStreamHandler) as server:
        server.serve_forever()


serve("localhost",26011)
