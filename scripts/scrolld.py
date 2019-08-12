#!/usr/bin/env python3
import queue

import socketserver
import threading
import typing

class Data(typing.NamedTuple):
    message: str
    response: str
    index: int

class MyTCPHandler(socketserver.BaseRequestHandler):
    """
    The request handler class for our server.

    It is instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.
    """


    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def handle(self):
        try:
            self.server.data
        except AttributeError:
            self.server.data = {}

        # self.request is the TCP socket connected to the client
        self.data = self.request.recv(1024).strip()

        try:
            data = self.data.decode()
            data_id = data[:3]
            size = int(data[3:])
        except ValueError:
            if len(self.data):
                data = self.data.decode()
                data_id = data[:3]
                message = data[3:]
                if not len(message):
                    del self.server.data[data_id]
                    self.request.sendall(b'OK')
                    return

                lines = message.split('\n')

                self.server.data[data_id] = Data(
                    message=' | '.join(lines) + ' | ',
                    response='',
                    index=0
                )
                self.request.sendall(b'OK')
            else:
                self.request.sendall(b'NOK')
            return

        if data_id not in self.server.data:
            self.request.sendall(b'')
            return

        message, response, index = self.server.data[data_id]

        response = response[1:]
        if len(response) < size:
            response = response.ljust(size)
        elif len(response) > size:
            response = response[len(response)-size:]

        response += message[index]
        index = (index + 1) % len(message)

        self.server.data[data_id] = Data(
            message=message,
            response=response,
            index=index,
        )
        self.request.sendall(response.encode())


if __name__ == "__main__":
    HOST, PORT = "localhost", 9999

    # Create the server, binding to localhost on port 9999
    with socketserver.TCPServer((HOST, PORT), MyTCPHandler) as server:
        # Activate the server; this will keep running until you
        # interrupt the program with Ctrl-C
        server.serve_forever()

