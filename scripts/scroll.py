#!/usr/bin/env python3
import socket
import sys

HOST, PORT = "localhost", 1236
data_id = sys.argv[1][:3]
data = " ".join(sys.argv[2:])

# Create a socket (SOCK_STREAM means a TCP socket)
with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
    # Connect to server and send data
    try:
        sock.connect((HOST, PORT))
        sock.sendall(bytes(data_id + data + "\n", "utf-8"))
        # Receive data from the server and shut down
        received = str(sock.recv(1024), "utf-8")

        print(received)
    except Exception as e:
        pass
