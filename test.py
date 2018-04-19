import struct
import socket
with open("file.txt","rb") as f:
    a = f.read(8)
    print(type(a), len(a))
    b = struct.unpack("d",a)
    print(b)
