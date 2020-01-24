# simple-secure-sockets
A simple layer over the top of USOCKET for socket communication between clients and server

**Currently not secure at all**

Can connect up to roughly 500 simultaneous connections on the server. Can send data (max size 255 bytes currently) and these end up in a queue on the receiving client which has the same name as client. 
