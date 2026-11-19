"""Open N idle connections to a server, report its RSS growth per connection,
then check it still answers quickly. Usage: idle.py PORT PID N"""
import socket, sys, time

port, pid, n = int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3])

def rss_kib():
    for line in open(f"/proc/{pid}/status"):
        if line.startswith("VmRSS:"):
            return int(line.split()[1])

before = rss_kib()
conns = []
for _ in range(n):
    s = socket.create_connection(("127.0.0.1", port))
    conns.append(s)
time.sleep(2)
after = rss_kib()
s = socket.create_connection(("127.0.0.1", port))
start = time.monotonic()
s.sendall(b"GET / HTTP/1.1\r\nHost: a\r\nConnection: close\r\n\r\n")
reply = b""
while True:
    b = s.recv(65536)
    if not b:
        break
    reply += b
took = time.monotonic() - start
print(f"{n} idle connections: RSS {before} KiB -> {after} KiB, {(after - before) * 1024 / n:.0f} bytes each; "
      f"a request alongside them took {took * 1000:.1f} ms ({reply.split(b' ')[1].decode()})")
for c in conns:
    c.close()
