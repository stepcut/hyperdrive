"""Python's view of IPv6 addresses, for the differential test.

Reads one hex-encoded byte string per line, and writes one JSON object
per line: whether ipaddress.IPv6Address accepts it, and if so the
128-bit value and the text form it renders.
"""

import ipaddress
import json
import sys

for line in sys.stdin:
    text = bytes.fromhex(line.strip()).decode("latin-1")
    try:
        address = ipaddress.IPv6Address(text)
    except ValueError:
        print(json.dumps({"ok": False}))
    else:
        print(json.dumps({"ok": True, "value": format(int(address), "032x"), "text": str(address)}))
