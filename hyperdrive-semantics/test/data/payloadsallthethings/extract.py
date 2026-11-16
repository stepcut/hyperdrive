#!/usr/bin/env python3
"""Extract the hosts used in the SSRF filter-bypass examples of
PayloadsAllTheThings' "Server Side Request Forgery/README.md" into
ssrf-hosts.txt, one authority per line.

Usage: extract.py README.md > ssrf-hosts.txt

Only the "Bypassing Filters" sections up to "Bypass Abusing URL Parsing
Discrepancy" (inclusive) are used. Every http(s):// URL contributes the
text between "//" and the next "/", "?" or "#", or the end of the line.
The domains in the "Domain Redirect" table are added as well.
"""
import re
import sys

text = open(sys.argv[1], encoding="utf-8").read()
start = text.index("## Bypassing Filters")
end = text.index("### Bypass PHP filter_var() Function")
section = text[start:end]

hosts = []
in_code = False
for line in section.splitlines():
    if line.strip().startswith("```"):
        in_code = not in_code
        continue
    if in_code:
        for m in re.finditer(r"https?://([^/?#\s]*)", line):
            hosts.append(m.group(1))
    # the domain redirect table: | localtest.me | `::1` |
    m = re.match(r"\|\s*([A-Za-z0-9.\-\[\]_]+)\s*\|\s*`", line)
    if m and m.group(1) != "Domain":
        hosts.append(m.group(1))

seen = set()
for h in hosts:
    if h not in seen:
        seen.add(h)
        print(h)
