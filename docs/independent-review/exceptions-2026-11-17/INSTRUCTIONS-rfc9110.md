# Which RFC 9110 requirements apply to an origin server?

You are classifying the requirements of RFC 9110 (HTTP Semantics) for a
specific implementation, independently: from the RFC text alone.

## The implementation

An HTTP origin server (HTTP/1.1, HTTP/2 and HTTP/3): it receives requests and sends responses for
resources it owns. It is not a client or user agent (it never sends
requests or receives responses), not a proxy, gateway or tunnel (it
never forwards messages), and not a cache. It does not process data of
the `message/http` or `application/http` media types. It does
interpret request framing (Content-Length, Transfer-Encoding, chunked
coding), parses request lines and fields, generates responses including
their framing, manages persistent connections, and may use TLS.

A requirement applies to it if the server could ever be the party the
requirement constrains: for example a "sender" or "server" requirement
when it sends a response, a "recipient" or "server" requirement when it
receives a request, or a requirement on all implementations. A
requirement whose subject is a client, user agent, proxy, intermediary,
gateway, tunnel or cache applies only if the server can be that party.
Read the whole sentence and its context in the RFC, not just its first
words: "a recipient" in a section about responses is a client.

## What you may read

Only the files in this directory: `requirements.md` (the requirements,
numbered R1 to R412, with their sections and levels), and `rfc9110.txt`
and `rfc9112.txt` for context. Do not read anything else on this machine,
and do not search the web.

## What to produce

`classification.json`: a JSON array with one object per requirement:

    { "id": "R1",
      "verdict": "applies" | "client" | "intermediary" | "cache"
               | "media-type" | "registration" | "uncertain",
      "reason": "one or two sentences",
      "evidence": "the words of the RFC (requirement or its context) that decide it" }

Use "client" for clients and user agents, "intermediary" for proxies,
gateways and tunnels, "cache" for caches, "media-type" for processing
message/http data, "registration" for IANA registrations or authors of
specifications. Use "applies" if the server could be the party
constrained, even only in some cases, and say which cases. Use
"uncertain" only when the text genuinely cannot decide it, and explain.

When you finish, reply with the counts per verdict and the requirements
you found hardest, with why.
