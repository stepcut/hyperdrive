# RFC 9112 requirements

**R1** (Section 2.2, MUST): A recipient MUST parse an HTTP message as a sequence of octets in an encoding that is a superset of US-ASCII [USASCII].

**R2** (Section 2.2, MAY): Although the line terminator for the start-line and fields is the sequence CRLF, a recipient MAY recognize a single LF as a line terminator and ignore any preceding CR.

**R3** (Section 2.2, MUST): A sender MUST NOT generate a bare CR (a CR character not immediately followed by LF) within any protocol elements other than the content.

**R4** (Section 2.2, MUST): A recipient of such a bare CR MUST consider that element to be invalid or replace each bare CR with SP before processing the element or forwarding the message.

**R5** (Section 2.2, MUST): An HTTP/1.1 user agent MUST NOT preface or follow a request with an extra CRLF.

**R6** (Section 2.2, MUST): If terminating the request message body with a line-ending is desired, then the user agent MUST count the terminating CRLF octets as part of the message body length.

**R7** (Section 2.2, SHOULD): In the interest of robustness, a server that is expecting to receive and parse a request-line SHOULD ignore at least one empty line (CRLF) received prior to the request-line.

**R8** (Section 2.2, MUST): A sender MUST NOT send whitespace between the start-line and the first header field.

**R9** (Section 2.2, MUST): A recipient that receives whitespace between the start-line and the first header field MUST either reject the message as invalid or consume each whitespace-preceded line without further processing of it (i.e., ignore the entire line, along with any subsequent lines preceded by whitespace, until a properly formed header field is received or the header section is terminated).

**R10** (Section 2.2, SHOULD): When a server listening only for HTTP request messages, or processing what appears from the start-line to be an HTTP request message, receives a sequence of octets that does not match the HTTP-message grammar aside from the robustness exceptions listed above, the server SHOULD respond with a 400 (Bad Request) response and close the connection.

**R11** (Section 2.3, MUST): Intermediaries that process HTTP messages (i.e., all intermediaries other than those acting as tunnels) MUST send their own HTTP-version in forwarded messages, unless it is purposefully downgraded as a workaround for an upstream issue.

**R12** (Section 2.3, MAY): A server MAY send an HTTP/1.0 response to an HTTP/1.1 request if it is known or suspected that the client incorrectly implements the HTTP specification and is incapable of correctly processing later version responses, such as when a client fails to parse the version number correctly or when an intermediary is known to blindly forward the HTTP-version even when it doesn't conform to the given minor version of the protocol.

**R13** (Section 2.3, SHOULD): Such protocol downgrades SHOULD NOT be performed unless triggered by specific client attributes, such as when one or more of the request header fields (e.g., User-Agent) uniquely match the values sent by a client known to be in error.

**R14** (Section 3, MAY): Although the request-line grammar rule requires that each of the component elements be separated by a single SP octet, recipients MAY instead parse on whitespace-delimited word boundaries and, aside from the CRLF terminator, treat any form of whitespace as the SP separator while ignoring preceding or trailing whitespace; such whitespace includes one or more of the following octets: SP, HTAB, VT (%x0B), FF (%x0C), or bare CR.

**R15** (Section 3, SHOULD): A server that receives a method longer than any that it implements SHOULD respond with a 501 (Not Implemented) status code.

**R16** (Section 3, MUST): A server that receives a request- target longer than any URI it wishes to parse MUST respond with a 414 (URI Too Long) status code (see Section 15.5.15 of [HTTP]).

**R17** (Section 3, SHOULD): It is RECOMMENDED that all HTTP senders and recipients support, at a minimum, request-line lengths of 8000 octets.

**R18** (Section 3.2, SHOULD): Recipients of an invalid request-line SHOULD respond with either a 400 (Bad Request) error or a 301 (Moved Permanently) redirect with the request-target properly encoded.

**R19** (Section 3.2, SHOULD): A recipient SHOULD NOT attempt to autocorrect and then process the request without a redirect, since the invalid request-line might be deliberately crafted to bypass security filters along the request chain.

**R20** (Section 3.2, MUST): A client MUST send a Host header field (Section 7.2 of [HTTP]) in all HTTP/1.1 request messages.

**R21** (Section 3.2, MUST): If the target URI includes an authority component, then a client MUST send a field value for Host that is identical to that authority component, excluding any userinfo subcomponent and its "@" delimiter (Section 4.2 of [HTTP]).

**R22** (Section 3.2, MUST): If the authority component is missing or undefined for the target URI, then a client MUST send a Host header field with an empty field value.

**R23** (Section 3.2, MUST): A server MUST respond with a 400 (Bad Request) status code to any HTTP/1.1 request message that lacks a Host header field and to any request message that contains more than one Host header field line or a Host header field with an invalid field value.

**R24** (Section 3.2.1, MUST): When making a request directly to an origin server, other than a CONNECT or server-wide OPTIONS request (as detailed below), a client MUST send only the absolute path and query components of the target URI as the request-target.

**R25** (Section 3.2.1, MUST): If the target URI's path component is empty, the client MUST send "/" as the path within the origin-form of request-target.

**R26** (Section 3.2.2, MUST): When making a request to a proxy, other than a CONNECT or server-wide OPTIONS request (as detailed below), a client MUST send the target URI in "absolute-form" as the request-target.

**R27** (Section 3.2.2, MUST): A client MUST send a Host header field in an HTTP/1.1 request even if the request-target is in the absolute-form, since this allows the Host information to be forwarded through ancient HTTP/1.0 proxies that might not have implemented Host.

**R28** (Section 3.2.2, MUST): When a proxy receives a request with an absolute-form of request- target, the proxy MUST ignore the received Host header field (if any) and instead replace it with the host information of the request- target.

**R29** (Section 3.2.2, MUST): A proxy that forwards such a request MUST generate a new Host field value based on the received request-target rather than forward the received Host field value.

**R30** (Section 3.2.2, MUST): When an origin server receives a request with an absolute-form of request-target, the origin server MUST ignore the received Host header field (if any) and instead use the host information of the request-target.

**R31** (Section 3.2.2, MUST): A server MUST accept the absolute-form in requests even though most HTTP/1.1 clients will only send the absolute-form to a proxy.

**R32** (Section 3.2.3, MUST): When making a CONNECT request to establish a tunnel through one or more proxies, a client MUST send only the host and port of the tunnel destination as the request-target.

**R33** (Section 3.2.4, MUST): When a client wishes to request OPTIONS for the server as a whole, as opposed to a specific named resource of that server, the client MUST send only "*" (%x2A) as the request-target.

**R34** (Section 3.2.4, MUST): If a proxy receives an OPTIONS request with an absolute-form of request-target in which the URI has an empty path and no query component, then the last proxy on the request chain MUST send a request-target of "*" when it forwards the request to the indicated origin server.

**R35** (Section 3.3, MAY): A server that can uniquely identify an authority from the request context MAY use that identity as a default without this risk.

**R36** (Section 4, MAY): The first line of a response message is the status-line, consisting of the protocol version, a space (SP), the status code, and another space and ending with an OPTIONAL textual phrase describing the status code.

**R37** (Section 4, MAY): Although the status-line grammar rule requires that each of the component elements be separated by a single SP octet, recipients MAY instead parse on whitespace-delimited word boundaries and, aside from the line terminator, treat any form of whitespace as the SP separator while ignoring preceding or trailing whitespace; such whitespace includes one or more of the following octets: SP, HTAB, VT (%x0B), FF (%x0C), or bare CR.

**R38** (Section 4, SHOULD): A client SHOULD ignore the reason-phrase content because it is not a reliable channel for information (it might be translated for a given locale, overwritten by intermediaries, or discarded when the message is forwarded via other versions of HTTP).

**R39** (Section 4, MUST): A server MUST send the space that separates the status-code from the reason-phrase even when the reason-phrase is absent (i.e., the status-line would end with the space).

**R40** (Section 5.1, MUST): A server MUST reject, with a response status code of 400 (Bad Request), any received request message that contains whitespace between a header field name and colon.

**R41** (Section 5.1, MUST): A proxy MUST remove any such whitespace from a response message before forwarding the message downstream.

**R42** (Section 5.2, MUST): A sender MUST NOT generate a message that includes line folding (i.e., that has any field line value that contains a match to the obs-fold rule) unless the message is intended for packaging within the "message/http" media type.

**R43** (Section 5.2, MUST): A server that receives an obs-fold in a request message that is not within a "message/http" container MUST either reject the message by sending a 400 (Bad Request), preferably with a representation explaining that obsolete line folding is unacceptable, or replace each received obs-fold with one or more SP octets prior to interpreting the field value or forwarding the message downstream.

**R44** (Section 5.2, MUST): A proxy or gateway that receives an obs-fold in a response message that is not within a "message/http" container MUST either discard the message and replace it with a 502 (Bad Gateway) response, preferably with a representation explaining that unacceptable line folding was received, or replace each received obs-fold with one or more SP octets prior to interpreting the field value or forwarding the message downstream.

**R45** (Section 5.2, MUST): A user agent that receives an obs-fold in a response message that is not within a "message/http" container MUST replace each received obs-fold with one or more SP octets prior to interpreting the field value.

**R46** (Section 6.1, MUST): A recipient MUST be able to parse the chunked transfer coding (Section 7.1) because it plays a crucial role in framing messages when the content size is not known in advance.

**R47** (Section 6.1, MUST): A sender MUST NOT apply the chunked transfer coding more than once to a message body (i.e., chunking an already chunked message is not allowed).

**R48** (Section 6.1, MUST): If any transfer coding other than chunked is applied to a request's content, the sender MUST apply chunked as the final transfer coding to ensure that the message is properly framed.

**R49** (Section 6.1, MUST): If any transfer coding other than chunked is applied to a response's content, the sender MUST either apply chunked as the final transfer coding or terminate the message by closing the connection.

**R50** (Section 6.1, MAY): Any recipient along the request/response chain MAY decode the received transfer coding(s) or apply additional transfer coding(s) to the message body, assuming that corresponding changes are made to the Transfer-Encoding field value.

**R51** (Section 6.1, MAY): Transfer-Encoding MAY be sent in a response to a HEAD request or in a 304 (Not Modified) response (Section 15.4.5 of [HTTP]) to a GET request, neither of which includes a message body, to indicate that the origin server would have applied a transfer coding to the message body if the request had been an unconditional GET.

**R52** (Section 6.1, MUST): A server MUST NOT send a Transfer-Encoding header field in any response with a status code of 1xx (Informational) or 204 (No Content).

**R53** (Section 6.1, MUST): A server MUST NOT send a Transfer-Encoding header field in any 2xx (Successful) response to a CONNECT request (Section 9.3.6 of [HTTP]).

**R54** (Section 6.1, SHOULD): A server that receives a request message with a transfer coding it does not understand SHOULD respond with 501 (Not Implemented).

**R55** (Section 6.1, MUST): A client MUST NOT send a request containing Transfer-Encoding unless it knows the server will handle HTTP/1.1 requests (or later minor revisions); such knowledge might be in the form of specific user configuration or by remembering the version of a prior received response.

**R56** (Section 6.1, MUST): A server MUST NOT send a response containing Transfer- Encoding unless the corresponding request indicates HTTP/1.1 (or later minor revisions).

**R57** (Section 6.1, MAY): A server MAY reject a request that contains both Content-Length and Transfer-Encoding or process such a request in accordance with the Transfer-Encoding alone.

**R58** (Section 6.1, MUST): Regardless, the server MUST close the connection after responding to such a request to avoid the potential attacks.

**R59** (Section 6.1, MUST): A server or client that receives an HTTP/1.0 message containing a Transfer-Encoding header field MUST treat the message as if the framing is faulty, even if a Content-Length is present, and close the connection after processing the message.

**R60** (Section 6.2, MUST): A sender MUST NOT send a Content-Length header field in any message that contains a Transfer-Encoding header field.

**R61** (Section 6.3, MUST): A client MUST ignore any Content-Length or Transfer-Encoding header fields received in such a message.

**R62** (Section 6.3, MUST): An intermediary that chooses to forward the message MUST first remove the received Content-Length field and process the Transfer-Encoding (as described below) prior to forwarding the message downstream.

**R63** (Section 6.3, MUST): If a Transfer-Encoding header field is present in a request and the chunked transfer coding is not the final encoding, the message body length cannot be determined reliably; the server MUST respond with the 400 (Bad Request) status code and then close the connection.

**R64** (Section 6.3, MUST): If a message is received without Transfer-Encoding and with an invalid Content-Length header field, then the message framing is invalid and the recipient MUST treat it as an unrecoverable error, unless the field value can be successfully parsed as a comma-separated list (Section 5.6.1 of [HTTP]), all values in the list are valid, and all values in the list are the same (in which case, the message is processed with that single value used as the Content-Length field value).

**R65** (Section 6.3, MUST): If the unrecoverable error is in a request message, the server MUST respond with a 400 (Bad Request) status code and then close the connection.

**R66** (Section 6.3, MUST): If it is in a response message received by a proxy, the proxy MUST close the connection to the server, discard the received response, and send a 502 (Bad Gateway) response to the client.

**R67** (Section 6.3, MUST): If it is in a response message received by a user agent, the user agent MUST close the connection to the server and discard the received response.

**R68** (Section 6.3, MUST): If the sender closes the connection or the recipient times out before the indicated number of octets are received, the recipient MUST consider the message to be incomplete and close the connection.

**R69** (Section 6.3, SHOULD): Since there is no way to distinguish a successfully completed, close- delimited response message from a partially received message interrupted by network failure, a server SHOULD generate encoding or length-delimited messages whenever possible.

**R70** (Section 6.3, MAY): A server MAY reject a request that contains a message body but not a Content-Length by responding with 411 (Length Required).

**R71** (Section 6.3, SHOULD): Unless a transfer coding other than chunked has been applied, a client that sends a request containing a message body SHOULD use a valid Content-Length header field if the message body length is known in advance, rather than the chunked transfer coding, since some existing services respond to chunked with a 411 (Length Required) status code even though they understand the chunked transfer coding.

**R72** (Section 6.3, MUST): A user agent that sends a request that contains a message body MUST send either a valid Content-Length header field or use the chunked transfer coding.

**R73** (Section 6.3, MUST): A client MUST NOT use the chunked transfer coding unless it knows the server will handle HTTP/1.1 (or later) requests; such knowledge can be in the form of specific user configuration or by remembering the version of a prior received response.

**R74** (Section 6.3, MAY): If the final response to the last request on a connection has been completely received and there remains additional data to read, a user agent MAY discard the remaining data or attempt to determine if that data belongs as part of the prior message body, which might be the case if the prior message's Content-Length value is incorrect.

**R75** (Section 6.3, MUST): A client MUST NOT process, cache, or forward such extra data as a separate response, since such behavior would be vulnerable to cache poisoning.

**R76** (Section 7.1, MAY): The chunked transfer coding wraps content in order to transfer it as a series of chunks, each with its own size indicator, followed by an OPTIONAL trailer section containing trailer fields.

**R77** (Section 7.1, MUST): A recipient MUST be able to parse and decode the chunked transfer coding.

**R78** (Section 7.1, MUST): Therefore, recipients MUST anticipate potentially large hexadecimal numerals and prevent parsing errors due to integer conversion overflows or precision loss due to integer representation.

**R79** (Section 7.1, SHOULD): Their presence SHOULD be treated as an error.

**R80** (Section 7.1.1, MUST): A recipient MUST ignore unrecognized chunk extensions.

**R81** (Section 7.1.2, MAY): A recipient that removes the chunked coding from a message MAY selectively retain or discard the received trailer fields.

**R82** (Section 7.1.2, MUST): A recipient that retains a received trailer field MUST either store/ forward the trailer field separately from the received header fields or merge the received trailer field into the header section.

**R83** (Section 7.1.2, MUST): A recipient MUST NOT merge a received trailer field into the header section unless its corresponding header field definition explicitly permits and instructs how the trailer field value can be safely merged.

**R84** (Section 7.2, SHOULD): The presence of parameters with any of these compression codings SHOULD be treated as an error.

**R85** (Section 7.3, MUST): Registrations MUST include the following fields:

**R86** (Section 7.3, MUST): Names of transfer codings MUST NOT overlap with names of content codings (Section 8.4.1 of [HTTP]) unless the encoding transformation is identical, as is the case for the compression codings defined in Section 7.2.

**R87** (Section 7.3, SHOULD): Future registrations of transfer codings SHOULD NOT define parameters called "q" (case-insensitively) in order to avoid ambiguities.

**R88** (Section 7.3, MUST): Values to be added to this namespace require IETF Review (see Section 4.8 of [RFC8126]) and MUST conform to the purpose of transfer coding defined in this specification.

**R89** (Section 7.4, MUST): A client MUST NOT send the chunked transfer coding name in TE; chunked is always acceptable for HTTP/1.1 recipients.

**R90** (Section 7.4, MAY): When multiple transfer codings are acceptable, the client MAY rank the codings by preference using a case-insensitive "q" parameter (similar to the qvalues used in content negotiation fields; see Section 12.4.2 of [HTTP]).

**R91** (Section 7.4, MUST): Since the TE header field only applies to the immediate connection, a sender of TE MUST also send a "TE" connection option within the Connection header field (Section 7.6.1 of [HTTP]) in order to prevent the TE header field from being forwarded by intermediaries that do not support its semantics.

**R92** (Section 8, MAY): A server that receives an incomplete request message, usually due to a canceled request or a triggered timeout exception, MAY send an error response prior to closing the connection.

**R93** (Section 8, MUST): A client that receives an incomplete response message, which can occur when a connection is closed prematurely or when decoding a supposedly chunked transfer coding fails, MUST record the message as incomplete.

**R94** (Section 9.2, MUST): A client that has more than one outstanding request on a connection MUST maintain a list of outstanding requests in the order sent and MUST associate each received response message on that connection to the first outstanding request that has not yet received a final (non- 1xx) response.

**R95** (Section 9.2, MUST): If a client receives data on a connection that doesn't have outstanding requests, the client MUST NOT consider that data to be a valid response; the client SHOULD close the connection, since message delimitation is now ambiguous, unless the data consists only of one or more CRLF (which can be discarded per Section 2.2).

**R96** (Section 9.3, SHOULD): HTTP implementations SHOULD support persistent connections.

**R97** (Section 9.3, MUST): A client that does not support persistent connections MUST send the "close" connection option in every request message.

**R98** (Section 9.3, MUST): A server that does not support persistent connections MUST send the "close" connection option in every response message that does not have a 1xx (Informational) status code.

**R99** (Section 9.3, MAY): A client MAY send additional requests on a persistent connection until it sends or receives a "close" connection option or receives an HTTP/1.0 response without a "keep-alive" connection option.

**R100** (Section 9.3, MUST): A server MUST read the entire request message body or close the connection after sending its response; otherwise, the remaining data on a persistent connection would be misinterpreted as the next request.

**R101** (Section 9.3, MUST): Likewise, a client MUST read the entire response message body if it intends to reuse the same connection for a subsequent request.

**R102** (Section 9.3, MUST): A proxy server MUST NOT maintain a persistent connection with an HTTP/1.0 client (see Appendix C.2.2 for information and discussion of the problems with the Keep-Alive header field implemented by many HTTP/1.0 clients).

**R103** (Section 9.3.2, MAY): A client that supports persistent connections MAY "pipeline" its requests (i.e., send multiple requests without waiting for each response).

**R104** (Section 9.3.2, MUST): A server MAY process a sequence of pipelined requests in parallel if they all have safe methods (Section 9.2.1 of [HTTP]), but it MUST send the corresponding responses in the same order that the requests were received.

**R105** (Section 9.3.2, SHOULD): A client that pipelines requests SHOULD retry unanswered requests if the connection closes before it receives all of the corresponding responses.

**R106** (Section 9.3.2, MUST): When retrying pipelined requests after a failed connection (a connection not explicitly closed by the server in its last complete response), a client MUST NOT pipeline immediately after connection establishment, since the first remaining request in the prior pipeline might have caused an error response that can be lost again if multiple requests are sent on a prematurely closed connection (see the TCP reset problem described in Section 9.6).

**R107** (Section 9.3.2, SHOULD): A user agent SHOULD NOT pipeline requests after a non-idempotent method, until the final response status code for that method has been received, unless the user agent has a means to detect and recover from partial failure conditions involving the pipelined sequence.

**R108** (Section 9.3.2, MAY): An intermediary that receives pipelined requests MAY pipeline those requests when forwarding them inbound, since it can rely on the outbound user agent(s) to determine what requests can be safely pipelined.

**R109** (Section 9.3.2, SHOULD): If the inbound connection fails before receiving a response, the pipelining intermediary MAY attempt to retry a sequence of requests that have yet to receive a response if the requests all have idempotent methods; otherwise, the pipelining intermediary SHOULD forward any received responses and then close the corresponding outbound connection(s) so that the outbound user agent(s) can recover accordingly.

**R110** (Section 9.5, SHOULD): A client or server that wishes to time out SHOULD issue a graceful close on the connection.

**R111** (Section 9.5, SHOULD): Implementations SHOULD constantly monitor open connections for a received closure signal and respond to it as appropriate, since prompt closure of both sides of a connection enables allocated system resources to be reclaimed.

**R112** (Section 9.5, MAY): A client, server, or proxy MAY close the transport connection at any time.

**R113** (Section 9.5, SHOULD): A server SHOULD sustain persistent connections, when possible, and allow the underlying transport's flow-control mechanisms to resolve temporary overloads rather than terminate connections with the expectation that clients will retry.

**R114** (Section 9.5, SHOULD): A client sending a message body SHOULD monitor the network connection for an error response while it is transmitting the request.

**R115** (Section 9.5, SHOULD): If the client sees a response that indicates the server does not wish to receive the message body and is closing the connection, the client SHOULD immediately cease transmitting the body and close its side of the connection.

**R116** (Section 9.6, SHOULD): A sender SHOULD send a Connection header field (Section 7.6.1 of [HTTP]) containing the "close" connection option when it intends to close a connection.

**R117** (Section 9.6, MUST): A client that sends a "close" connection option MUST NOT send further requests on that connection (after the one containing the "close") and MUST close the connection after reading the final response message corresponding to this request.

**R118** (Section 9.6, MUST): A server that receives a "close" connection option MUST initiate closure of the connection (see below) after it sends the final response to the request that contained the "close" connection option.

**R119** (Section 9.6, SHOULD): The server SHOULD send a "close" connection option in its final response on that connection.

**R120** (Section 9.6, MUST): The server MUST NOT process any further requests received on that connection.

**R121** (Section 9.6, MUST): A server that sends a "close" connection option MUST initiate closure of the connection (see below) after it sends the response containing the "close" connection option.

**R122** (Section 9.6, MUST): The server MUST NOT process any further requests received on that connection.

**R123** (Section 9.6, MUST): A client that receives a "close" connection option MUST cease sending requests on that connection and close the connection after reading the response message containing the "close" connection option; if additional pipelined requests had been sent on the connection, the client SHOULD NOT assume that they will be processed by the server.

**R124** (Section 9.7, MUST): All HTTP data MUST be sent as TLS "application data" but is otherwise treated like a normal connection for HTTP (including potential reuse as a persistent connection).

**R125** (Section 9.8, SHOULD): When encountering an incomplete close, a client SHOULD treat as completed all requests for which it has received either

**R126** (Section 9.8, SHOULD): A client detecting an incomplete close SHOULD recover gracefully.

**R127** (Section 9.8, MUST): Clients MUST send a closure alert before closing the connection.

**R128** (Section 9.8, MAY): Clients that do not expect to receive any more data MAY choose not to wait for the server's closure alert and simply close the connection, thus generating an incomplete close on the server side.

**R129** (Section 9.8, SHOULD): Servers SHOULD be prepared to receive an incomplete close from the client, since the client can often locate the end of server data.

**R130** (Section 9.8, MUST): Servers MUST attempt to initiate an exchange of closure alerts with the client before closing the connection.

**R131** (Section 9.8, MAY): Servers MAY close the connection after sending the closure alert, thus generating an incomplete close on the client side.

**R132** (Section 10.1, MUST): A recipient of "message/http" data MUST replace any obsolete line folding with one or more SP characters when the message is consumed.

