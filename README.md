# Proloxy: Prolog Reverse Proxy

## Introduction

A [reverse proxy](https://en.wikipedia.org/wiki/Reverse_proxy)
relays requests to different web services. The main advantage is
*isolation of concerns*: You can run different and independent web
services and serve them under a common umbrella URL.

![Proloxy: Reverse proxy written in Prolog](http://www.metalevel.at/proloxy/proloxy.svg)

Proloxy requires SWI-Prolog <b>7.5.8</b> or later.

## Configuration

Proloxy uses an extensible **Prolog predicate** to relay requests to
different web services: For each arriving HTTP&nbsp;request, Proloxy
calls the predicate `request_prefix_target(+Request, -Prefix, -Target)`.
Its arguments are:

- `Request` is the instantiated
  [HTTP request](http://eu.swi-prolog.org/pldoc/man?predicate=http_read_request/2).
- `Prefix` is prepended to relative paths in HTTP&nbsp;*redirects*
  that the target service emits, so that the next client request is
  again relayed to the intended target service.
- `Target` is the URI of the target service.

You configure Proloxy by providing a **Prolog file** that contains the
definition of `request_prefix_target/3` and any additional predicates
and directives you need. For each web service you want to make
available, add a clause of `request_prefix_target/3` to relate an
instantiated HTTP&nbsp;request to a&nbsp;prefix and the
desired&nbsp;target. Each clause may use arbitrary Prolog code to
analyse the request and form the target.

When dispatching an HTTP request, Proloxy considers the clauses of
`request_prefix_target/3` in the order they appear in your
configuration file and **commits** to the **first clause that
succeeds**. It relays the request to the computed target, and then
sends the target's response to the client.

For example, the following clause relays *all* requests to a local web
server on port 3031, passing along the original request path. The
target server can for example host the site's main page, to be used if
no other rules apply:

    request_prefix_target(Request, '', Target) :-
            memberchk(request_uri(URI), Request),
            atomic_list_concat(['http://localhost:3031',URI], Target).

[**config.pl**](config.pl) shows a sample configuration file that uses
Prolog rules to dispatch requests to two different web services.

### Virtual hosts

The rule language is general enough to express **virtual hosts**. In
the case of *name-based* virtual hosts, this means that you dispatch
requests to different web services based on the *domain name* that is
used to access your site.

For example, to dispatch all requests of users who access your server
via `your-domain.com` to a web server running on port&nbsp;4040 (while
leaving the path unchanged), use:

    request_prefix_target(Request, '', Target) :-
            memberchk(host('your-domain.com'), Request),
            memberchk(request_uri(URI), Request),
            atomic_list_concat(['http://localhost:4040',URI], Target).

Using this method, you can host multiple domains with a single Proloxy
instance, dispatching requests to different underlying services.

### Redirections and other replies

You can also use the predicates `http_404/2` and `http_redirect/3`
from
[`library(http/http_dispatch)`](http://eu.swi-prolog.org/pldoc/man?section=httpdispatch)
in your configuration files.

For example, the following snippet responds with "HTTP 404
not&nbsp;found" if the URI contains `.git`:

    :- use_module(library(http/http_dispatch)).

    request_prefix_target(Request, _, _) :-
            memberchk(request_uri(Path), Request),
            sub_atom(Path, _, _, _, '.git/'),
            http_404([], Request).

In some cases, it is convenient to respond directly with plain text or
HTML&nbsp;content *instead* of relaying the request to a different web
service. If a clause of `request_prefix_target/3` emits any text on
standout output, then this output is sent to the client as the
HTTP&nbsp;response. Such responses typically start with `Content-type:
text/plain` (or&nbsp;`text/html`), followed by two&nbsp;newlines and
the body of the reply. In rules that emit output, `Target` must be the
atom&nbsp;`-` to avoid relaying the request to a different service.

Proloxy provides the predicate `output_from_process(+Program, +Args)`
to emit process output (from `stdout` and&nbsp;`stderr`) on standard
output. For example, we can configure Proloxy to show the system's
uptime when the URL&nbsp;`/uptime` is accessed:

    request_prefix_target(Request, _, -) :-
            memberchk(request_uri('/uptime'), Request),
            format("Content-type: text/plain; charset=utf-8~n~n"),
            output_from_process('/usr/bin/uptime', []).

Auxiliary programs and scripts can be conveniently invoked with
this&nbsp;method.

### Relaying header fields

The extensible predicate `transmit_header_field/1` allows you to relay
header fields that the target service emits to the client. The
argument is the name of the header field you want to transmit if it
exists in the target's response. For example, you can put the
following in&nbsp;`config.pl`:

    transmit_header_field(last_modified).

The name of the header field is matched case-insensitively and
underscore&nbsp;(`_`) matches hyphen&nbsp;(`-`).

By default, Proloxy does _not_ relay _any_ response
header&nbsp;fields.

Here is a recommended configuration:

    transmit_header_field(cache_control).
    transmit_header_field(expires).
    transmit_header_field(last_modified).
    transmit_header_field(pragma).

Further, the extensible predicate `add_header/1` lets you add custom
header fields to the response. An example is:

    add_header(strict_transport_security('max-age=63072000; includeSubdomains')).

This enables HTTP Strict Transport Security (HSTS), which is useful
when running HTTPS servers.

## Testing the configuration

Since each configuration file is also a valid Prolog program, you can
easily **test** your configuration. Consulting the Prolog program in
SWI-Prolog lets you detect syntax errors and singleton variables in
your configuration file. To test whether HTTP requests are dispatched
as you intend, query `request_prefix_target/3`. For example:

<pre>
$ swipl config.pl
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.3.14)
...

?- once(request_prefix_target(<b>[request_uri(/)]</b>, P, T)).
P = '',
T = 'http://localhost:3031/'.

?- once(request_prefix_target(<b>[request_uri('/rits/demo.html')]</b>, P, T)).
P = '/rits',
T = 'http://localhost:4040/demo.html'.
</pre>

Note that:

1. we are using `once/1` to commit to the *first clause that succeeds*.
2. we are simulating an actual HTTP request, using a list of header fields.
3. the answers tell us how the given HTTP requests are dispatched.

The ability to conveniently test your configuration is a nice
property, and a natural consequence of using Prolog as the
configuration language. You can also write unit tests for your
configuration and therefore easily detect regressions.

## Running Proloxy

You can run Proloxy as a **Unix daemon**. See the [SWI-Prolog
documentation](http://eu.swi-prolog.org/pldoc/man?section=httpunixdaemon)
for invocation options.

In the following, assume that your proxy rules are stored in the file
called `config.pl`.

To run Proloxy as a Unix daemon on the standard HTTP port&nbsp;(80) as
user&nbsp;`web`, use:

    $ sudo swipl config.pl proloxy.pl --user=web

To run the process in the foreground and with a Prolog toplevel, use:

<pre>
$ sudo swipl config.pl proloxy.pl --user=web <b>--interactive</b> 
</pre>

You can also use a different port that does not need root privileges:

<pre>
$ swipl config.pl proloxy.pl --interactive <b>--port=3040</b>
</pre>

## Launching Proloxy on system startup

[**proloxy.service**](proloxy.service) is a sample **systemd**
unit&nbsp;file that runs Proloxy on system startup. Adapt the paths
and options as needed, copy the file to&nbsp;`/etc/systemd/system/`
and install it using:

    $ sudo systemctl enable /etc/systemd/system/proloxy.service
    $ sudo systemctl start proloxy.service

## Security: Run HTTPS servers

You can run Proloxy as an&nbsp;**HTTPS** server and thus encrypt
traffic for all hosted services at&nbsp;once.

See [**LetSWICrypt**](https://github.com/triska/letswicrypt) for more
information.

A common use case when using HTTPS is to run a second Proloxy instance
as a regular HTTP&nbsp;server on port&nbsp;80 to redirect each request
for&nbsp;http://*X* to&nbsp;**https**://*X*. You can do this with the
following configuration file for the HTTP&nbsp;server:

    :- use_module(library(http/http_dispatch)).

    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       Redirect each request X to https://X
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    request_prefix_target(Request, _, _) :-
            memberchk(request_uri(URI), Request),
            memberchk(host(Host), Request),
            atomic_list_concat(['https://',Host,URI], Target),
            http_redirect(moved, Target, Request).

## WebSocket connections

Proloxy supports proxying of WebSocket connections.

As an example, consider making
[noVNC](https://github.com/kanaka/noVNC) available via&nbsp;`/vnc/`,
assuming that noVNC listens on port&nbsp;6080. The following clauses
accomplish the configuration:

    request_prefix_target(Request, '/vnc', Target) :-
            memberchk(request_uri(URI), Request),
            atom_concat('/vnc', Rest, URI),
            atomic_list_concat(['http://localhost:6080',Rest], Target).

    request_prefix_target(Request, '/websockify', Target) :-
            memberchk(request_uri(URI), Request),
            atom_concat('/websockify', Rest, URI),
            atomic_list_concat(['ws://localhost:6080',Rest], Target).

WebSocket connections are automatically detected via the
`Upgrade: websocket` and other header fields.

## Serving very large files

In typical use cases, Proloxy relays requests to *other*
web&nbsp;servers, and sends their answers to the client. The overhead
is typically negligible, since the other web services usually reside
on the same&nbsp;machine.

However, if a web server sends very large files in response to some
requests, Proloxy may not have enough global&nbsp;stack space to
collect the&nbsp;response.

In such cases, one solution is to configure Proloxy so that such
large&nbsp;files are sent directly by Proloxy, without involving a
different web&nbsp;service. For example, the following snippet
configures Proloxy to directly send any files (such as
ISO&nbsp;images) that are located in `/home/web/iso`, and are accessed
via&nbsp;`/iso/`.

    :- use_module(library(http/http_dispatch)).

    request_prefix_target(Request, '', _) :-
            memberchk(request_uri(URI), Request),
            atom_concat('/iso/', Rest, URI),
            http_safe_file(Rest, []),
            atom_concat('/home/web/iso', URI, Path),
            http_reply_file(Path, [unsafe(true)], Request).
