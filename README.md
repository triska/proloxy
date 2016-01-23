# Proloxy: Prolog Reverse Proxy

## Introduction

A [reverse proxy](https://en.wikipedia.org/wiki/Reverse_proxy)
relays requests to different web services. The main advantage is
*isolation of concerns*: You can run different and independent web
services and serve them under a common umbrella URL.

![Proloxy: Reverse proxy written in Prolog](http://www.metalevel.at/proloxy/proloxy.svg)

## Configuration

Proloxy uses **Prolog rules** to relay requests to different web services.

You store these rules in a separate (Prolog) file. See
[**config.pl**](config.pl) for a sample configuration file with two
rules.

The predicate `request_prefix_target(+Request, -Prefix, -URI)` is
called by Proloxy to dispatch HTTP requests. Its arguments are:

- `Request` is the instantiated
  [HTTP request](http://eu.swi-prolog.org/pldoc/man?predicate=http_read_request/2).
- `Prefix` is prepended to relative paths in HTTP&nbsp;*redirects*
  that the target service emits, so that the next client request is
  again relayed to the intended target service.
- `URI` is the URI of the target service.

To make a new web service available, add a clause that relates an
instantiated HTTP&nbsp;request to a target&nbsp;URI and a&nbsp;prefix.
The clause may use arbitrary Prolog code and any number of additional
predicates and libraries to analyse the request and form the target.

When dispatching an HTTP request, Proloxy considers the clauses of
`request_prefix_target/3` in the order they appear in your
configuration file and *commits* to the **first clause that
succeeds**. It relays the request to the computed target, and then
sends the target's response to the client.

For example, by adding the following Prolog rule, *all* requests are
relayed to a local web server on port 3031, passing the original
request path unmodified. This different server can for example host
the site's main page:

    request_prefix_target(Request, '', TargetURI) :-
            memberchk(request_uri(URI), Request),
            atomic_list_concat(['http://localhost:3031',URI], TargetURI).

You can add new target services, using this configuration element.

### A useful predicate: `atom_prefix_rest/3`

A useful predicate for dispatching requests is `atom_prefix_rest/3`.
It is true *iff* `Atom` starts with `Prefix`, followed by `Rest`:

    atom_prefix_rest(Atom, Prefix, Rest) :-
            sub_atom(Atom, 0, Len, _, Prefix),
            sub_atom(Atom, Len, _, 0, Rest).

This allows you to write rules like the following, dispatching for
example `/rits/demo.html` to `/demo.html` on a [RITS
server](https://github.com/triska/rits):

    request_prefix_target(Request, '/rits', TargetURI) :-
            memberchk(request_uri(URI), Request),
            atom_prefix_rest(URI, '/rits', Target),
            atomic_list_concat(['http://localhost:4040',Target], TargetURI).

### Virtual hosts

The rule language is general enough to express **virtual hosts**. In
the case of *name-based* virtual hosts, this means that you dispatch
requests to different web services based on the *domain name* that is
used to access your site.

For example, to dispatch all requests of users who access your server
via `your-domain.com` to a web server running on port&nbsp;4040 (while
leaving the path unchanged), use:

    request_prefix_target(Request, '', TargetURI) :-
            memberchk(host('your-domain.com'), Request),
            memberchk(request_uri(URI), Request),
            atomic_list_concat(['http://localhost:4040',URI], TargetURI).

Using this method, you can host multiple domains with a single Proloxy
instance, dispatching requests to different underlying services.

## Testing the configuration

Since the configuration is a Prolog program, you can easily
**test**&nbsp;it. Consulting the Prolog program in SWI-Prolog lets you
detect syntax errors and singleton variables in your configuration
file. To test whether HTTP requests are dispatched as you intend,
query `request_prefix_target/3`. For example:

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

To run Proloxy as a Unix daemon on the standard HTTP port (80) as user
`web`, use:

    sudo swipl config.pl proloxy.pl --user=web

To run the process in the foreground and with a Prolog toplevel, use:

<pre>
sudo swipl config.pl proloxy.pl --user=web <b>--interactive</b> 
</pre>

You can also use a different port that does not need root privileges:

<pre>
swipl config.pl proloxy.pl --interactive <b>--port=3040</b>
</pre>

You can also easily enable **HTTPS** with the `--https`, `--certfile`
and `--keyfile` options.


Proloxy requires SWI-Prolog <b>7.3.12</b> or later.
