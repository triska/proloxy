# Proloxy: Prolog Reverse Proxy

## Introduction

A [reverse proxy](https://en.wikipedia.org/wiki/Reverse_proxy)
relays requests to different web services. The main advantage is
*isolation of concerns*: You can run different and independent web
services and serve them under a common umbrella URL.

![Proloxy: Reverse proxy written in Prolog](proloxy.png)

## Configuration

Proloxy uses **Prolog rules** to relay requests to different web services.

You store these rules in a separate (Prolog) file. See
[config.pl](config.pl) for a sample configuration file with two rules.

To make a new web service available, add a clause to the predicate
`request_prefix_target(+Request, -Prefix, -URI)`. This predicate is
called by Proloxy. It is *given* the [HTTP
request](http://eu.swi-prolog.org/pldoc/man?predicate=http_read_request/2),
and must relate it to a target&nbsp;URI and a&nbsp;prefix, using
arbitrary Prolog code. The prefix is needed to rewrite
HTTP&nbsp;*redirects* that the target server emits, so that the next
client request is again relayed to the intended target service. The
**first matching clause** is used.

For example, by adding the Prolog rule:

    request_prefix_target(Request, '', TargetURI) :-
            memberchk(request_uri(URI), Request),
            atomic_list_concat(['http://localhost:3031',URI], TargetURI).

all requests are relayed to a local web server on port 3031,
passing the original request path unmodified. This different server
can for example host the site's main page.

You can add new target services, using this configuration element.

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

## Running Proloxy

You can run Proloxy as a **Unix daemon**. See the [SWI-Prolog
documentation](http://eu.swi-prolog.org/pldoc/man?section=httpunixdaemon)
for invocation options.

In the following, assume that your proxy rules are stored in the file
called `rules.pl`.

To run Proloxy as a Unix daemon on the standard HTTP port (80) as user
`web`, use:

    sudo swipl rules.pl proloxy.pl --user=web

To run the process in the foreground and with a Prolog toplevel, use:

<pre>
sudo swipl rules.pl proloxy.pl --user=web <b>--interactive</b> 
</pre>

You can also use a different port that does not need root privileges:

<pre>
swipl rules.pl proloxy.pl --interactive <b>--port=3040</b>
</pre>

You can also easily enable **HTTPS** with the `--https`, `--certfile`
and `--keyfile` options.


Proloxy requires SWI-Prolog <b>7.3.12</b> or later.
