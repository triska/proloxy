# Proloxy

## Reverse HTTP proxy written in Prolog

A [reverse proxy](https://en.wikipedia.org/wiki/Reverse_proxy)
relays requests to different web services. The main advantage is
*isolation of concerns*: You can run different and independent web
services and serve them under a common umbrella URL.

![Proloxy: Reverse HTTP proxy written in SWI-Prolog](proloxy.png)

## Configuration

Proloxy uses Prolog rules to relay requests to different web services.

To make a new web service available, add a clause to the predicate
`request_prefix_target(+Request, -Prefix, -URI)`. This predicate is
called by Proloxy. It is given the HTTP request, and must compute a
prefix and a target URI. The prefix is needed to rewrite HTTP
redirects that the target server emits, so that the next client
request is again relayed to the intended target service.

The first matching clause is used.

For example, by adding the Prolog rule:

    request_prefix_target(Request, '', TargetURI) :-
            memberchk(request_uri(URI), Request),
            atomic_list_concat(['http://localhost:3031',URI], TargetURI).

all requests are relayed to a local web server on port 3031,
passing the original request path unmodified. This different server
can for example host the site's main page.

You can add new target services, using this configuration element.

These clauses can be stored in custom rule files, and you can load
them using additional `-l` options on the command line.

Proloxy **requires** SWI-Prolog &gt;= 7.3.12.
