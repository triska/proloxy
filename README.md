# Proloxy

## Reverse HTTP proxy written in Prolog

A [reverse HTTP proxy](https://en.wikipedia.org/wiki/Reverse_proxy)
relays requests to different web services. The main advantage is
*isolation of concerns*: You can run different and independent web
services and serve them under a common umbrella URL.

![Proloxy: Reverse HTTP proxy written in SWI-Prolog](proloxy.png)

## Configuration

Proloxy uses rules to relay requests to different web services.
There are two ways to make new web services available, with the
first having priority over the second:

1. Add a `http_handler/3` directive of the form:

       :- http_handler(Prefix, prefix_target(Prefix, Target), [prefix]).

   This relays requests for `Prefix/Path` to `Target/Path`.

   For example, the directive:

       :- http_handler('/rits',
                       prefix_target('/rits', 'http://localhost:4043'),
                       [prefix]).

   relays `/rits/File` to `http://localhost:4043/File`, where a
   [RITS](https://github.com/triska/rits) HTTP server handles the
   request.

   Note in particular that:

   -) Prefix is _removed_ from the original path, i.e., the target
      server does *not* see the Prefix part of the URI.

   -) The _same_ prefix that is used for selecting the target
      service is also specified as the prefix in `prefix_target/2`.
      This is because HTTP _redirects_ that the target server emits
      need to be rewritten. For example, if the RITS server
      redirects to <R>, then Proloxy needs to redirect the client
      to /rits/<R>, so that the next client request is again
      relayed to the RITS server.

   The directive with the longest matching Prefix is used.

2. Add a custom `request_prefix_uri/3` clause, relating the HTTP
   request to a prefix (which is, as above, needed to rewrite HTTP
   redirects that the target server emits) and target URI. The
   first matching clause is used.

   For example, by adding the Prolog rule:

       request_prefix_uri(Request, '', TargetURI) :-
               memberchk(request_uri(URI), Request),
               atomic_list_concat(['http://localhost:4041',URI], TargetURI).

   all requests are relayed to a local web server on port&nbsp;4041,
   passing the original request path unmodified. This different server
   can for example host the site's main page.

   Note that this can also be expressed with `http_handler/3`:

       :- http_handler(/,
                       prefix_target('', 'http://localhost:4041'),
                       [prefix]).

   However, `request_prefix_uri/3` gives you more flexibility and lets
   you for example also relay requests to different services based on
   the Host field of the request.

You can add new target services, using these two configuration elements.

Please see the source file ([**proloxy.pl**](proloxy.pl)) for more
information.

Proloxy **requires** SWI-Prolog &gt;= 7.3.12.
