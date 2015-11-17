/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   HTTP reverse proxy.

   This lets you relay traffic to different web servers, reachable via
   dedicated paths and a common umbrella URL.

   Proloxy needs SWI-Prolog >= 7.3.12.

   Copyright (c) 2015 Markus Triska (triska@metalevel.at)

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(proloxy,
          [ proloxy/1,                  % +Port
            proloxy_https/1             % +Port
          ]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_server_files)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Configuration
   =============

   Different web services are reachable via different paths.

   For example, in the situation below we relay requests for:

      /rits/<File>

   to http://localhost:4043/<File>, where a RITS HTTP server handles
   the request. Note that '/rits' needs to be specified as the prefix
   in handle_request/2 too, because redirects that the RITS server
   emits need to be rewritten: If the RITS server redirects to <R>,
   then the proxy needs to redirect the client to /rits/<R>, so that
   the new request is again delegated to the RITS server.

   In the example below, all other requests are relayed to a local web
   server on port 4041, passing the original request path unmodified.
   This different server can for example host the site's main page.

   You can add new services below, using a dedicated prefix for each.

   An additional feature: You can easily enable HTTPS for all web
   servers at once by making Proloxy itself use HTTPS for the client
   connection (the Proloxy<->servers connections stay local).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- http_handler(/,
                handle_request('', 'http://localhost:4041'),
                [prefix]).

:- http_handler('/rits',
                handle_request('/rits', 'http://localhost:4043'),
                [prefix]).

%:- initialization http_daemon.

:- debug(proloxy).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   1) is request_uri guaranteed to be present? how else to obtain the
      request URI? (please document it if it is the case)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


handle_request(Prefix, To, Request) :-
        debug(proloxy, "request: ~q\n", [Request]),
        memberchk(request_uri(URI0), Request),
        atom_concat(Prefix, URI, URI0),
        memberchk(method(Method0), Request),
        atomic_list_concat([To,URI], Target),
        debug(proloxy, "target: ~q\n", [Target]),
        method_pure(Method0, Method),
        proxy(Method, Prefix, URI0, Target, Request).

proxy(data(Method), _, _, Target, Request) :-
        read_data(Request, Data),
        http_open(Target, In, [method(Method), post(Data),
                               % cert_verify_hook(cert_accept_any),
                               header(content_type, ContentType)]),
        call_cleanup(read_string(In, _, Bytes),
                     close(In)),
        throw(http_reply(bytes(ContentType, Bytes))).
proxy(other(Method), Prefix, URI, Target, _) :-
        http_open(Target, In, [method(Method),
                               % cert_verify_hook(cert_accept_any),
                               redirect(false),
                               header(location, Location0),
                               status_code(Code),
                               header(content_type, ContentType)]),
        call_cleanup(read_string(In, _, Bytes),
                     close(In)),
        (   redirect_code(Code, Status) ->
            (   uri_is_global(Location0) ->
                Location = Location0
            ;   atom_concat(Prefix, Location0, Location)
            ),
            Reply =.. [Status,Location],
            throw(http_reply(Reply))
        ;   Code == 404 ->
            throw(http_reply(not_found(URI)))
        ;   throw(http_reply(bytes(ContentType, Bytes)))
        ).

redirect_code(301, moved).
redirect_code(302, moved_temporary).
redirect_code(303, see_other).

read_data(Request, bytes(ContentType, Bytes)) :-
        memberchk(input(In), Request),
        memberchk(content_type(ContentType), Request),
        (   memberchk(content_length(Len), Request) ->
            read_string(In, Len, Bytes)
        ;   read_string(In, _, Bytes)
        ).

method_pure(M0, M) :- once(defaulty_pure(M0, M)).

defaulty_pure(post, data(post)).
defaulty_pure(put, data(put)).
defaulty_pure(M, other(M)).

proloxy(Port) :-
	http_server(http_dispatch, [port(Port)]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   proloxy_https/1 is like proloxy/1, but uses HTTPS.

   You need a certificate and key to make this work.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

proloxy_https(Port) :-
	http_server(http_dispatch,
		    [ port(Port),
		      ssl([ certificate_file('server.crt'),
			    key_file('server.key'),
			    password("your_password")
			  ])
		    ]).

http:http_address -->
	html(address([a(href('https://github.com/triska/proloxy'),
                        'Proloxy')])).
