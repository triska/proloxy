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

:- dynamic request_prefix_uri/3.
:- http_handler(/, custom_target, [prefix]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Configuration
   =============

   Proloxy uses rules to relay requests to different web services.

   To make a new web service available, add a request_prefix_uri/3
   clause, relating an HTTP request to a prefix and target URI. The
   first matching clause is used. The prefix is needed to rewrite HTTP
   redirects that the target server emits, so that the next client
   request is again relayed to the intended target service.

   For example, by adding the Prolog rule:

       request_prefix_uri(Request, '', TargetURI) :-
               memberchk(request_uri(URI), Request),
               atomic_list_concat(['http://localhost:4041',URI], TargetURI).

   all requests are relayed to a local web server on port 4041,
   passing the original request path unmodified. This different server
   can for example host the site's main page.

   You can add new target services, using this configuration element.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Sample configuration.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% Custom rule for choosing a target URI.

request_prefix_uri(Request, '', TargetURI) :-
        memberchk(request_uri(URI), Request),
        atomic_list_concat(['http://localhost:4041',URI], TargetURI).



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Main logic. Relay requests based on the defined rules.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%:- initialization http_daemon.

:- debug(proloxy).

custom_target(Request) :-
        debug(proloxy, "request: ~q\n", [Request]),
        (   request_prefix_uri(Request, Prefix, TargetURI) ->
            true % commit to first matching clause
        ;   throw(no_matching_clause)
        ),
        memberchk(request_uri(URI), Request),
        memberchk(method(Method0), Request),
        debug(proloxy, "target URI: ~q\n", [TargetURI]),
        method_pure(Method0, Method),
        proxy(Method, Prefix, URI, TargetURI, Request).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Entry point for requests that are relayed based on http_handler/3
   directives.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   1) is request_uri guaranteed to be present? how else to obtain the
      request URI? (please document it if it is the case)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

prefix_target(Prefix, To, Request) :-
        debug(proloxy, "request: ~q\n", [Request]),
        memberchk(request_uri(URI0), Request),
        atom_concat(Prefix, URI, URI0),
        memberchk(method(Method0), Request),
        atomic_list_concat([To,URI], TargetURI),
        debug(proloxy, "target: ~q\n", [TargetURI]),
        method_pure(Method0, Method),
        proxy(Method, Prefix, URI0, TargetURI, Request).

proxy(data(Method), _, _, TargetURI, Request) :-
        read_data(Request, Data),
        http_open(TargetURI, In, [method(Method), post(Data),
                                  % cert_verify_hook(cert_accept_any),
                                  header(content_type, ContentType)]),
        call_cleanup(read_string(In, _, Bytes),
                     close(In)),
        throw(http_reply(bytes(ContentType, Bytes))).
proxy(other(Method), Prefix, URI, TargetURI, _) :-
        http_open(TargetURI, In, [method(Method),
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

http:http_address -->
	html(address([a(href('https://github.com/triska/proloxy'),
                        'Proloxy')])).
