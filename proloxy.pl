/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Proloxy: HTTP reverse proxy written in Prolog.

   This lets you relay traffic to different web servers based on rules.

   See config.pl for a sample configuration.

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
          [ proloxy/1                  % +Port
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

:- dynamic request_prefix_target/3.
:- http_handler(/, custom_target, [prefix]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Main logic. Relay requests based on the defined rules.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- initialization http_daemon.

:- debug(proloxy).

custom_target(Request) :-
        debug(proloxy, "request: ~q\n", [Request]),
        (   user:request_prefix_target(Request, Prefix, TargetURI) ->
            % commit to first matching clause
            memberchk(request_uri(URI), Request),
            memberchk(method(Method0), Request),
            debug(proloxy, "target URI: ~q\n", [TargetURI]),
            method_pure(Method0, Method),
            proxy(Method, Prefix, URI, TargetURI, Request)
        ;   throw(http_reply(unavailable(p([tt('request_prefix_target/3'),
                                            ': No matching rule for ~q'-[Request]]))))
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Relay request to TargetURI. Rewrite the response if necessary.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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
