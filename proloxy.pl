/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   HTTP reverse proxy.

   Currently very preliminary:

   It only directs <File> to localhost:4041/<File>, that is, it just
   directs everything to a different port.

   Eventually, we want to relay requests to different ports based on
   flexible rules.

   There are some pending issues in the HTTP libraries of SWI-Prolog.
   As soon as they are fixed, we can simplify the code a lot.

   Written by Markus Triska, July 2015.
   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(proloxy,
	  [ proloxy/1			% +Port
	  ]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_client)).
:- use_module(library(memfile)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_error)).

%:- initialization http_daemon.

:- http_handler(/, handle_request, [prefix]).

:- debug(myhttp).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   1) is request_uri guaranteed to be present? how else to obtain the
      request URI? (please document it if it is the case)

   2)  http_get('localhost:4041', R, []) --> false
       http_get('127.0.0.1:4041', R, []) --> works
          --- solution: use http://localhost:4041

   3) http_reply(file(text/plain, '/proloxy.pl'), user_output, []).
      ==> fails. --- issue filed.

   4) Why is there no codes() option in throw(http_reply) ? This would
      allow us to get rid of the mem files. --- issue filed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


handle_request(Request) :-
        debug(myhttp, "request: ~q\n", [Request]),
        memberchk(request_uri(URI), Request),
        atomic_list_concat(['http://127.0.0.1:4041',URI], Target),
        debug(myhttp, "target: ~q\n", [Target]),
        % Strategy: read everything to a memory file, then serve that file
        (   memberchk(method(get), Request) ->
            new_memory_file(MemFile),
            open_memory_file(MemFile, write, MemOut, [encoding(octet)]),
            http_get(Target, _, [to(stream(MemOut)),reply_header(Hs)]),
            close(MemOut),
            size_memory_file(MemFile, Size),
            debug(myhttp, "size: ~q\n", [Size]),
            open_memory_file(MemFile, read, MemIn, [encoding(octet)]),
            (   memberchk(content_type(MimeType), Hs) ->
                throw(http_reply(stream(MemIn, Size), [content_type(MimeType)]))
            ;   throw(http_reply(stream(MemIn, Size), []))
            )
        ;   memberchk(method(post), Request) ->
            %memberchk(content_length(Length), Request),
            %memberchk(input(Stream), Request),
            http_read_data(Request, Codes, [to(codes)]),
            debug(myhttp, "read: ~s\n", [Codes]),
            new_memory_file(MemFile),
            open_memory_file(MemFile, write, MemOut, [encoding(octet)]),
            memberchk(content_type(MimeType1), Request),
            debug(myhttp, "\n\nmime type: ~q\n\n", [MimeType1]),

            % TODO: if I use MimeType1 instead of specifying
            % 'application/json', I get, on the RITS side: error("Domain
            % error: `mimetype' expected, found `'application/json;
            % charset=utf-8; charset=UTF-8''")). --- issue filed.

            http_post(Target, codes('application/json',Codes), _,
                      [to(stream(MemOut)),
                       reply_header(Hs)]),
            debug(myhttp, "header: ~q\n", [Hs]),
            memberchk(content_type(MimeType), Hs),
            close(MemOut),
            debug(myhttp, "\n\nSecond mime type: ~q\n\n", [MimeType]),
            size_memory_file(MemFile, Size),
            open_memory_file(MemFile, read, MemIn, [encoding(octet)]),
            debug(myhttp, "size: ~q\n", [Size]),
            throw(http_reply(stream(MemIn,Size), [content_type(MimeType)]))
        ;   throw(method_not_supported)
        ).


proloxy(Port) :-
	http_server(http_dispatch, [port(Port)]).

