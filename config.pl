/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Proloxy sample configuration file.

   This simple example file contains 2 clauses of request_prefix_target/3.

   The first one relays URIs that start with /rits/ to a (RITS) server
   on port 4040.

   The second and final rule relays everything else to a different web
   server on port 3031.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Rule 1: Provide access to a RITS server via the prefix /rits.
   For example, accessing http://your-url.com/rits/demo.html
   fetches /demo.html from the RITS server running on port 4040.

   The prefix (second argument) is also /rits, so that redirects from
   the RITS server are rewritten in such a way that the following
   client request is again correctly delegated to the RITS server.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

request_prefix_target(Request, '/rits', TargetURI) :-
        memberchk(request_uri(URI), Request),
        without_prefix(URI, '/rits', Target),
        atomic_list_concat(['http://localhost:4040',Target], TargetURI).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Rule 2: Relay everything else to a local web server on port 3031.
   Note that the original path is used, and Prefix is therefore empty.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

request_prefix_target(Request, '', TargetURI) :-
        memberchk(request_uri(URI), Request),
        atomic_list_concat(['http://localhost:3031',URI], TargetURI).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Auxiliary predicate: If Atom0 starts with Prefix, removing the
   Prefix yields Atom. Fails otherwise. Example:

   ?- without_prefix('/dir1/dir2/file', '/dir1', File).
   %@ File = '/dir2/file'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

without_prefix(Atom0, Prefix, Atom) :-
        sub_atom(Atom0, 0, Len, _, Prefix),
        sub_atom(Atom0, Len, _, 0, Atom).

