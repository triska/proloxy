# Proloxy

## Reverse HTTP proxy written in Prolog

A [reverse HTTP proxy](https://en.wikipedia.org/wiki/Reverse_proxy)
relays requests to different web services. The main advantage is
*isolation of concerns*: You can run different and independent web
services and serve them under a common umbrella URL.

![Proloxy: Reverse HTTP proxy written in SWI-Prolog](proloxy.png)

A sample configuration, relaying requests for `/rits/<File>` to a
[RITS](https://github.com/triska/rits) web server running on
port&nbsp;4043, and everything else to a server on port&nbsp;4041:

    :- http_handler(/,
                    handle_request('', 'http://localhost:4041'),
                    [prefix]).

    :- http_handler('/rits',
                    handle_request('/rits', 'http://localhost:4043'),
                    [prefix]).

Please see the source file ([**proloxy.pl**](proloxy.pl)) for more
information.

Proloxy **requires** SWI-Prolog &gt;= 7.3.12.
