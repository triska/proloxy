# Proloxy
Reverse HTTP proxy written in Prolog.

A reverse HTTP proxy relays requests to different web services, based
on some rules. The main advantage is isolation of concerns: You can
run different and independent web services and serve them under a
common umbrella URL.

This reverse proxy is currently very preliminary:

It relays all requests to port 4041 on localhost, where another web
server must handle the request.

This **requires** SWI-Prolog &gt;= 7.3.11.
