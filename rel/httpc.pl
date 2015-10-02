/* vim: set syntax:prolog */

dump_swi_homepage :-
  setup_call_cleanup(
    tcp_connect('www.swi-prolog.org':'http', Stream, []),
    ( format(Stream,
             'GET / HTTP/1.1~n\c
              Host: www.swi-prolog.org~n\c
              Connection: close~n~n', []),
      flush_output(Stream),
      copy_stream_data(Stream, current_output)
    ),
    close(Stream)).

modified(URL, Stamp) :-
  http_open(URL, In,
      [ method(head)
      , header(last_modified, Modified)
      ]),
  close(In),
  Modified \== '',
  parse_time(Modified, Stamp).

