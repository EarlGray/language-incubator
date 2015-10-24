-module(fact).
-compile(export_all).

fact(0) -> 1;
fact(N) when N > 0 ->
  N * fact(N - 1).

fact0(N) when is_number(N); N >= 0 ->
  Go = fun(_Slf, Acc, 0) -> Acc;
          (Self, Acc, I) -> Self(Self, I * Acc, I - 1)
       end,
  Go(Go, 1, N).
