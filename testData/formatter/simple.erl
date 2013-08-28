  -module  (   simple )  .

  -record(i, {    }  )  .

-spec convert(tuple()) -> list()  ;
        (  list()  ) -> tuple().

  f( [] )    ->    f(   )  ;
  f(A  )    ->    f(   )   .