-module(fca_closures).

-import(fca_lib,
         [closure_extent/2,
          closure_intent/2,
          enum_closure_intent/2,
          bit_compresion/1,
          is_superset/2,
          band_bits/2,
          bor_bits/2,
          print_bits/1
         ]).

-export([get_closed_sets/2,
         next_closure/2,
         get_nth_bit/2,
         set_nth_bit/3,
         first_element_of_difference/2
        ]).


get_closed_sets(Size, Context) ->
    ElementZero = << <<0:1>> || _ <- lists:seq(1, Size) >>,
    ElementLast = << <<1:1>> || _ <- lists:seq(1, Size) >>,
    FirstClosure = closure_intent(ElementZero, Context),
    [FirstClosure | get_closures(FirstClosure, Context, ElementLast)].

get_closures(ElementLast, Context, ElementLast) ->
    [];
get_closures(PrevClosure, Context, ElementLast) ->
    Closure = next_closure(PrevClosure, Context),
    [ Closure | get_closures(Closure, Context, ElementLast)].


next_closure(Element, Context)->
    Size = bit_size(Element),
    next_closure(Size, Element, Context).

next_closure(0, Element, _Context) -> Element;
next_closure(M, Element, Context) ->
    case get_nth_bit(M,Element) of
        1 ->  next_closure(M-1, set_nth_bit(0, M, Element), Context);
        0 ->  closure_test(M, Element, Context)
    end.

closure_test(M, Element, Context) ->
    %%io:format("fca ~p~n",[[ <<X:1>> || <<X:1>> <= set_nth_bit(1, M, Element)]]),
    Closure = fca_lib:closure_intent(set_nth_bit(1, M, Element), Context),
    MinElemOfDiff = first_element_of_difference(Closure, Element),
    case MinElemOfDiff >= M of
        true -> Closure;
        false -> next_closure(M-1, Element, Context)
    end.

get_nth_bit(N,Vector) ->
    << _S:(N-1), B:1, _Tail/bitstring >> = Vector,
    B.
set_nth_bit(Value, N, Vector) ->
    << Head:(N-1), _B:1, Tail/bitstring >> = Vector,
    << Head:(N-1), Value:1, Tail/bitstring >>.

first_element_of_difference(Vector1,Vector2)->
    first_element_of_difference(Vector1,Vector2, 1).

first_element_of_difference(<<>>,<<>>, N) -> N;
first_element_of_difference(<<1:1,_/bitstring>>, <<0:1,_/bitstring>>, N) -> N;
first_element_of_difference(<<_:1,RX/bitstring>>,
                            <<_:1,RY/bitstring>>, N) ->
    first_element_of_difference(RX, RY, N+1).
