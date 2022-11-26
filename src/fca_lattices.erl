-module(fca_lattices).

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

-export([supremum/2,
         infimum/3,
         lectically_bigger/2,
         lectic_next/1,
         lectic_prev/1,
         lectic_close/3
]).


supremum(IntentA, IntentB) ->
    band_bits(IntentA, IntentB).

infimum(IntentA, IntentB, Context) ->
    OR = bor_bits(IntentA, IntentB),
    closure_intent(OR, Context).

lectic_close(Element, Num, Context)->
    Size = bit_size(Element),
    <<PredElem:(Num-1)/bitstring, _/bitstring>> = Element,
    TailInteger = round(math:pow(2,Size-Num)),
    Tail = << TailInteger:(Size-Num+1)>>,
    NewElement =  <<PredElem:(Num-1)/bitstring, Tail/bitstring>>,
    %%io:format("Old~p~n",[[ <<X:1>> || <<X:1>> <= Tail]]),
    closure_intent(NewElement, Context).

lectically_bigger(<<>>,<<>>) ->%equal
    false;
lectically_bigger(<< X:1, XR/bitstring >>, << Y:1, YR/bitstring >>)
  when X == Y -> lectically_bigger(<<XR/bitstring >>, <<YR/bitstring >>);
lectically_bigger(<< 1:1, _XR/bitstring >>, << 0:1, _YR/bitstring >>) ->
    true;
lectically_bigger(_,_) ->
    false.

lectic_next(Bits)->
    Size = bit_size(Bits),
    %Max = integer_to_bin(round(math:pow(2,Size+1)-1)),
    lectic_next_do(Bits, Size).

lectic_next_do(<<>>, _Size) -> too_big;
lectic_next_do(Bits, Size) ->
    case Bits of
        << XR:(Size-1)/bitstring, 0:1>>  -> << XR/bitstring, 1:1>>;
        << XR:(Size-1)/bitstring, 1:1>> ->
            case lectic_next_do(<<XR/bitstring >>, Size-1) of
                <<Rest/bitstring>> -> << Rest/bitstring, 0:1 >>;
                too_big -> too_big
            end
    end.

lectic_prev(Bits)->
    Size = bit_size(Bits),
    lectic_prev_do(Bits, Size).

lectic_prev_do(<<>>, _Size) -> too_small;
lectic_prev_do(Bits, Size) ->
    case Bits of
        << XR:(Size-1)/bitstring, 1:1>>  -> << XR/bitstring, 0:1>>;
        << XR:(Size-1)/bitstring, 0:1>> ->
            case lectic_prev_do(<<XR/bitstring >>, Size-1) of
                <<Rest/bitstring>> -> << Rest/bitstring, 1:1 >>;
                too_small -> too_small
            end
    end.
