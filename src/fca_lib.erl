-module(fca_lib).

%% API
-export([closure_extent/2,
         closure_intent/2,
         enum_closure_extent/2,
         bit_compresion/1,
         is_superset/2,
         band_bits/2,
         bor_bits/2,
         print_bits/1]).

%%%===================================================================
%%% API
%%%===================================================================
closure_intent(Intent, ContextBit)->
    Size = bit_size(Intent),
    case closure_extent(Intent, ContextBit) of
        <<X:Size, Tail/bitstring>> -> closure_intent(Tail, Size, <<X:Size>>);
        <<>> ->  Max = round(math:pow(2, Size)-1), <<Max:Size>>
    end.

closure_extent(Intent, ContextBit)->
    IntentSize = bit_size(Intent),
    << <<X:IntentSize>> || <<X:IntentSize>> <= ContextBit,
                           is_superset(<<X:IntentSize>>, Intent)>>.

closure_intent(<<>>, _Size, Current) -> Current;
closure_intent(Intent, Size, Current) ->
    <<X:Size/bitstring, Tail/bitstring>> = Intent,
    BAND = band_bits(<<X:Size/bitstring>>, <<Current:Size/bitstring>>),
    closure_intent(Tail, Size, <<BAND:Size/bitstring>>).

band_bits(<<>>,<<>>) -> <<>>;
band_bits(<< X:1, XR/bitstring >>, << Y:1, YR/bitstring >>)
  when (X == 1) and (Y == 1) ->
     <<Rest/bitstring>> = band_bits( <<XR/bitstring>>,
                                     <<YR/bitstring>> ),
    << 1:1, Rest/bitstring >>;
band_bits(<<_X:1, XR/bitstring >>, <<_Y:1, YR/bitstring >>) ->
    <<Rest/bitstring>> = band_bits( <<XR/bitstring>>, <<YR/bitstring>> ),
    << 0:1, Rest/bitstring >>.


bor_bits(<<>>,<<>>) -> <<>>;
bor_bits(<< X:1, XR/bitstring >>, << Y:1, YR/bitstring >>)
  when (X == 0) and (Y == 0) ->
     <<Rest/bitstring>> = bor_bits( <<XR/bitstring>>,
                                     <<YR/bitstring>> ),
    << 0:1, Rest/bitstring >>;
bor_bits(<<_X:1, XR/bitstring >>, <<_Y:1, YR/bitstring >>) ->
    <<Rest/bitstring>> = bor_bits( <<XR/bitstring>>, <<YR/bitstring>> ),
    << 1:1, Rest/bitstring >>.


enum_closure_extent(Set, ContextBit)->
    IntentSize = bit_size(Set),
    enum_closure_extent(Set, ContextBit, IntentSize, 1).

enum_closure_extent(_Set, <<>>, _Size, _Enum)-> {[], <<>>};
enum_closure_extent(Set, ContextBit, Size, Enum)->
    %io:format("~p~n",[bit_size(ContextBit)]),
    <<X:Size, TailContextBit/bitstring>> = ContextBit,
    {EnumList, Bits} = enum_closure_extent(Set, TailContextBit, Size, Enum+1),
    case  is_superset(<<X:Size>>, Set) of
        true -> {[Enum | EnumList], << X:Size, Bits/bitstring >>};
        false -> {EnumList,  Bits}
    end.

%% (A,B) subconcept of (C,D) ==  is_superset(B, D)
is_superset(<<>>,<<>>) -> true;
is_superset(<<X:1,_/bitstring>>, <<Y:1,_/bitstring>>) when X < Y -> false;
is_superset(<<_:1,RX/bitstring>>, <<_:1,RY/bitstring>>) -> is_superset(RX, RY).



bit_compresion(Binary)-><< <<X:1>> ||  <<X:8>> <= Binary >>.

print_bits(Bits)->
    [ X || <<X:1>> <= Bits].

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
