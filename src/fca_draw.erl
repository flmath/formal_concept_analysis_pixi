-module(fca_draw).

%% API
-export([

         create_labeled_points/2,
         closed_family_to_pixi/2,
         make_size_partition/1,
         get_connections/1,
         make_points_layered/1,
         make_points_layered_custom/2,
         pixi_format_points/1,
         pixi_format_connections/1,
         pixi_json/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
closed_family_to_pixi(Family, Labels) ->
    FamilyBySize = make_size_partition(Family),
    Connections = get_connections(FamilyBySize),
    {FamilyBySize, Connections, Labels}.


%% %%%========================================= ==========================
%% %%% Internal functions
%% %%%===================================================================

get_connections_filter([])-> [];
get_connections_filter([{_,[]} | FamilyBySize])->
    get_connections_filter( FamilyBySize);
get_connections_filter([H | FamilyBySize]) ->
    [H | get_connections_filter( FamilyBySize)].


get_connections(FamilyBySize)->
    Filtered =  get_connections_filter(FamilyBySize),
    get_connections(Filtered, []).

get_connections([H1, H2], Acc)->
    NewConns = get_connections_handle(H1, H2),
    NewConns++Acc;
get_connections([H1, H2 | FamilyBySize], Acc)->
    NewConns = get_connections_handle(H1, H2),
    get_connections([H2 | FamilyBySize], NewConns ++ Acc).



get_connections_handle({N1, Layer1}, {N2, Layer2})->
    [{L1, N1, L2, N2} || {L1, El1} <- Layer1, {L2, El2} <- Layer2,
                         true == fca_lib:is_superset(El2, El1)].

make_size_partition(Family) ->
    SortedFamily = lists:sort(fun compare_bit_sum/2, Family),
    make_size_partition(SortedFamily, 0, [], 0).




make_size_partition([], _Num, [], _) -> [];
make_size_partition([], Num, Last, _Indx) -> [{Num,  Last}];
make_size_partition( [H | SortedFamily], Num, Acc, Indx) ->
    case bit_sum(H)==Num of
        true -> make_size_partition(SortedFamily, Num,
                                    [{Indx, H} | Acc], Indx+1);
        false -> [ {Num , Acc} |
                   make_size_partition([H | SortedFamily], Num+1, [], 0)]
    end.

compare_bit_sum(Set1, Set2)->
    bit_sum(Set1) =< bit_sum(Set2).

bit_sum(Set)->
   lists:sum( [X || <<X:1>> <= Set]).

create_labeled_points(Family, Labels) ->
    create_labeled_points(Family, Labels, []).


create_labeled_points([], _Labels, Result) -> Result;
create_labeled_points([{Y, Layer}| Layers], Labels, Result) ->
    create_labeled_points(Layers, Layer, Labels, Y, Result).

create_labeled_points(Layers, [], Labels, _Y, Result) ->
    create_labeled_points(Layers, Labels, Result);

create_labeled_points(Layers, [{X, _Vector} | Vectors], Map, Y, Result) when map_size(Map) == 0 ->
    create_labeled_points(Layers, Vectors, #{}, Y, [{" ", X, Y} | Result]);
create_labeled_points(Layers, [{X, Vector} | Vectors], Labels, Y, Result) ->
    {NewLabels, ReturnedLabels} = get_label(Vector, Labels),
    create_labeled_points(Layers, Vectors, NewLabels, Y,
                          [{label_concat(ReturnedLabels), X, Y} | Result]).

label_concat(" ") ->
    " ";
label_concat(ReturnedLabels) ->
    lists:concat(lists:join(" ", ReturnedLabels)).

get_label(Vector, Labels)->
    Indexes = get_vec_numbers(Vector),
    take_labels(Indexes, Labels).

take_labels(Indexes, Labels) ->
    take_labels(Indexes, Labels, []).

take_labels([], Labels, []) -> {Labels, " "};
take_labels([], Labels, Acc) -> {Labels, lists:sort(Acc)};
take_labels([H | Indexes], Labels, Acc) ->
    {Key, Map} = case maps:take(H, Labels) of
                 error -> {[], Labels};
                 {K, M} -> {[K], M}
             end,
    take_labels(Indexes, Map, Key ++ Acc).

get_vec_numbers(Bin)->
    List = fca_lib:print_bits(Bin),
    take_indexes(List, lists:seq(1, length(List))).

take_indexes([], [])-> [];
take_indexes([0 | T1], [_ | T2] ) ->
    take_indexes(T1,T2);
take_indexes([1 | T1], [Indx | T2]) ->
    [Indx | take_indexes(T1, T2)].

make_points_layered(Points)->
    SortedPoints = [{_,_,Num} | _] = lists:keysort(3, Points),
    make_points_layered_help(SortedPoints, Num, []).

make_points_layered_help([], _Num, []) -> [];
make_points_layered_help([], Num, Last) -> [{Num,  Last}];
make_points_layered_help([{Label, X,Y} | Points], Y, Acc) ->
    make_points_layered_help(Points, Y, [{Label, X,Y} | Acc]);
make_points_layered_help([{Label, X,Y} | Points], Num, []) ->
    make_points_layered_help( [{Label, X,Y} | Points], Num+1, []);
make_points_layered_help([{Label, X,Y} | Points], Num, Acc) ->
   [{Num, Acc} | make_points_layered_help( [{Label, X,Y} | Points], Num+1, [])].




make_points_layered_custom(Fun, Points)->
    SortedPoints = [{_,_,Num} | _] = lists:keysort(3, Points),
   % Fun('$START') ++
    make_points_layered_custom_help(Fun, SortedPoints, Num, []).

make_points_layered_custom_help(Fun, [], _Num, []) ->
    [Fun([]) | Fun('$END')];
make_points_layered_custom_help(Fun, [], Num, Last) ->
    [Fun({Num, Last}) | Fun('$END')];
make_points_layered_custom_help(Fun, [{Label, X,Y} | Points], Y, Acc) ->
    make_points_layered_custom_help(Fun, Points, Y, [{Label, X,Y} | Acc]);
make_points_layered_custom_help(Fun, [{Label, X,Y} | Points], Num, []) ->
    make_points_layered_custom_help(Fun, [{Label, X,Y} | Points], Num+1, []);
make_points_layered_custom_help(Fun, [{Label, X,Y} | Points], Num, Acc) ->
   [Fun({Num, Acc}) | make_points_layered_custom_help(Fun, [{Label, X,Y} | Points], Num+1, [])].




pixi_format_points('$START') -> "";
pixi_format_points('$END') -> "";
pixi_format_points({_Num, Acc}) ->
    ",\\\n[\\\n"++
        pixi_format_list_points(Acc).

pixi_format_list_points([]) -> "";
pixi_format_list_points([{Name, X, Y}]) ->
    "{\"name\" : \""++ Name++"\", \"x1\" : \""++integer_to_list(X)++"\", \"y1\" : \""++
        integer_to_list(Y)++"\"}\\\n]";
pixi_format_list_points([{Name, X, Y} | Acc]) ->
    "{\"name\" : \""++ Name++"\", \"x1\" : \""++integer_to_list(X)++"\", \"y1\" : \""++
        integer_to_list(Y)++"\"},\\\n" ++
        pixi_format_list_points(Acc).

pixi_format_connections([]) -> "";
pixi_format_connections([{X1,Y1,X2,Y2}]) ->
    "{\"x1\" : \"" ++ integer_to_list(X1) ++
        "\", \"y1\" : \"" ++ integer_to_list(Y1) ++
        "\", \"x2\" : \"" ++ integer_to_list(X2) ++
        "\", \"y2\" : \"" ++ integer_to_list(Y2) ++
        "\"}\\\n";
pixi_format_connections([{X1,Y1,X2,Y2} | Points]) ->
    "{\"x1\" : \"" ++ integer_to_list(X1) ++
        "\", \"y1\" : \"" ++ integer_to_list(Y1) ++
        "\", \"x2\" : \"" ++ integer_to_list(X2) ++
        "\", \"y2\" : \"" ++ integer_to_list(Y2) ++
        "\"},\\\n"++
        pixi_format_connections(Points).




pixi_json(ClosedSets, Labels)->
    Layers = make_size_partition(ClosedSets),
    Connections = get_connections(Layers),
    LabeledPoints = create_labeled_points(Layers, Labels),
    %LabeledLayeredPoints = make_points_layered(LabeledPoints),
    JsonPoints =
        lists:append(make_points_layered_custom(fun pixi_format_points/1, LabeledPoints)),
    JsonConnections = pixi_format_connections(Connections),

    "data =\n\'{\"points\":\\\n[\\\n"++
       tl(tl(tl(JsonPoints))) ++
        "],\\\n\"lines\":\\\n[\\\n" ++
        JsonConnections ++ "]}\';\n".
