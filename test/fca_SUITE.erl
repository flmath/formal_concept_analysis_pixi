%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
-module(fca_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    OK1 = application:start(sasl),
    OK2 = application:start(fca),
    ct:pal("************************* applications start ~p", [{OK1,OK2}]),
  %  ct:pal("************************* applications start ~p", [erlang:pwd()]),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [basic_return_case,
     basic_load_case,
     basic_intent_size_case,
     basic_bit_operations,
     get_closure_intent,
     get_closure_intent_edges,
     get_closure_enum_extent,
     get_closure_extent,
     get_closure_extent_edges,
     supremum_test,
     infimum_test,
     lectic_closure,
     lectic_order_cmp,
     lectic_order_next,
     closed_sets,
     closed_sets_loop,
     size_partition_lattice,
     get_connections_test,
     get_connections2_test,
     create_labeled_points,
     make_layered_points,
     small_make_layered_points_custom,
     same_layer_points_custom,
     make_layered_points_custom,
     pixi_format_connections
     ].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
basic_return_case() ->   [].
basic_return_case(_Config) ->
    Return = fca_server:context_table(),
    ?assertEqual({ok, <<>>}, Return).


basic_load_case() ->   [].
basic_load_case(_Config) ->
    fca_server:context_load_table(4, <<0,0,0,1,0,0,1,0>>),
    Return = fca_server:context_table(),
    ?assertEqual({ok, <<0,0,0,1,0,0,1,0>>}, Return).


basic_intent_size_case() -> [].
basic_intent_size_case(_Config) ->
    fca_server:context_load_table(4, <<0,0,0,1,0,0,1,0>>),
    Return = fca_server:intent_size(),
    ?assertEqual({ok, 4}, Return).

basic_bit_operations() -> [].
basic_bit_operations(_Config) ->
    B1 = fca_lib:bit_compresion(<<1,1,0,1>>),
    B2 = fca_lib:bit_compresion(<<1,0,0,1>>),
    True = fca_lib:is_superset(B1,B2),
    False = fca_lib:is_superset(B2,B1),
    ?assertEqual(true, True),
    ?assertEqual(false, False).

get_closure_enum_extent() -> [].
get_closure_enum_extent(_Config) ->
    Context = fca_lib:bit_compresion(<<1,1,0,1,
                                       0,1,1,0,
                                       1,0,0,1>>),

    Element = fca_lib:bit_compresion(<<1,0,0,1>>),
    Closure = fca_lib:enum_closure_extent(Element, Context),
    Expected = {[1,3],
                fca_lib:bit_compresion(<<1,1,0,1,
                                         1,0,0,1>>)},

    ?assertEqual(Expected, Closure).

get_closure_extent() -> [].
get_closure_extent(_Config) ->
    Context = fca_lib:bit_compresion(<<1,1,0,1,
                                       0,1,1,0,
                                       1,0,0,1>>),

    Element = fca_lib:bit_compresion(<<1,0,0,1>>),
    Closure = fca_lib:closure_extent(Element, Context),
    Expected = fca_lib:bit_compresion(<<1,1,0,1,
                                        1,0,0,1>>),

    ?assertEqual(Expected, Closure).

get_closure_extent_edges() -> [].
get_closure_extent_edges(_Config) ->
    Context = fca_lib:bit_compresion(<<1,1,0,1,
                                       0,1,1,0,
                                       1,0,0,1>>),

    Element = fca_lib:bit_compresion(<<0,0,0,0>>),
    Element2 = fca_lib:bit_compresion(<<1,1,1,1>>),
    Closure = fca_lib:closure_extent(Element, Context),
    Closure2 = fca_lib:closure_extent(Element2, Context),
    Expected = Context,
    Expected2 = fca_lib:bit_compresion(<<>>),
    [
     ?assertEqual(Expected, Closure),
     ?assertEqual(Expected2, Closure2)
    ].

get_closure_intent() -> [].
get_closure_intent(_Config) ->
    Context = fca_lib:bit_compresion(<<1,1,0,1,1,
                                       0,0,1,0,1,
                                       0,1,1,1,1,
                                       1,0,0,1,0>>),

    Element = fca_lib:bit_compresion(<<0,0,1,0,0>>),
    Closure = fca_lib:closure_intent(Element, Context),
    Expected = fca_lib:bit_compresion(<<0,0,1,0,1>>),

    ?assertEqual(Expected, Closure).

get_closure_intent_edges() -> [].
get_closure_intent_edges(_Config) ->
    Context = fca_lib:bit_compresion(<<1,1,0,1,1,
                                       1,0,1,0,1,
                                       1,1,1,1,1,
                                       1,0,0,1,0>>),

    Element = fca_lib:bit_compresion(<<0,0,0,0,0>>),
    Element2 = fca_lib:bit_compresion(<<1,1,1,1,1>>),

    Closure = fca_lib:closure_intent(Element, Context),
    Closure2 = fca_lib:closure_intent(Element2, Context),
    Expected = fca_lib:bit_compresion(<<1,0,0,0,0>>),
    Expected2 = fca_lib:bit_compresion(<<1,1,1,1,1>>),
    [
     ?assertEqual(Expected, Closure),
     ?assertEqual(Expected2, Closure2)
    ].



supremum_test() -> [].
supremum_test(_Config) ->
    Element1 = fca_lib:bit_compresion(<<0,1,0,1,1>>),
    Element2 = fca_lib:bit_compresion(<<1,1,0,1,0>>),
    Result = fca_lattices:supremum(Element1, Element2),
    Expected = fca_lib:bit_compresion(<<0,1,0,1,0>>),
    ?assertEqual(Expected, Result).

infimum_test() -> [].
infimum_test(_Config) ->
    Context = fca_lib:bit_compresion(<<1,1,0,1,1,1,
                                       0,0,1,0,1,0,
                                       0,1,1,1,0,1,
                                       0,0,1,1,0,1,
                                       1,0,0,1,0,1,
                                       1,1,1,1,0,1>>),

    Element1 = fca_lib:bit_compresion(<<0,1,1,0,0,0>>),
    Element2 = fca_lib:bit_compresion(<<0,0,1,1,0,0>>),
    Result = fca_lattices:infimum(Element1, Element2, Context),
    Expected = fca_lib:bit_compresion(<<0,1,1,1,0,1>>),

    ?assertEqual(Expected, Result).

lectic_order_cmp()-> [].
lectic_order_cmp(_Config)->
    Nul = fca_lib:bit_compresion(<<0,0,0,0,0,0>>),
    Ett = fca_lib:bit_compresion(<<0,0,0,0,0,1>>),
    Tva = fca_lib:bit_compresion(<<0,0,0,0,1,0>>),
    %%Tre = fca_lib:bit_compresion(<<0,0,0,0,1,1>>),
    Fyra = fca_lib:bit_compresion(<<0,0,0,1,0,0>>),
    %%Fem = fca_lib:bit_compresion(<<0,0,0,1,0,1>>),
    Sex = fca_lib:bit_compresion(<<0,0,0,1,1,0>>),
    Sju = fca_lib:bit_compresion(<<0,0,0,1,1,1>>),
    Atta = fca_lib:bit_compresion(<<0,0,1,0,0,0>>),
    Nio = fca_lib:bit_compresion(<<0,0,1,0,0,1>>),
    Tio = fca_lib:bit_compresion(<<0,0,1,0,1,0>>),

    Result1 = fca_lattices:lectically_bigger(Ett,Nul),
    Result2 = fca_lattices:lectically_bigger(Atta,Sju),
    Result3 = fca_lattices:lectically_bigger(Tio,Nio),
    Result4 = fca_lattices:lectically_bigger(Tva, Fyra),
    Result5 = fca_lattices:lectically_bigger(Sex, Sju),

    [?assertEqual(true, Result1),
     ?assertEqual(true, Result2),
     ?assertEqual(true, Result3),
     ?assertEqual(false, Result4),
     ?assertEqual(false, Result5)].

lectic_order_next()-> [].
lectic_order_next(_Config)->
    Nul = fca_lib:bit_compresion(<<0,0,0,0,0,0>>),
    Ett = fca_lib:bit_compresion(<<0,0,0,0,0,1>>),
    Tva = fca_lib:bit_compresion(<<0,0,0,0,1,0>>),
    Tre = fca_lib:bit_compresion(<<0,0,0,0,1,1>>),
    Fyra = fca_lib:bit_compresion(<<0,0,0,1,0,0>>),
    %% Fem = fca_lib:bit_compresion(<<0,0,0,1,0,1>>),
    Sex = fca_lib:bit_compresion(<<0,0,0,1,1,0>>),
    Sju = fca_lib:bit_compresion(<<0,0,0,1,1,1>>),
    Atta = fca_lib:bit_compresion(<<0,0,1,0,0,0>>),
    Nio = fca_lib:bit_compresion(<<0,0,1,0,0,1>>),
    Tio = fca_lib:bit_compresion(<<0,0,1,0,1,0>>),
    Bygga = fca_lib:bit_compresion(<<1,1,1,1,1,1>>),

    Result1 = fca_lattices:lectic_next(Ett),
    Result2 = fca_lattices:lectic_next(Atta),
    Result3 = fca_lattices:lectic_next(Nio),
    Result4 = fca_lattices:lectic_next(Tva),
    Result5 = fca_lattices:lectic_next(Sju),
    Result6 = fca_lattices:lectic_next(Sex),
    Result7 = fca_lattices:lectic_next(Nul),
    Result8 = fca_lattices:lectic_next(Bygga),

    Result01 = fca_lattices:lectic_prev(Tre),
    Result02 = fca_lattices:lectic_prev(Tio),
    Result03 = fca_lattices:lectic_prev(Atta),
    Result04 = fca_lattices:lectic_prev(Fyra),
    Result05 = fca_lattices:lectic_prev(Nio),
    Result06 = fca_lattices:lectic_prev(Ett),
    Result07 = fca_lattices:lectic_prev(Tva),
    Result08 = fca_lattices:lectic_prev(Nul),


    [?assertEqual(Tva, Result1),
     ?assertEqual(Nio, Result2),
     ?assertEqual(Tio, Result3),
     ?assertEqual(Tre, Result4),
     ?assertEqual(Atta, Result5),
     ?assertEqual(Sju, Result6),
     ?assertEqual(Ett, Result7),
     ?assertEqual(too_big, Result8),

     ?assertEqual(Tva, Result01),
     ?assertEqual(Nio, Result02),
     ?assertEqual(Sju, Result03),
     ?assertEqual(Tre, Result04),
     ?assertEqual(Atta, Result05),
     ?assertEqual(Nul, Result06),
     ?assertEqual(Ett, Result07),
     ?assertEqual(too_small, Result08)].

lectic_closure() -> [].
lectic_closure(_Config) ->
    Context = fca_lib:bit_compresion(<<0,0,1,0,1,0,
                                       1,1,0,1,1,1,
                                       0,1,0,1,0,1,
                                       0,0,1,1,0,1,
                                       1,1,1,0,1,1,
                                       1,1,1,1,0,1>>),

    Element1 = fca_lib:bit_compresion(<<1,1,0,0,1,0>>),
    Element2 = fca_lib:bit_compresion(<<0,1,0,0,1,0>>),
    Result1 = fca_lattices:lectic_close(Element1, 3, Context),
    Result2 = fca_lattices:lectic_close(Element2, 4, Context),
    Expected1 = fca_lib:bit_compresion(<<1,1,1,0,0,1>>),
    Expected2 = fca_lib:bit_compresion(<<0,1,0,1,0,1>>),

    [?assertEqual(Expected1, Result1),
     ?assertEqual(Expected2, Result2)].


closed_sets() -> [].
closed_sets(_Config) ->
    Context = fca_lib:bit_compresion(<<0,1,0,1,0,
                                       0,1,0,0,1,
                                       0,0,1,0,0,
                                       1,1,1,0,0,
                                       0,0,0,1,0,
                                       0,1,1,0,0,
                                       0,0,0,0,1
                                     >>),
    Element1 = fca_lib:bit_compresion(<<0,0,0,0,0>>),
    FirstClosure = fca_lib:closure_intent(Element1, Context),
    %%{} is closed
    Result1 = fca_closures:next_closure(FirstClosure, Context),
    %% m=5 -> {5} ''-> {5} new closed set {5}
    Result2 = fca_closures:next_closure(Result1, Context),
    %%m =5 -> rm 5th, m= 4 -> {4} ''-> {4}
    Result3 = fca_closures:next_closure(Result2, Context),
    %% 5 -> {4,5}, ''-> All,
    %% but snallest new element is 1, min(All/{4}) 1> 5(m)
    %% so we cant say  Result is m is more 1, so we ignore
    %% closest set still {4}
    %% m = 4 -> {}, 3 -> {3} '' ->{3}, min({3}/{})>=m=3
    Result4 = fca_closures:next_closure(Result3, Context),
    %% 5 -> {3,5}, ''-> All, ignore
    %% 4 -> {3,4}, ''-> All, ignore
    %% 3 -> {}, 2 -> {2} ''-> {2}
    Result5 = fca_closures:next_closure(Result4, Context),
    %% closet {2}
    %% 5 -> {2,5} ''-> {2,5} 5>2
    Result6 = fca_closures:next_closure(Result5, Context),
    %% closet {2,5}
    %% 5-> {2} 4-> {2,4} ''-> {2,4}
    Result7 = fca_closures:next_closure(Result6, Context),
    %% closet {2,4}
    %% 5-> {2,4,5} ''->All 1<5
    %% 4 -> {2}, 3 -> {2,3} ''-> {2,3}
    Result8 = fca_closures:next_closure(Result7, Context),
    %% closet {2,3}
    %% 5-> {2,3,5} ''->All 1<5
    %% 4-> {2,3,4},  ''->All 1<4
    %% 3 -> {2} 2-> {} 1-> {1} ''-> {1,2,3}
    Result9 = fca_closures:next_closure(Result8, Context),
    %% closet {1,2,3}
    %% 5 -> {1,2,3,5} ''-> All 4<5
    %% 4 -> {1,2,3,4} ''-> All 4>=4
    %%Slut

    Expected1 = fca_lib:bit_compresion(<<0,0,0,0,1>>),
    Expected2 = fca_lib:bit_compresion(<<0,0,0,1,0>>),
    Expected3 = fca_lib:bit_compresion(<<0,0,1,0,0>>),
    Expected4 = fca_lib:bit_compresion(<<0,1,0,0,0>>),
    Expected5 = fca_lib:bit_compresion(<<0,1,0,0,1>>),
    Expected6 = fca_lib:bit_compresion(<<0,1,0,1,0>>),
    Expected7 = fca_lib:bit_compresion(<<0,1,1,0,0>>),
    Expected8 = fca_lib:bit_compresion(<<1,1,1,0,0>>),
    Expected9 = fca_lib:bit_compresion(<<1,1,1,1,1>>),

    [?assertEqual(Expected1, Result1),
     ?assertEqual(Expected2, Result2),
     ?assertEqual(Expected3, Result3),
     ?assertEqual(Expected4, Result4),
     ?assertEqual(Expected5, Result5),
     ?assertEqual(Expected6, Result6),
     ?assertEqual(Expected7, Result7),
     ?assertEqual(Expected8, Result8),
     ?assertEqual(Expected9, Result9)
    ].

closed_sets_loop() -> [].
closed_sets_loop(_Config) ->
    Context = fca_lib:bit_compresion(<<0,1,0,1,0,
                                       0,1,0,0,1,
                                       0,0,1,0,0,
                                       1,1,1,0,0,
                                       0,0,0,1,0,
                                       0,1,1,0,0,
                                       0,0,0,0,1
                                     >>),

    Result = fca_closures:get_closed_sets(5, Context),

    Expected =
        [
         fca_lib:bit_compresion(<<0,0,0,0,0>>),
         fca_lib:bit_compresion(<<0,0,0,0,1>>),
         fca_lib:bit_compresion(<<0,0,0,1,0>>),
         fca_lib:bit_compresion(<<0,0,1,0,0>>),
         fca_lib:bit_compresion(<<0,1,0,0,0>>),
         fca_lib:bit_compresion(<<0,1,0,0,1>>),
         fca_lib:bit_compresion(<<0,1,0,1,0>>),
         fca_lib:bit_compresion(<<0,1,1,0,0>>),
         fca_lib:bit_compresion(<<1,1,1,0,0>>),
         fca_lib:bit_compresion(<<1,1,1,1,1>>)
        ],
    [
     ?assertEqual(Expected, Result)
    ].

size_partition_lattice() -> [].
size_partition_lattice(_Config) ->
    Family = [
         fca_lib:bit_compresion(<<0,0,0,0,0>>),
         fca_lib:bit_compresion(<<0,0,0,0,1>>),
         fca_lib:bit_compresion(<<0,0,0,1,0>>),
         fca_lib:bit_compresion(<<0,0,1,0,0>>),
         fca_lib:bit_compresion(<<0,1,0,0,0>>),
         fca_lib:bit_compresion(<<0,1,0,0,1>>),
         fca_lib:bit_compresion(<<0,1,0,1,0>>),
         fca_lib:bit_compresion(<<0,1,1,0,0>>),
         fca_lib:bit_compresion(<<1,1,1,0,0>>),
         fca_lib:bit_compresion(<<1,1,1,1,1>>)
        ],
    Result = fca_draw:make_size_partition(Family),

    Expected =  [
                 {0,
                  [
                   {0, fca_lib:bit_compresion(<<0,0,0,0,0>>)}]},
                 {1,
                  [
                   {3, fca_lib:bit_compresion(<<0,1,0,0,0>>)},
                   {2, fca_lib:bit_compresion(<<0,0,1,0,0>>)},
                   {1, fca_lib:bit_compresion(<<0,0,0,1,0>>)},
                   {0, fca_lib:bit_compresion(<<0,0,0,0,1>>)}]},
                 {2,
                  [
                   {2, fca_lib:bit_compresion(<<0,1,1,0,0>>)},
                   {1, fca_lib:bit_compresion(<<0,1,0,1,0>>)},
                   {0, fca_lib:bit_compresion(<<0,1,0,0,1>>)}]},
                 {3,
                  [{0, fca_lib:bit_compresion(<<1,1,1,0,0>>)}]},
                 {4, []},
                 {5,
                  [
                   {0, fca_lib:bit_compresion(<<1,1,1,1,1>>)}]}
                ],
    [
     ?assertEqual(Expected, Result)
    ].


get_connections_test() -> [].
get_connections_test(_Config) ->
    Layers = [
              {1,
               [
                {3, fca_lib:bit_compresion(<<0,1,0,0,0>>)},
                {2, fca_lib:bit_compresion(<<0,0,1,0,0>>)},
                {1, fca_lib:bit_compresion(<<0,0,0,1,0>>)},
                {0, fca_lib:bit_compresion(<<0,0,0,0,1>>)}
               ]},
              {2,
               [
                {2, fca_lib:bit_compresion(<<0,1,1,0,0>>)},
                {1, fca_lib:bit_compresion(<<0,1,0,1,0>>)},
                {0, fca_lib:bit_compresion(<<0,1,0,0,1>>)}
               ]}
             ],

    Result = lists:sort(fca_draw:get_connections(Layers)),

    Expected =  lists:sort([{3,1,2,2},
                            {3,1,1,2},
                            {3,1,0,2},
                            {2,1,2,2},
                            {1,1,1,2},
                            {0,1,0,2}
                           ]),
    [
     ?assertEqual(Expected, Result)
    ].


get_connections2_test() -> [].
get_connections2_test(_Config) ->
    Layers = [
              {1,
               [
                {3, fca_lib:bit_compresion(<<0,1,0,0,0>>)}
               ]},
              {2, []},
              {3,
               [
                {1, fca_lib:bit_compresion(<<0,1,1,0,0>>)},
                {0, fca_lib:bit_compresion(<<0,0,0,1,0>>)}
               ]}
             ],

    Result = fca_draw:get_connections(Layers),

    Expected =  [{3,1,1,3}],
    [
     ?assertEqual(Expected, Result)
    ].




create_labeled_points() -> [].
create_labeled_points(_Config) ->
    Labels = #{1=>"a",
               2=>"b",
               3=>"c",
               4=>"d",
               5=>"e"},

    Family =  [
               {0,
                [
                 {0, fca_lib:bit_compresion(<<0,0,0,0,0>>)}]},
               {1,
                [
                 {3, fca_lib:bit_compresion(<<0,1,0,0,0>>)},
                 {2, fca_lib:bit_compresion(<<0,0,1,0,0>>)},
                 {1, fca_lib:bit_compresion(<<0,0,0,1,0>>)},
                 {0, fca_lib:bit_compresion(<<0,0,0,0,1>>)}]},
               {2,
                [
                 {2, fca_lib:bit_compresion(<<0,1,1,0,0>>)},
                 {1, fca_lib:bit_compresion(<<0,1,0,1,0>>)},
                 {0, fca_lib:bit_compresion(<<0,1,0,0,1>>)}]},
               {3,
                [{0, fca_lib:bit_compresion(<<1,1,1,0,0>>)}]},
               {4, []},
               {5,
                [
                 {0, fca_lib:bit_compresion(<<1,1,1,1,1>>)}]}
              ],


    Result = lists:sort(fca_draw:create_labeled_points(Family, Labels)),
    Expected =
        lists:sort([
                    {" ", 0, 0},
                    {"b", 3, 1},
                    {"c", 2, 1},
                    {"d", 1, 1},
                    {"e", 0, 1},
                    {" ", 0, 2},
                    {" ", 1, 2},
                    {" ", 2, 2},
                    {"a", 0, 3},
                    {" ", 0, 5}]),

    [
     ?assertEqual(Expected, Result)
    ].


make_layered_points() -> [].
make_layered_points(_Config) ->
    Points =
        [
         {" ", 0, 0},
         {"b", 3, 1},
         {"c", 2, 1},
         {"d", 1, 1},
         {"e", 0, 1},
         {" ", 0, 2},
         {" ", 1, 2},
         {" ", 2, 2},
         {"a", 0, 3},
         {" ", 0, 5}
        ],


    Result = lists:sort(fca_draw:make_points_layered(Points)),
    Expected =
         [
          {0,[{" ", 0, 0}]},
          {1,[{"e", 0, 1},
              {"d", 1, 1},
              {"c", 2, 1},
              {"b", 3, 1}]},
          {2,[{" ", 2, 2},
              {" ", 1, 2},
              {" ", 0, 2}]},
          {3,[{"a", 0, 3}]},
          {5,[{" ", 0, 5}]}],
    [
     ?assertEqual(Expected, Result)
    ].


small_make_layered_points_custom() -> [].
small_make_layered_points_custom(_Config) ->
    Points =
        [
         {" ", 0, 0},
         {"e", 0, 1}
        ],


    Result = lists:append(fca_draw:make_points_layered_custom(fun fca_draw:pixi_format_points/1, Points)),
    Expected =
        ",\\\n[\\\n{\"name\" : \" \", \"x1\" : \"0\", \"y1\" : \"0\"}\\\n],"++
        "\\\n[\\\n{\"name\" : \"e\", \"x1\" : \"0\", \"y1\" : \"1\"}\\\n]",
    [
     ?assertEqual(Expected, Result)
    ].

same_layer_points_custom() -> [].
same_layer_points_custom(_Config) ->
    Points =
        [
         {" ", 0, 1},
         {"e", 1, 1}
        ],


    Result = lists:append(fca_draw:make_points_layered_custom(fun fca_draw:pixi_format_points/1, Points)),

    Expected =
        ",\\\n[\\\n{\"name\" : \"e\", \"x1\" : \"1\", \"y1\" : \"1\"},\\\n"++
        "{\"name\" : \" \", \"x1\" : \"0\", \"y1\" : \"1\"}\\\n]",

    [
     ?assertEqual(Expected, Result)
    ].



make_layered_points_custom() -> [].
make_layered_points_custom(_Config) ->
    Points =
        [
         {" ", 0, 0},
         {"b", 3, 1},
         {"c", 2, 1},
         {"d", 1, 1},
         {"e", 0, 1},
         {" ", 0, 2},
         {" ", 1, 2},
         {" ", 2, 2},
         {"a", 0, 3},
         {" ", 0, 5}
        ],


    Result = lists:append(fca_draw:make_points_layered_custom(fun fca_draw:pixi_format_points/1, Points)),
    Expected =

    Expected =
        ",\\\n[\\\n{\"name\" : \" \", \"x1\" : \"0\", \"y1\" : \"0\"}\\\n],\\\n[\\\n"++
        "{\"name\" : \"e\", \"x1\" : \"0\", \"y1\" : \"1\"},\\\n"++
        "{\"name\" : \"d\", \"x1\" : \"1\", \"y1\" : \"1\"},\\\n"++
        "{\"name\" : \"c\", \"x1\" : \"2\", \"y1\" : \"1\"},\\\n"++
        "{\"name\" : \"b\", \"x1\" : \"3\", \"y1\" : \"1\"}\\\n],\\\n[\\\n"++
        "{\"name\" : \" \", \"x1\" : \"2\", \"y1\" : \"2\"},\\\n"++
        "{\"name\" : \" \", \"x1\" : \"1\", \"y1\" : \"2\"},\\\n"++
        "{\"name\" : \" \", \"x1\" : \"0\", \"y1\" : \"2\"}\\\n],\\\n[\\\n"++
        "{\"name\" : \"a\", \"x1\" : \"0\", \"y1\" : \"3\"}\\\n],\\\n[\\\n"++
        "{\"name\" : \" \", \"x1\" : \"0\", \"y1\" : \"5\"}\\\n]",

    [
     ?assertEqual(Expected, Result)
    ].



pixi_format_connections() -> [].

pixi_format_connections(_Config) ->
    Points = lists:sort([{1,1,1,2},
                         {1,4,1,5}
                        ]),

    Result = fca_draw:pixi_format_connections(Points),

    Expected =
        "{\"x1\" : \"1\", \"y1\" : \"1\", \"x2\" : \"1\", \"y2\" : \"2\"},\\\n" ++
        "{\"x1\" : \"1\", \"y1\" : \"4\", \"x2\" : \"1\", \"y2\" : \"5\"}\\\n",
    [
     ?assertEqual(Expected, Result)
    ].
