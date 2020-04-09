:- dynamic assert_list/1, init_db/2.
% Main predicate 
play_azul(NumberOfPlayers):- init_db(NumberOfPlayers, InitialDb),
                                        game([InitialDb | History]).

% State Engine
game([]) :- !.
game([[Players, FactoryList, CenterTokens, Bag, GameState]| RestHistory]):-  game(RestHistory).

% init database for each kind of game (depends of the number of players)
init_db(NumberOfPlayers, [Players, FactoryList, CenterTokens, Bag, GameState]):-  build_players(NumberOfPlayers, Players),
                                                                        assert_list(Players),
                                                                        number_of_factories_by_number_of_players(NumberOfPlayers, NumberOfFactories),
                                                                        build_factories(NumberOfFactories, FactoryList),
                                                                        assert_list(FactoryList),
                                                                        Bag = bag((blue, 20), (yellow, 20), (red, 20), (balck, 20), (white, 20)),
                                                                        assertz(Bag),
                                                                        CenterTokens = center_tokens((blue, 0), (yellow, 0), (red, 0), (balck, 0), (white, 0)),
                                                                        assertz(CenterTokens),
                                                                        GameState = game_state(round0, non_step),
                                                                        assertz(GameState).


assert_list([]):- !.
assert_list([Head | Tail]):- assertz(Head), assert_list(Tail).

build_players(0, [] ) :- !.
build_players(NumberOfPlayers, [Head| Players]):- build_player_info(NumberOfPlayers, Head),
                                                  X is NumberOfPlayers - 1,
                                                  build_players(X, Players).

build_player_info( PlayerNumber, Info) :- compose_board(X),
                                          concat(player, PlayerNumber, Player),
                                          compound_name_arguments(Info, Player, X).

build_factories(0, []) :- !.
build_factories(NumberOfFactories, [Head | FactoryList]):- X is NumberOfFactories - 1,
                                                           initial_factory(L),
                                                           concat(factory, NumberOfFactories, F),
                                                           compound_name_arguments(Head, F, L),
                                                           build_factories(X, FactoryList).

initial_factory([non_color, non_color, non_color, non_color]).

% Static definitions for a game
number_of_factories_by_number_of_players(2, 5).
number_of_factories_by_number_of_players(3, 7).
number_of_factories_by_number_of_players(4, 9).

init_center_table(0).

%% Board parts
score_track(X) :- integer(X), X >= 0.

pattern_lines( 1, [ ( non_color, 1 ) ]) :- !.
pattern_lines( Pattern_length, [(non_color, Pattern_length) | Tail]) :- Y is Pattern_length - 1,
                                                                          pattern_lines(Y, Tail).

floor_line(X) :- X >= 0, X =< 7.

%%% The wall (like Pink Floyd :) )
minor_position( Compound, Position, Color):- arg(Pos, Compound, Color),
                                                  Pos - 5 > Position.
%
line_of_colors([(blue, empty), (yellow, empty), (red, empty), (black, empty), (white, empty)]).
wall_for_board([(blue, empty), (yellow, empty), (red, empty), (black, empty), (white, empty)], [], -5) :- !.
wall_for_board(All_Colors, [Head | Tail], Position) :- line_of_colors(All_Colors),
                                              compound_name_arguments(Compound, colors, All_Colors),
                                              partition(minor_position(Compound, Position), All_Colors, InitialPart, FinalPart),
                                              append(InitialPart, FinalPart, Head),
                                              NewPosition is Position - 1,
                                              wall_for_board(All_Colors, Tail, NewPosition).

% Compose board method
compose_board([PatternLines, Wall, ScoreTrack, FloorLine]) :- pattern_lines(5, InvertedPatternLines),
                                                              reverse(InvertedPatternLines, PatternLines),
                                                              wall_for_board(_, Wall, 0),
                                                              ScoreTrack is 0,
                                                              FloorLine is 0.

% Writters
format_wall([]) :- write('').
format_wall([Head | Tail]) :- write(Head), nl, format_wall(Tail).

format_board([PatternLines, Wall, ScoreTrack, FloorLine]) :- write('----Pattern Lines----'),nl,
                                                             format_wall(PatternLines),
                                                             write('----Wall----'), nl,
                                                             format_wall(Wall),
                                                             write('----ScoreTrack----'),nl,
                                                             write(ScoreTrack),nl,
                                                             write('----FloorLine----'),nl,
                                                             write(FloorLine),nl,
                                                             write('--------'), nl.
format_all_players([]) :- write('').
format_all_players([Player | Tail]):- compound_name_arguments(Player, PlayerName, Board),
                                      format('---~w---', [PlayerName]), nl,
                                      format_board(Board),
                                      format_all_players(Tail).

format_factories([]) :- write('').
format_factories([Head | Tail]) :- format('~w', [Head]),nl, format_factories(Tail).

format_db([Players, FactoryList, CenterTableState, BagState, GameState]):- write('-----PLAYERS-----'),nl,
                                                                format_all_players(Players),
                                                                write('-----FACTORIES----'),nl,
                                                                format_factories(FactoryList),
                                                                write('---CENTER TABLE---'),nl,
                                                                compound_name_arguments(CenterTableState, _, Count),
                                                                write(Count), nl,
                                                                write('------  BAG  -----'),nl,
                                                                compound_name_arguments(BagState, _, TokensInBag),
                                                                format('~w', [TokensInBag]), nl,
                                                                write('------GAME STATE--'),nl,
                                                                compound_name_arguments(GameState, _, Game),
                                                                format('~w', [Game]), nl,nl.