:- dynamic create_sublist/2.
% Main predicate 
play_azul(NumberOfPlayers):- init_db(NumberOfPlayers, InitialDb),
                                        game([InitialDb | _]).

% State Engine
game([]):- !.
game([
[Players,
FactoryList,
CenterTokens,
Bag,
(_, factory_offert),
TokensInTopBox]| RestHistory]):- put_tokens([Bag, TokensInTopBox], FactoryList,
                                 FactoryListWithPuttedTokens, [NewBag, NewTokensInTopBox]),
                                 players_decisions(Players,
                                 FactoryListWithPuttedTokens,
                                 CenterTokens,
                                 NewPlayersState,
                                 NewFactoryList,
                                 NewCenterTokens),
                                 change_state((_, factory_offert), NewGameState),
                                 game([[NewPlayersState, NewFactoryList,
                                 NewCenterTokens, NewBag, NewGameState,
                                 NewTokensInTopBox] |RestHistory]).


players_decisions([CurrentP | Players], 
CurrentFactoryList,
CurrentCenterTokens,
NewPlayers,
NewFactoryList,
NewCenterTokens):- format_state(
                   CurrentP, CurrentFactoryList, CurrentCenterTokens),
                   choose_the_place_for_take(
                   PlaceForTake,
                   [CurrentCenterTokens| CurrentFactoryList]),
                   
                   .

choose_the_place_for_take(PlaceForTake, ListOfOptions) :- .

from_center_or_from_factorylist(random_user,
                                PlaceForTake,
                                Places):- %remove the current center tokens if empty
                                        partition(is_empty,
                                        Places, ValidPlaces, _),
                                        random_member(PlaceForTake,
                                        ValidPlaces).

equal(X, Y):- X == Y.
put_tokens([Bag, TokensInTopBox], [], [], [Bag, TokensInTopBox]):- !.
%Four steps.
put_tokens([Bag, TokensInTopBox],
           [HeadF | FactoryList],
           [NewHeadF | NewFactoryList],
           [LastBag , LastTokensInTopBox] ):- partition(equal(non_color),
                                              HeadF,
                                              EmptyPartOfFactory,
                                              PartWithColors),
                                              fill_empty([Bag, TokensInTopBox],
                                              EmptyPartOfFactory, NewPart,
                                              [NewBag, NewTokensInTopBox]),
                                              append(NewPart, PartWithColors, NewHeadF),
                                              put_tokens([NewBag, NewTokensInTopBox],
                                              FactoryList, NewFactoryList, [LastBag,
                                              LastTokensInTopBox]).

fill_empty([Bag, TokensInTopBox],
            EmptyPartOfFactory,
            NewPart,
            [NewBag,
            NewTokensInTopBox] ):- length(EmptyPartOfFactory, TotalCount),
                                   take_from(Bag, TotalCount, TakedFromBag, NewBag),
                                   length(TakedFromBag, CountFromBag), CountFromTopBox is TotalCount - CountFromBag,
                                   take_from(TokensInTopBox, CountFromTopBox, TakedFromTopBox, NewTokensInTopBox),
                                   append(TakedFromBag, TakedFromTopBox, NewPart).

take_from([], _, [], []):- !.
take_from(PlaceForTake, 0, [], PlaceForTake):- !.
take_from( PlaceForTake, Count, [Element| Tail], NewPlaceForTake):- random_select(Element, PlaceForTake, Rest),
                                                                    NewCount is Count -1,
                                                                    take_from(Rest, NewCount, Tail, NewPlaceForTake).


change_state(OldGameState, NewGameState):- call(OldGameState, Round, Phase),
                                           concat(round, RoundNumberStr, Round), 
                                           atom_number(RoundNumberStr, RoundNumber),
                                           NewRoundNumber is RoundNumber + 1,
                                           switch_phase(Phase, NewPhase),
                                           concat(NewRound, round, NewRoundNumber),
                                           compound_name_arguments(NewGameState, game_state, [NewRound, NewPhase]).

switch_phase(non_step, factory_offert).
switch_phase(factory_offert, building_wall).
switch_phase(building_wall, next_round_preparation).
switch_phase(next_round_preparation, factory_offert).
% init database for each kind of game (depends of the number of players)
init_db(NumberOfPlayers,
        [Players,
        FactoryList,
        CenterTokens,
        Bag,
        GameState,
        TokensInTopBox]):-  build_players(NumberOfPlayers, Players),
                            number_of_factories_by_number_of_players(
                            NumberOfPlayers, NumberOfFactories),
                            build_factories(NumberOfFactories, FactoryList),
                            create_list([(color(blue), 20),(color(yellow), 20),
                            (color(red), 20), (color(balck), 20), (color(white), 20)],
                            Bag),
                            CenterTokens= [init_token],
                            GameState= game_state(round1, factory_offert),
                            TokensInTopBox= empty.

assert_list([]):- !.
assert_list([Head | Tail]):- assertz(Head), assert_list(Tail).

build_players(0, [] ) :- !.
build_players(NumberOfPlayers, [Head| Players]):- build_player_info(NumberOfPlayers, Head),
                                                  X is NumberOfPlayers - 1,
                                                  build_players(X, Players).

build_player_info(PlayerNumber, Info):- compose_board(X),
                                        concat(player, PlayerNumber, Player),
                                        compound_name_arguments(Info, Player, X).

build_factories(0, []) :- !.
build_factories(NumberOfFactories,
[Head | FactoryList]):- X is NumberOfFactories - 1,
                        initial_factory(Head),
                        build_factories(X, FactoryList).

initial_factory([]).

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


%%% The wall (like Pink Floyd :) )
minor_position( Compound, Position, Color):- arg(Pos, Compound, Color),
                                                  Pos - 5 > Position.
%
line_of_colors([(blue, empty), (yellow, empty), (red, empty), (black, empty), (white, empty)]).
wall_for_board([(blue, empty), (yellow, empty), (red, empty), (black, empty), (white, empty)], [], -5) :- !.
wall_for_board(All_Colors, 
[Head | Tail], 
Position) :- line_of_colors(All_Colors),
             compound_name_arguments(Compound, colors, All_Colors),
             partition(minor_position(Compound, Position), 
             All_Colors, InitialPart, FinalPart),
             append(InitialPart, FinalPart, Head),
             NewPosition is Position - 1,
             wall_for_board(All_Colors, Tail, NewPosition).

% Compose board method
compose_board([PatternLines, 
Wall,
ScoreTrack, 
FloorLine]) :- pattern_lines(5, InvertedPatternLines),
               reverse(InvertedPatternLines, PatternLines),
               wall_for_board(_, Wall, 0),
               ScoreTrack is 0,
               FloorLine = empty.

% Writters
format_wall([]) :- write('').
format_wall([Head | Tail]) :- write(Head), nl, format_wall(Tail).

format_board([PatternLines, 
Wall, 
ScoreTrack, 
FloorLine]) :- write('----Pattern Lines----'),nl,
               format_wall(PatternLines),
               write('----Wall----'), nl,
               format_wall(Wall),
               write('----ScoreTrack----'),nl,
               write(ScoreTrack),nl,
               write('----FloorLine----'),nl,
               write(FloorLine),nl,
               write('--------'), nl.

format_all_players([]) :- write('').
format_all_players([Player | Tail]):- format_player(Player),
                                      format_all_players(Tail).

format_player(Player):- compound_name_arguments(Player, PlayerName, Board),
                        format('---~w---', [PlayerName]), nl,
                        format_board(Board).


format_factories([]) :- write('').
format_factories([Head | Tail]) :- format('~w', [Head]),nl, 
                                   format_factories(Tail).

format_db([Players, FactoryList,
CenterTableState,
BagState,
GameState,
TokensInTopBox]):- write('-----PLAYERS-----'),nl,
              format_all_players(Players),
              write('-----FACTORIES----'),nl,
              format_factories(FactoryList),
              write('---CENTER TABLE---'),nl,
              %compound_name_arguments(CenterTableState, _, Count),
              write(CenterTableState), nl,
              write('------  BAG  -----'),nl,
              %compound_name_arguments(BagState, _, TokensInBag),
              format('~w', [BagState]), nl,
              write('------GAME STATE--'),nl,
              %compound_name_arguments(GameState, _, Game),
              format('~w', [GameState]),nl,
              format('~w', TokensInTopBox), nl, nl.

format_state(CurrentP,
             CurrentFactoryList,
             CurrentCenterTokens) :-format('----The current player----'),nl,
                                    format_player(CurrentP),
                                    write('----The player has the following choices:----'),nl,
                                    write('----Take from the factory list:----'),nl,
                                    format_factories(CurrentFactoryList),nl,
                                    write('---Or take from the top of the center of the table'), nl,
                                    format('~w', CurrentCenterTokens), nl,nl
% Auxiliar methods
create_sublist([], []):- !.
create_sublist([Head| ElementsAndRepetitions], 
[H | List]):-  Head =.. [_, Element, Repetitions],
               assertz(Element),
               compound_name_arguments(Element, Instanciator, _),
               length(H, Repetitions),
               maplist(Instanciator, H),
               retract(Element),
               create_sublist(ElementsAndRepetitions, List).

create_list(ElementsAndRepetitions, List):- create_sublist(ElementsAndRepetitions, L),
                                            append(L, List).
is_empty([]).
