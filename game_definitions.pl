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
(Round, factory_offert),
TokensInTopBox]| RestHistory]):- length(FactoryList, CountFactories),
                                 fill_factories(Bag, TokensInTopBox,
                                 NewFactoryList, CountFactories),
                                 format('***** The factory offert phase for ~w *****', Round),nl,
                                 players_decisions_in_factory_offert(Players,
                                 NewFactoryList,
                                 CenterTokens,
                                 NewPlayersState),
                                 NewGameState= (Round, building_wall),
                                 game([[NewPlayersState, NewFactoryList,
                                 _, _, NewGameState,
                                 _] |RestHistory]).

players_decisions_in_factory_offert(PlayersState,
                                   [], 
                                   [],
                                   NewPlayersState):- PlayersState == NewPlayersState, !.

players_decisions_in_factory_offert([CurrentP | Players], 
FactoryList,
CenterTokens,
NewPlayersState):- player_in_factory_offert(CurrentP,
                   FactoryList,
                   CenterTokens,
                   NewFactoryList,
                   NewCenterTokens,
                        NewPState),
                   append(Players, NewPState, Players1),
                   write('Salí del pla...'),nl,
                   format_all_players(Players1),
                   players_decisions_in_factory_offert(
                   Players1,
                   NewFactoryList,
                   NewCenterTokens,
                   NewPlayersState)
                   .

player_in_factory_offert( CurrentPlayerState, 
                          FactoryList, 
                          CenterTokens,
                          NewFactoryList,
                          NewCenterTokens,
                          NewPState):- format_state(
                                             CurrentPlayerState,
                                             FactoryList,
                                             CenterTokens),
                                             choose_the_place_for_take(
                                             FactoryList,
                                             CenterTokens,
                                             NewCenterTokens,
                                             NewFactoryList,
                                             TakedTokens),
                                             compound_name_arguments(
                                             CurrentPlayerState,
                                             PlayerName,
                                             [PatternLines,
                                             Wall,
                                             ScoreTrack,
                                             FloorLine]),
                                             format('~w has the following taked tokens: ',
                                             PlayerName),nl,
                                             format('~w', [TakedTokens]),nl,
                                             format('~w has the following pattern_line',
                                             PlayerName),nl,
                                             format_wall(PatternLines),nl,
                                             nth1(1, TakedTokens, Token ),
                                             partition(
                                                full_with_other_token(Token, Wall),
                                                PatternLines,
                                                _,
                                                PossibleElections),
                                             partition(
                                                token_already_in_wall(Token, Wall),
                                                PossibleElections,
                                                _,
                                                PossibleElectionsWithoutTokenInWall),
                                             format('~w can choose one of the followings pattern lines:',
                                             PlayerName),nl,
                                             format_wall(PossibleElectionsWithoutTokenInWall),nl,
                                             length(PossibleElectionsWithoutTokenInWall, Len),
                                             
                                             format('Choose a number between 1 to ~w',
                                             Len),nl, 

                                             read(IndexFromPossibleElection),nl,
                                             nth1(IndexFromPossibleElection, 
                                             PossibleElectionsWithoutTokenInWall,
                                             PatternLineElected),
                                             nth1(IndexInTheOriginalPatternLine,
                                             PatternLines, PatternLineElected),
                                             
                                             partition(equal(non_color),
                                             PatternLineElected,
                                             NonColorPart, 
                                             ColorPart),
                                             fill_pattern_line(TakedTokens,
                                             NonColorPart,
                                             ForFill,
                                             RemainderFromTaked),

                                             reverse(ForFill, ForFillReverse),
                                             append(ForFillReverse,
                                             ColorPart,
                                             PatternLineNew),
                                             append(FloorLine,
                                             RemainderFromTaked,
                                             NewFloorLine),

                                             change_pattern_line(PatternLines,
                                             IndexInTheOriginalPatternLine,
                                             1,
                                             PatternLineNew,
                                             NewPatternLines),
                                             
                                             NewPState= [NewPatternLines,
                                             Wall,
                                             ScoreTrack,
                                             NewFloorLine],
                                             format('*******************************************'),nl,
                                             format('The New pattern lines for the ~w are',
                                             PlayerName),nl,
                                             format_wall(NewPatternLines), nl,
                                             format('And the floor line for that player is'),nl,
                                             write(NewFloorLine),nl,
                                             format('Now the center of the table is'),nl,
                                             write(NewCenterTokens), nl,
                                             format('*******************************************'),nl.

change_pattern_line([_ | PatternLines],
                     IndexForPattern,
                     IndexForPattern,
                     PatternLineNew,
                     [ PatternLineNew | PatternLines]):- !.

change_pattern_line([X | PatternLines],
                    IndexForPattern,
                    CurrentIndex,
                    PatternLineNew,
                    [X | NewPatternLines] ):- N is CurrentIndex + 1,
                                              change_pattern_line(
                                                 PatternLines,
                                                 IndexForPattern,
                                                 N,
                                                 PatternLineNew,
                                                 NewPatternLines
                                              ).  

fill_pattern_line([], NonColorPart, NonColorPart, []).
fill_pattern_line(RemainderFromTaked, [], [], RemainderFromTaked).
fill_pattern_line([X | TakedTokens],
                  [_ | NonColorPart],
                  [X | FillPattern],
                  RemainderFromTaked):-  fill_pattern_line(
                                         TakedTokens,
                                         NonColorPart,
                                         FillPattern,
                                         RemainderFromTaked).

token_already_in_wall(Token,
                       Wall,
                       Pattern):- length(Pattern, IndexLineWall),
                                 nth1(IndexLineWall, Wall, LineWall),
                                 member((Token, full), LineWall).

full_with_other_token(Token,
                      _,
                      Pattern):- partition(equal(non_color),
                                 Pattern,
                                 NonColorPart,
                                 _),
                                 NonColorPart \== Pattern,
                                 not(member(Token, Pattern)).
                                           
% Para el caso de si el centro de mesa o la factory list están vacíos,
%se hace una llamada diferente a la función.
choose_the_place_for_take(FactoryList,
                          CenterTokens,
                          NewFactoryList,
                          NewCenterTokens,
                          TakedTokens):- write('Write factory_list., for take from the factory list'),nl,
                                         write('Write center_tokens., for take from the center tokens'),nl,
                                         read(PlaceForTake),
                                         take_from(PlaceForTake,
                                         FactoryList,
                                         CenterTokens,
                                         NewCenterTokens,
                                         NewFactoryList,
                                         TakedTokens).
                                         

take_from(factory_list,
          FactoryList,
          CenterTokens,
          NewFactoryList,
          NewCenterTokens,
          TakedTokens):- take_from_factory_list(FactoryList,
                         CenterTokens,
                         NewFactoryList,
                         NewCenterTokens,
                         TakedTokens ).

take_from(center_tokens,
          _,
          CenterTokens,
          NewCenterTokens,
          _,
          TakedTokens):- take_from_center(CenterTokens,
                             TakedTokens,
                             NewCenterTokens).

take_from_factory_list(FactoryList,
                       CenterTokens,
                       NewFactoryList,
                       NewCenterTokens,
                       TakedTokens ):-    write('You choose take tokens from factory list'),nl,
                                          write('Those are the factories:'),nl,
                                          format_factories(FactoryList),
                                          length(FactoryList, L),
                                          format('Choose the factory from you want to take, entering a number from 1 to ~w', L),nl,
                                          read(FactoryNumber),
                                          nth1(FactoryNumber, FactoryList, Factory, NewFactoryList),
                                          write('You selected the factory with the following tokens'),nl,
                                          sort(0,
                                          @=<,
                                          Factory,
                                          SortedFactory),
                                          write(SortedFactory),nl,
                                          format('Remember, you can only take all tokens from one color'),nl,
                                          format('Enter your color option:'),nl,
                                          read(ColorOption),nl,
                                          partition(equal(ColorOption),
                                          Factory,
                                          TakedTokens,
                                          TokensToTheCenter),
                                          append(CenterTokens,
                                             TokensToTheCenter,
                                             NewCenterTokens).


take_from_center(CenterTokens,
                TakedTokens,
                NewCenterTokens):- write('You choose take tokens from center'),
                                   write('Those are the tokens in the center of the table:'),
                                   sort(0, @=<, CenterTokens, SortedCenter),
                                   write(SortedCenter),
                                   write('The options that you have are the following'),
                                   sort(SortedCenter, _),
                                   write('Remember, you can only take all tokens from one color'),
                                   write('Enter your color option:'),
                                   read(ColorOption),
                                   partition(equal(ColorOption),
                                   CenterTokens,
                                   TakedTokensStep1,
                                   NewCenterTokensStep1),
                                   partition(equal(init_token),
                                   NewCenterTokensStep1,
                                   InitToken,
                                   NewCenterTokens),
                                   append(TakedTokensStep1, 
                                   InitToken,
                                   TakedTokens).


equal(X, Y):- X == Y.


% Si la bolsa se queda vacía, llenarla con los elementos de la tapa de la caja

fill_factories(_, _, [], 0):- !.

fill_factories(Bag, TokensInTopBox,
_, _):- length(Bag, L), L < 4, 
     append(Bag, TokensInTopBox, NewBag),
     fill_factories(NewBag, [],
     _, _).

fill_factories(Bag, _,
[HeadFactory| FactoryList],
CountFactories):- X is CountFactories - 1,
                  random_tokens(Bag, NewBag, HeadFactory),
                  fill_factories(NewBag, _, FactoryList, X).

random_tokens(Bag, NewBag, HeadFactory):- random_select(Token1, Bag, BagStep1),
                                          random_select(Token2, BagStep1, BagStep2),
                                          random_select(Token3, BagStep2, BagStep3),
                                          random_select(Token4, BagStep3, NewBag),
                                          append([Token1, Token2, Token3, Token4], [], HeadFactory).

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


change_state(Round, _):- call(_, Round, Phase),
                                   concat(round, RoundNumberStr, Round), 
                                   atom_number(RoundNumberStr, RoundNumber),
                                   NewRoundNumber is RoundNumber + 1,
                                   switch_phase(Phase, NewPhase),
                                   concat(NewRound, round, NewRoundNumber),
                                   compound_name_arguments(_, game_state, [NewRound, NewPhase]).


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
                            (color(red), 20), (color(black), 20), (color(white), 20)],
                            Bag),
                            CenterTokens= [init_token],
                            GameState= (round1, factory_offert),
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

pattern_lines( 1, [ [non_color] ]) :- !.
pattern_lines( Pattern_length,
[ Pattern | Tail]) :- compound_name_arguments(Instantiator, 
                      instantiator, [non_color]),
                      assertz(Instantiator),
                      length(Pattern, Pattern_length),
                      maplist(instantiator, Pattern),
                      retract(Instantiator),
                      Y is Pattern_length - 1,
                      pattern_lines(Y, Tail), !.

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
               FloorLine = [non_color, non_color, non_color, non_color, non_color, non_color, non_color].

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
format_factories([Head | Tail]) :- sort(0, @=<, Head, SortedHead),
                                   format('~w', [SortedHead]),nl, 
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
             CurrentCenterTokens):- format('----The current player----'),nl,
                                    format_player(CurrentP),
                                    write('----The player has the following choices:----'),nl,
                                    write('----Take from the factory list:----'),nl,
                                    format_factories(CurrentFactoryList),nl,
                                    write('---Or take from the top of the center of the table'), nl,
                                    format('~w', CurrentCenterTokens), nl, nl.
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

take_n(0, X, [], X) :- !.
take_n(N, [X|Xs], [X|Ys], R) :- M is N-1, take_n(M, Xs, Ys, R).
