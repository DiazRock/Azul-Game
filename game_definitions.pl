:- dynamic build_boards/2.
% Main predicate 
%play_azul(NumberOfPlayers, Narrative):- init_db(NumberOfPlayers, InitialDb),
%                                        game(InitialDb, [InitialDb| Narrative]).

% init database for each kind of game (depends of the number of players)
init_db( 4, InitialDb):- build_boards(4, Boards).


build_boards(0, [] ).
build_boards(NumberOfPlayers, [Head| Boards]):- compose_board(Head),
                                                assert(Head).
                                                X is NumberOfPlayers - 1,
                                                build_boards(X, Boards).

% Static definitions for a game
number_of_factories_by_number_of_players(2, 5).
number_of_factories_by_number_of_players(3, 7).
number_of_factories_by_number_of_players(4, 9).

line_of_colors([blue, yellow, red, black, white]).
rotate_line(Line, NewLine):- line_of_colors(Line), 
%% Board parts
score_track(X) :- integer(X), X >= 0.

pattern_lines( 1, [ ( non_color, 1 ) ]) :- !.
pattern_lines( Pattern_length, [(non_color, Pattern_length) | Tail]) :- Y is Pattern_length - 1,
                                                                          pattern_lines(Y, Tail).

floor_line(X) :- X >= 0, X =< 7.

%%% The wall (like Pink Floyd :) )
minor_position( Compound, Position, Color):- arg(Pos, Compound, Color),
                                                  Pos - 5 > Position.

line_of_colors([blue, yellow, red, black, white]).
wall_for_board([blue, yellow, red, black, white], [], -5) :- !.
wall_for_board(All_Colors, [Head | Tail], Position) :- line_of_colors(All_Colors),
                                              compound_name_arguments(Compound, colors, All_Colors),
                                              partition(minor_position(Compound, Position), All_Colors, InitialPart, FinalPart),
                                              append(InitialPart, FinalPart, Head),
                                              NewPosition is Position - 1,
                                              wall_for_board(All_Colors, Tail, NewPosition).

format_wall([]) :- write('').
format_wall([Head | Tail]) :- write(Head), nl, format_wall(Tail).

%% Compose board method
compose_board()