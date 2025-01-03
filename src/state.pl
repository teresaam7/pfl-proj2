initial_state(Size, state(Board, white)) :-
    empty_board(Size, Board).

empty_board(Size, Board) :-
    length(Board, Size),
    maplist(empty_row(Size), Board).

empty_row(Size, Row) :-
    length(Row, Size),
    maplist(=(empty), Row).

% Clause when the game is over (Winner is not 'none').
game_loop(GameState, _, _, _, _) :-
    game_over(GameState, Winner),
    Winner \= none,
    display_game(GameState),
    nl, format("Game over! Winner: ~w~n", [Winner]).

% Clause when the game is not over.
game_loop(GameState, Player1, Player2, TurnCount, PieRule) :-
    display_game(GameState),
    game_over(GameState, none),
    GameState = state(_, CurrentPlayer),
    handle_turn(GameState, Player1, Player2, CurrentPlayer, TurnCount, PieRule).

% Clause for exiting the game.
game_loop(_, _, _, _, _) :-
    nl, write('Game exited. Thank you for playing!'), !.

% Clause for handling turn when its the second turn (Pie rule applies).
handle_turn(GameState, Player1, Player2, _, TurnCount, _) :-
    TurnCount =:= 2,
    ask_pie_rule(GameState, Player1, Player2, TurnCount).

% Clause for handling turn in all other cases.
handle_turn(GameState, Player1, Player2, CurrentPlayer, TurnCount, PieRule) :-
    TurnCount \= 2,
    continue_turn(GameState, Player1, Player2, CurrentPlayer, TurnCount, PieRule).

% Clause for handling pie rule when Player1 is a hard computer.
ask_pie_rule(GameState, computer(2), Player2, TurnCount) :-
    pie_rule_decision(GameState, Response),
    nl, format("Computer chooses to ~w to switch places.~n", [Response]),
    handle_pie_rule_response(GameState, computer(2), Player2, TurnCount, Response).

% Clause for handling pie rule when Player1 is an easy computer.
ask_pie_rule(GameState, computer(1), Player2, TurnCount) :-
    random_pie_rule_response(Response),
    nl, format("Computer chooses to ~w to switch places.~n", [Response]),
    handle_pie_rule_response(GameState, computer(1), Player2, TurnCount, Response).

% Clause for handling pie rule when Player1 is a human player.
ask_pie_rule(GameState, Player1, Player2, TurnCount) :-
    repeat,
    nl, write('Would you like to switch places with your opponent? y/n : '),
    catch(read(Answer), _, fail),
    process_human_pie_response(Answer, GameState, Player1, Player2, TurnCount),!.

% Handle human input for pie rule.

process_human_pie_response(Answer, GameState, Player1, Player2, TurnCount) :-
    member(Answer, ['y', 'n']),
    handle_pie_rule_response(GameState, Player1, Player2, TurnCount, Answer), !.

process_human_pie_response(_, _, _, _, _) :-
    write('Invalid choice! Please enter y, n.'), nl, fail.

% Clause when there are pieces in central positions.
pie_rule_decision(state(Board, white), 'y') :-
    length(Board, Size),
    central_positions(Size, Central),
    count_pieces(Board, white, Central, Count),
    Count > 0.

% Clause when there are no pieces in central positions.
pie_rule_decision(state(Board, white), 'n') :-
    length(Board, Size),
    central_positions(Size, Central),
    count_pieces(Board, white, Central, Count),
    Count =< 0.

central_positions(Size, Positions) :-
    Mid is (Size + 1) div 2,
    findall((R,C), (
        member(R, [Mid-1, Mid, Mid+1]),
        member(C, [Mid-1, Mid, Mid+1]),
        R > 0, R =< Size,
        C > 0, C =< Size
    ), Positions).


count_pieces(Board, Player, Positions, Count) :-
    findall(Position, (member(Position, Positions), is_player_at(Board, Position, Player)), Found),
    length(Found, Count).


is_player_at(Board, (Row, Col), Player) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Cell),
    Cell = Player.


random_pie_rule_response('y') :-
    random(0.0, 1.0, Rand), 
    Rand < 0.5.
random_pie_rule_response('n') :-
    random(0.0, 1.0, Rand), 
    Rand >= 0.5.

handle_pie_rule_response(GameState, Player1, Player2, TurnCount, 'y') :-
    NewTurnCount is TurnCount + 1,
    game_loop(GameState, Player1, Player2, NewTurnCount, 'y').

handle_pie_rule_response(GameState, Player1, Player2, TurnCount, 'n') :-
    NewTurnCount is TurnCount + 1,
    game_loop(GameState, Player1, Player2, NewTurnCount, 'n').

handle_pie_rule_response(GameState, Player1, Player2, TurnCount, _) :-
    write('Invalid choice, try again.'), nl,
    ask_pie_rule(GameState, Player1, Player2, TurnCount, 'n').

continue_turn(GameState, Player1, Player2, CurrentPlayer, TurnCount, PieRule) :-
    determine_player_type(CurrentPlayer, Player1, Player2, PieRule, CurrentPlayerType),
    handle_line_of_three(GameState, CurrentPlayerType, UpdatedGameState),
    choose_move(UpdatedGameState, CurrentPlayerType, Move),
    continue_turn_move(UpdatedGameState, Player1, Player2, CurrentPlayerType, TurnCount, PieRule, Move).

continue_turn_move(_,_,_,_,_,_,exit).

continue_turn_move(UpdatedGameState, Player1, Player2, CurrentPlayerType, TurnCount, PieRule, Move) :-
    move(UpdatedGameState, Move, TempGameState),
    handle_line_of_three(TempGameState, CurrentPlayerType, NewGameState),
    NewTurnCount is TurnCount + 1,
    game_loop(NewGameState, Player1, Player2, NewTurnCount, PieRule).
    


% Clause for determining player type when PieRule is 'y' and CurrentPlayer is black.
determine_player_type(black, Player1, _, 'y', Player1).

% Clause for determining player type when PieRule is not 'y' and CurrentPlayer is white.
determine_player_type(white, Player1, _, PieRule, Player1) :-
    PieRule \= 'y'.

% Clause for determining player type for all other cases.
determine_player_type(_, _, Player2, _, Player2).

start_game(Player1, Player2, Size) :-
    initial_state(Size, GameState),
    game_loop(GameState, Player1, Player2, 0, 'n').

choose_move(GameState, computer(1), Move) :-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).

choose_move(GameState, computer(2), Move) :-
    valid_moves(GameState, Moves),
    best_greedy_move(GameState, Moves, Move).

choose_move(GameState, human, Move) :-
    repeat,
    nl, write('(0 to exit)'),
    nl, write('Enter your move as (Row, Col): '),
    catch(read(Input), _, fail),
    handle_move_input(GameState, Input, Move).


%Handling human move input.
%Clause for exiting the game.
handle_move_input(_, 0, exit).
    
%Both Input and Move are valid.
handle_move_input(state(Board, Player), (Row, Col), Move) :-
    length(Board, Size),
    valid_input(Row, Col, Size),   
    valid_move(GameState, move(Row, Col)), 
    !,                      
    Move = move(Row, Col).  

%Invalid Input.
handle_move_input(state(Board, Player), (Row, Col), _) :-
    length(Board, Size),
    \+ valid_input(Row, Col, Size),
    write('Invalid input! Please enter (Row, Col) .'), nl,
    fail.

%Valid Input, Invalid Move.
handle_move_input(state(Board, Player), (Row, Col), _) :-
    length(Board, Size),
    valid_input(Row, Col, Size),    
    \+ valid_move(GameState, move(Row, Col)),
    write('Invalid move! That cell is already occupied or invalid.'), nl,
    fail.

%Helper function to validate Input.
valid_input(Row, Col, Size) :-
    integer(Row), integer(Col),
    Row > 0, Row =< Size,
    Col > 0, Col =< Size.
    


valid_move(state(Board, _), move(Row, Col)) :-
    valid_position(Board, Row, Col).

move(state(Board, Player), move(Row, Col), state(NewBoard, NextPlayer)) :-
    valid_moves(state(Board, Player), Moves),
    member(move(Row, Col), Moves),
    apply_move(Board, Row, Col, Player, NewBoard),
    next_player(Player, NextPlayer).

apply_move(Board, Row, Col, Player, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_nth(Col, OldRow, Player, NewRow),
    replace_nth(Row, Board, NewRow, NewBoard).

replace_nth(1, [_|Rest], Elem, [Elem|Rest]).
replace_nth(N, [X|Xs], Elem, [X|Ys]) :-
    N > 1,
    N1 is N - 1,
    replace_nth(N1, Xs, Elem, Ys).

valid_moves(state(Board, _), Moves) :-
    findall(move(Row, Col), valid_position(Board, Row, Col), Moves).

valid_position(Board, Row, Col) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Cell),
    Cell = empty.

% Clause for handling the line of three when PlayerType is human.
handle_line_of_three(state(Board, Player), human, state(NewBoard, Player)) :-
    next_player(Player, NextPlayer),
    find_lines_of_three(Board, NextPlayer, Lines),
    Lines \= [],
    write('Lines of three found: '), write(Lines), nl,
    choose_two_to_remove(Lines, StackPos),
    handle_removal(StackPos, Board, Lines, NextPlayer, NewBoard).

% Clause for handling the line of three when PlayerType is computer.
handle_line_of_three(state(Board, Player), computer(_), state(NewBoard, Player)) :-
    next_player(Player, NextPlayer),
    find_lines_of_three(Board, NextPlayer, Lines),
    Lines \= [],
    choose_best_removal(Lines, ToRemove, StackPos),
    update_board(Board, ToRemove, StackPos, NextPlayer, NewBoard).

% Clause for when no line of three is found or PlayerType is not relevant.
handle_line_of_three(GameState, _, GameState).

% Handle removal logic (common for both human and computer).
handle_removal(exit, _, _, _, _) :- true.  % Exit condition.

% Handle removal when StackPos is not exit.
handle_removal(StackPos, Board, Lines, NextPlayer, NewBoard) :-
    StackPos \= exit,
    select_removal(StackPos, Lines, ToRemove),
    update_board(Board, ToRemove, StackPos, NextPlayer, NewBoard).


select_removal(StackPos, [Line|_], ToRemove) :-
    exclude(==(StackPos), Line, ToRemove).


choose_best_removal([Line|_], ToRemove, StackPos) :-
    Line = [(R1, C1), (R2, C2), (R3, C3)],
    nth1(R1, Board, Row1), nth1(C1, Row1, Player),
    nth1(R2, Board, Row2), nth1(C2, Row2, Player),
    nth1(R3, Board, Row3), nth1(C3, Row3, Player),
    ToRemove = [(R1, C1), (R2, C2)],
    StackPos = (R3, C3),
    write('To Remove: '), write(ToRemove), nl,
    write('Stack Position: '), write(StackPos), nl.

find_lines_of_three(Board, Player, Lines) :-
    setof(Line, (valid_line(Board, Player, Line)), Lines).

valid_line(Board, Player, Line) :-
    (check_lines(Board, Player, Line);
     check_columns(Board, Player, Line);
     check_diagonals(Board, Player, Line)).

check_lines(Board, Player, Line) :-
    nth1(RowIdx, Board, Row),
    check_line(Row, RowIdx, Player, Line).

check_columns(Board, Player, Line) :-
    transpose(Board, TransposedBoard),
    nth1(ColIdx, TransposedBoard, Col),
    check_line(Col, ColIdx, Player, TempLine),
    maplist(swap_coords, TempLine, Line).

swap_coords((X, Y), (Y, X)).

check_diagonals(Board, Player, Line) :-
    diagonal(Board, Diagonals),
    member(Diagonal, Diagonals),
    check_diagonal_line(Diagonal, Player, Line).

check_diagonal_line(Line, Player, Result) :-
    append(_, [Player, Player, Player|_], Line),
    findall((Row, Col), 
            (nth1(Index, Line, Player), 
             Row is 8 - Index, 
             Col is Index), 
            Result).

check_line(Line, RowIdx, Player, Result) :-
    append(Beginning, [Player, Player, Player|_], Line),
 length(Beginning, Offset),
        O1 is Offset +1,
        O2 is Offset +2,
        O3 is Offset +3,
    Result = [
        (RowIdx,O1),
        (RowIdx, O2),
        (RowIdx, O3)
    ].


% Clause for handling valid input for selecting a position to stack.
choose_two_to_remove(Lines, (Row, Col)) :-
    repeat, 
    nl, write('(0 to exit)'),
    nl, write('Enter position to stack (e.g., (Rs, Cs)) : '),
    catch(read(Input), _, fail),
    process_valid_position(Lines, Input, Row, Col).

% Process the exit condition.
process_valid_position(_,0,_,_) :-
    nl, write('Exiting the game. Goodbye!'), nl, !.

% Process valid position selection.
process_valid_position(Lines, (Row, Col), Row, Col) :-
    integer(Row), integer(Col), 
    member(Line, Lines), member((Row, Col), Line),
    nl, format("Position ~w selected for stacking.~n", [(Row, Col)]), !.

% Handle invalid inputs.
process_valid_position(_,_,_,_) :-
    write('Invalid input! Please enter a valid position or 0 to exit.'), nl, fail.


update_board(Board, ToRemove, StackPos, Player, NewBoard) :-
    remove_pieces(Board, ToRemove, TempBoard),
    add_stack(TempBoard, StackPos, Player, NewBoard).

remove_pieces(Board, [], Board).
remove_pieces(Board, [(Row, Col)|Rest], NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_nth(Col, OldRow, empty, NewRow),
    replace_nth(Row, Board, NewRow, TempBoard),
    remove_pieces(TempBoard, Rest, NewBoard).

add_stack(Board, (Row, Col), white, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_nth(Col, OldRow, 8, NewRow), 
    replace_nth(Row, Board, NewRow, NewBoard).
add_stack(Board, (Row, Col), black, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_nth(Col, OldRow, x, NewRow),
    replace_nth(Row, Board, NewRow, NewBoard).

% Clause for when a line of stacks is found (there is a winner).
game_over(state(Board, _), Winner) :-
    line_of_stacks(Board, Winner), !.

% Clause for when the board is full and no winner (draw).
game_over(state(Board, _), draw) :-
    board_full(Board), !.

% Clause for when no winner and the board is not full.
game_over(state(_, _), none).


line_of_stacks(Board, Winner) :-
    stack_symbol(Winner, Stack),
    (check_lines_stacks(Board, Stack);
     check_columns_stacks(Board, Stack);
     check_diagonals_stacks(Board, Stack)).

check_lines_stacks(Board, Stack) :-
    member(Row, Board),
    append(_, [Stack, Stack, Stack|_], Row).

check_columns_stacks(Board, Stack) :-
    transpose(Board, TransposedBoard),
    member(Column, TransposedBoard),
    append(_, [Stack, Stack, Stack|_], Column).

check_diagonals_stacks(Board, Stack) :-
    diagonal(Board, Diagonals),
    member(Diagonal, Diagonals),
    % Extract just the values from the diagonal positions
    findall(Value, member((Value,_), Diagonal), Values),
    append(_, [Stack, Stack, Stack|_], Values).

stack_symbol(white, 8).
stack_symbol(black, x).

board_full(Board) :-
    \+ (member(Row, Board), member(empty, Row)).


next_player(white, black).
next_player(black, white).

%----------------------------------------------------------------

% Encontrar o melhor movimento baseado em estrat�gia greedy
best_greedy_move(state(Board, Player), Moves, BestMove) :-
    findall(Score-Move, (
        member(Move, Moves),
        evaluate_greedy_move(state(Board, Player), Move, Score)
    ), ScoredMoves),
    % Select move with highest score
    keysort(ScoredMoves, Sorted),
    last(Sorted, _-BestMove).

% Evaluate a potential move with weighted priorities
evaluate_greedy_move(state(Board, Player), move(Row, Col), FinalScore) :-
    apply_move(Board, Row, Col, Player, NewBoard),
    next_player(Player, Opponent),
    
    (blocks_opponent_line(Board, Row, Col, Opponent, BlockingScore)
    ; BlockingScore = 0),
    
    (can_form_own_line(Board, Row, Col, Player, FormLineScore)
    ; FormLineScore = 0),
    
    calculate_proximity_score(Board, Row, Col, Opponent, ProximityScore),
    
    % Weight the different factors
    FinalScore is BlockingScore * 1000 + FormLineScore * 800 + ProximityScore * 100.

% Clause for when there are blocking lines (Count > 0).
blocks_opponent_line(Board, Row, Col, Opponent, Score) :-
    findall(Line, (
        find_line_of_two(Board, Opponent, Line),
        can_block_line(Line, Row, Col)
    ), BlockingLines),
    length(BlockingLines, Count),
    Count > 0,
    Score = Count.

% Clause for when there are no blocking lines (Count = 0).
blocks_opponent_line(_, _, _, _, 0).

find_line_of_two(Board, Player, Line) :-
    % Check horizontal lines
    (check_horizontal_two(Board, Player, Line);
     % Check vertical lines
     check_vertical_two(Board, Player, Line);
     % Check diagonal lines
     check_diagonals_of_two(Board, Player, Line)).

% Check horizontal sequences of two pieces
check_horizontal_two(Board, Player, [(Row,Col1), (Row,Col2)]) :-
    length(Board, Size),
    between(1, Size, Row),
    between(1, Size-1, Col1),
    Col2 is Col1 + 1,
    nth1(Row, Board, RowList),
    nth1(Col1, RowList, Player),
    nth1(Col2, RowList, Player),
    (Col1 > 1, Col3 is Col1 - 1, nth1(Col3, RowList, empty);
     Col2 < Size, Col3 is Col2 + 1, nth1(Col3, RowList, empty)).

% Check vertical sequences of two pieces
check_vertical_two(Board, Player, [(Row1,Col), (Row2,Col)]) :-
    length(Board, Size),
    between(1, Size-1, Row1),
    between(1, Size, Col),
    Row2 is Row1 + 1,
    nth1(Row1, Board, Row1List),
    nth1(Row2, Board, Row2List),
    nth1(Col, Row1List, Player),
    nth1(Col, Row2List, Player),
    (Row1 > 1, Row3 is Row1 - 1, nth1(Row3, Board, Row3List), nth1(Col, Row3List, empty);
     Row2 < Size, Row3 is Row2 + 1, nth1(Row3, Board, Row3List), nth1(Col, Row3List, empty)).

can_block_line([(R1,C1), (R2,C2)], Row, Col) :-
    % Calculate the position that would complete the line
    predict_third_position((R1,C1), (R2,C2), (Row,Col)).

predict_third_position((R1, C1), (R2, C2), (R3, C3)) :-
    (   R1 = R2, R3 = R1,
        (C3 is C1 - 1; C3 is C2 + 1)
    );
    (   C1 = C2, C3 = C1,
        (R3 is R1 - 1; R3 is R2 + 1)
    );
   
    (   DR is R2 - R1, 
        DC is C2 - C1, 
        abs(DR) =:= abs(DC), 
        (   
            R3 is R1 - DR,
            C3 is C1 - DC
        ;   
            R3 is R2 + DR,
            C3 is C2 + DC
        )
    ).


can_form_own_line(Board, Row, Col, Player, Score) :-
    % Count how many potential lines this move could form
    length(Board, Size),
    findall(1, (
        find_line_of_two(Board, Player, Line),
        can_complete_line(Line, Row, Col, Size)
    ), Counts),
    length(Counts, Score).

can_complete_line([(R1,C1), (R2,C2)], Row, Col, Size) :-
    DR is R2 - R1,
    DC is C2 - C1,
    
    (
        Row =:= R1 - DR,
        Col =:= C1 - DC
    ;
        Row =:= R2 + DR,
        Col =:= C2 + DC
    ),
    Row > 0, Row =< Size, Col > 0, Col =< Size.

% Clause for when there are opponent pieces on the board.
calculate_proximity_score(Board, Row, Col, Opponent, Score) :-
    length(Board, Size),
    findall(Distance, (
        find_opponent_piece(Board, Opponent, OppRow, OppCol),
        calculate_distance((Row, Col), (OppRow, OppCol), Distance)
    ), Distances),
    Distances \= [], % Ensure there are distances
    min_list(Distances, MinDistance),
    Score is max(0, Size - MinDistance).

% Clause for when there are no opponent pieces (no proximity score).
calculate_proximity_score(_, _, _, _, 0).


% Helper to find opponent pieces on the board
find_opponent_piece(Board, Opponent, Row, Col) :-
    length(Board, Size),
    between(1, Size, Row),
    between(1, Size, Col),
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Opponent).

% Calculate Manhattan distance between two positions
calculate_distance((R1,C1), (R2,C2), Distance) :-
    Distance is abs(R1-R2) + abs(C1-C2).

min_list([H|T], Min) :- min_list(T, H, Min).
min_list([], Min, Min).
min_list([H|T], CurrentMin, Min) :-
    H < CurrentMin,
    min_list(T, H, Min).
min_list([H|T], CurrentMin, Min) :-
    H >= CurrentMin,
    min_list(T, CurrentMin, Min).

find_lines_of_two(Board, Player, Lines) :-
    setof(Line, (valid_line_of_two(Board, Player, Line)), Lines), !.
find_lines_of_two(_, _, []).

% Diagonal descendent
check_diagonals_of_two(Board, Player, [(R1, C1), (R2, C2)]) :-
    length(Board, Size),
    between(1, Size, R1),
    between(1, Size, C1),
    R2 is R1 + 1,
    C2 is C1 + 1,
    nth1(R1, Board, Row1),
    nth1(R2, Board, Row2),
    nth1(C1, Row1, Player),
    nth1(C2, Row2, Player),
    (   
        R3 is R1 - 1,
        C3 is C1 - 1,
        validate_diagonal_position(Board, R3, C3)
    ;   
        R3 is R2 + 1,
        C3 is C2 + 1,
        validate_diagonal_position(Board, R3, C3)
    ).

% Diagonal ascendent
check_diagonals_of_two(Board, Player, [(R1, C1), (R2, C2)]) :-
    length(Board, Size),
    between(1, Size, R1),
    between(1, Size, C1),

    R2 is R1 - 1,
    C2 is C1 + 1,
    nth1(R1, Board, Row1),
    nth1(R2, Board, Row2),
    nth1(C1, Row1, Player),
    nth1(C2, Row2, Player),
    (   
        R3 is R1 + 1,
        C3 is C1 - 1,
        validate_diagonal_position(Board, R3, C3)
    ;   
        R3 is R2 - 1,
        C3 is C2 + 1,
        validate_diagonal_position(Board, R3, C3)
    ).

validate_diagonal_position(Board, Row, Col) :-
    length(Board, Size),
    Row > 0, Row =< Size,
    Col > 0, Col =< Size,
    nth1(Row, Board, RowList),
    nth1(Col, RowList, empty).



heuristic_distance((R1, C1), (R2, C2), Distance) :-
    Distance is abs(R1 - R2) + abs(C1 - C2).

%----------------------------------------------------------------

% Extract all diagonals from a board
diagonal(Board, Diagonals) :-
    findall(Diag, diagonal_down(Board, Diag), DownDiags),
    findall(Diag, diagonal_up(Board, Diag), UpDiags),
    append(DownDiags, UpDiags, Diagonals).

diagonal_down(Board, Diagonal) :-
    length(Board, N),
    between(1, N, StartRow),
    diagonal_down_from(Board, StartRow, 1, Diagonal).
diagonal_down(Board, Diagonal) :-
    length(Board, N),
    between(2, N, StartCol),
    diagonal_down_from(Board, 1, StartCol, Diagonal).

diagonal_down_from(Board, Row, Col, []) :-
    length(Board, N),
    (Row > N ; Col > N).
diagonal_down_from(Board, Row, Col, [(Value,(Row,Col))|Rest]) :-
    length(Board, N),
    Row =< N,
    Col =< N,
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Value),
    NextRow is Row + 1,
    NextCol is Col + 1,
    diagonal_down_from(Board, NextRow, NextCol, Rest).

diagonal_up(Board, Diagonal) :-
    length(Board, N),
    between(1, N, StartRow),
    diagonal_up_from(Board, StartRow, 1, Diagonal).
diagonal_up(Board, Diagonal) :-
    length(Board, N),
    between(2, N, StartCol),
    diagonal_up_from(Board, N, StartCol, Diagonal).

diagonal_up_from(Board, Row, Col, []) :-
    length(Board, Size),
    (Row < 1 ; Col > Size).
diagonal_up_from(Board, Row, Col, [(Value,(Row,Col))|Rest]) :-
    length(Board, Size),
    Row >= 1,
    Col =< Size,
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Value),
    NextRow is Row - 1,
    NextCol is Col + 1,
    diagonal_up_from(Board, NextRow, NextCol, Rest).

check_diagonals(Board, Player, Line) :-
    diagonal(Board, Diagonals),
    member(Diagonal, Diagonals),
    find_three_consecutive(Diagonal, Player, Line).

find_three_consecutive([(Player,(R1,C1)), (Player,(R2,C2)), (Player,(R3,C3))|_], Player, [(R1,C1), (R2,C2), (R3,C3)]).
find_three_consecutive([_|Rest], Player, Line) :-
    find_three_consecutive(Rest, Player, Line).

between(Low, High, Low) :-
    Low =< High.
between(Low, High, Value) :-
    Low < High,
    Next is Low + 1,
    between(Next, High, Value).

