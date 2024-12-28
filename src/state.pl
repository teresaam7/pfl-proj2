initial_state(state(Board, white)) :-
    empty_board(Board).

empty_board(Board) :-
    length(Board, 7),
    maplist(empty_row, Board).

empty_row(Row) :-
    length(Row, 7),
    maplist(=(empty), Row).

game_loop(GameState, Player1, Player2, TurnCount, PieRule) :-
    display_game(GameState),
    game_over(GameState, Winner),
    (Winner \= none ->
        nl, format("Game over! Winner: ~w~n", [Winner])
    ;
        GameState = state(_, CurrentPlayer),
        handle_turn(GameState, Player1, Player2, CurrentPlayer, TurnCount, PieRule)
    ).
game_loop(_, _, _, _, _) :-
    nl, write('Game exited. Thank you for playing!'), !.

handle_turn(GameState, Player1, Player2, CurrentPlayer, TurnCount, PieRule) :-
    (TurnCount =:= 2 ->
        ask_pie_rule(GameState, Player1, Player2, TurnCount)
    ;
        continue_turn(GameState, Player1, Player2, CurrentPlayer, TurnCount, PieRule)
    ).

ask_pie_rule(GameState, Player1, Player2, TurnCount) :-
    (Player1 = computer(2) ->
        % Hard computer uses heuristic to decide
        pie_rule_decision(GameState, Response),
        nl, format("Computer chooses to ~w to switch places.~n", [Response]),
        handle_pie_rule_response(GameState, Player1, Player2, TurnCount, Response)
    ;
     Player1 = computer(1)->
        % Easy computer uses random choice
        random_pie_rule_response(Response),
        nl, format("Computer chooses to ~w to switch places.~n", [Response]),
        handle_pie_rule_response(GameState, Player1, Player2, TurnCount, Response)
    ;
        % Human player input
        repeat,  % Loop until valid input
        nl, write('(0 to exit)'),
        nl, write('Would you like to switch places with your opponent? y/n : '),
        catch(read(Answer), _, fail),
        (
            Answer = 0 ->  % Exit condition
            nl, write('Exiting the game. Goodbye!'), nl, !, fail   % Terminate Prolog execution
        ;
            member(Answer, ['y', 'n']) ->  % Valid pie rule input
            handle_pie_rule_response(GameState, Player1, Player2, TurnCount, Answer), !
        ;
            write('Invalid choice! Please enter y, n.'), nl, fail
        )
    ).


pie_rule_decision(state(Board, white), Response) :-
    central_positions(Central),
    count_pieces(Board, white, Central, Count),
    (Count > 0 -> Response = 'y'; Response = 'n').
central_positions([(4, 4), (3, 4), (4, 3), (4, 5), (5, 4), (3, 3), (3, 5), (5, 3), (5, 5)]).


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
    (Move = exit -> true ;
    move(UpdatedGameState, Move, TempGameState),
    handle_line_of_three(TempGameState, CurrentPlayerType, NewGameState),
    NewTurnCount is TurnCount + 1,
    game_loop(NewGameState, Player1, Player2, NewTurnCount, PieRule)).

determine_player_type(CurrentPlayer, Player1, Player2, PieRule, CurrentPlayerType) :-
    ( (PieRule = 'y', CurrentPlayer = black ; PieRule \= 'y', CurrentPlayer = white) ->
        CurrentPlayerType = Player1
    ;
        CurrentPlayerType = Player2
    ).


start_game(Player1, Player2) :-
    initial_state(GameState),
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
    (
        Input = 0 ->  
        nl, write('Exiting the game. Goodbye!'), nl, Move = exit, !   
    ;
        Input = (Row, Col), integer(Row), integer(Col), Row > 0, Row =< 7, Col > 0, Col =< 7 ->
        (valid_move(GameState, move(Row, Col)) ->
            Move = move(Row, Col), !
        ;
            write('Invalid move! That cell is already occupied or invalid.'), nl, fail
        )
    ;
        write('Invalid input! Please enter (Row, Col) .'), nl, fail
    ).



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

handle_line_of_three(state(Board, Player), PlayerType, state(NewBoard, Player)) :-
    next_player(Player, NextPlayer),
    find_lines_of_three(Board, NextPlayer, Lines),
    Lines \= [],
    (
        PlayerType = human ->
            write('Lines of three found: '), write(Lines), nl,
            choose_two_to_remove(Lines, StackPos),
            (StackPos = exit -> true ; 
            select_removal(StackPos, Lines, ToRemove),
            update_board(Board, ToRemove, StackPos, NextPlayer, NewBoard))
        ;
        PlayerType = computer(_) ->
            choose_best_removal(Lines, ToRemove, StackPos),
            update_board(Board, ToRemove, StackPos, NextPlayer, NewBoard)
    ).
handle_line_of_three(GameState, _, GameState).


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

choose_two_to_remove(Lines, StackPos) :-
    repeat, 
    nl, write('(0 to exit)'),
    nl, write('Enter position to stack (e.g., (Rs, Cs)) : '),
    catch(read(Input), _, fail),
    (
        Input = 0 ->  % Exit condition
        nl, write('Exiting the game. Goodbye!'),  nl, StackPos = exit, !
    ;
        Input = (Row, Col), integer(Row), integer(Col), 
        member(Line, Lines), member((Row, Col), Line) -> 
        StackPos = (Row, Col), 
        nl, format("Position ~w selected for stacking.~n", [StackPos]), !
    ;
        write('Invalid position! Please enter a valid (Row, Col) from the lines of three.'), nl, fail
    ).



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

game_over(state(Board, _), Winner) :-
    (line_of_stacks(Board, Winner) -> true ;
    board_full(Board) -> Winner = draw ;
    Winner = none).

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
    append(_, [Stack, Stack, Stack|_], Diagonal).


stack_symbol(white, 8).
stack_symbol(black, x).

board_full(Board) :-
    \+ (member(Row, Board), member(empty, Row)).


next_player(white, black).
next_player(black, white).

%----------------------------------------------------------------

% Encontrar o melhor movimento baseado em estratégia greedy
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

% Check if move blocks opponent from forming a line of three
blocks_opponent_line(Board, Row, Col, Opponent, Score) :-
    % Find all possible lines of two for opponent
    findall(Line, (
        find_line_of_two(Board, Opponent, Line),
        can_block_line(Line, Row, Col)
    ), BlockingLines),
    length(BlockingLines, Count),
    (Count > 0 -> Score = Count ; Score = 0).

find_line_of_two(Board, Player, Line) :-
    % Check horizontal lines
    (check_horizontal_two(Board, Player, Line);
     % Check vertical lines
     check_vertical_two(Board, Player, Line);
     % Check diagonal lines
     check_diagonals_of_two(Board, Player, Line)).

% Check horizontal sequences of two pieces
check_horizontal_two(Board, Player, [(Row,Col1), (Row,Col2)]) :-
    between(1, 7, Row),
    between(1, 6, Col1),
    Col2 is Col1 + 1,
    nth1(Row, Board, RowList),
    nth1(Col1, RowList, Player),
    nth1(Col2, RowList, Player),
    (Col1 > 1, Col3 is Col1 - 1, nth1(Col3, RowList, empty);
     Col2 < 7, Col3 is Col2 + 1, nth1(Col3, RowList, empty)).

% Check vertical sequences of two pieces
check_vertical_two(Board, Player, [(Row1,Col), (Row2,Col)]) :-
    between(1, 6, Row1),
    between(1, 7, Col),
    Row2 is Row1 + 1,
    nth1(Row1, Board, Row1List),
    nth1(Row2, Board, Row2List),
    nth1(Col, Row1List, Player),
    nth1(Col, Row2List, Player),
    (Row1 > 1, Row3 is Row1 - 1, nth1(Row3, Board, Row3List), nth1(Col, Row3List, empty);
     Row2 < 7, Row3 is Row2 + 1, nth1(Row3, Board, Row3List), nth1(Col, Row3List, empty)).

can_block_line([(R1,C1), (R2,C2)], Row, Col) :-
    % Calculate the position that would complete the line
    predict_third_position((R1,C1), (R2,C2), (Row,Col)).

can_form_own_line(Board, Row, Col, Player, Score) :-
    % Count how many potential lines this move could form
    findall(1, (
        find_line_of_two(Board, Player, Line),
        can_complete_line(Line, Row, Col)
    ), Counts),
    length(Counts, Score).

can_complete_line([(R1,C1), (R2,C2)], Row, Col) :-
    DR is R2 - R1,
    DC is C2 - C1,
    
    (
        Row =:= R1 - DR,
        Col =:= C1 - DC
    ;
        Row =:= R2 + DR,
        Col =:= C2 + DC
    ),
    Row > 0, Row =< 7, Col > 0, Col =< 7.

calculate_proximity_score(Board, Row, Col, Opponent, Score) :-
    findall(Distance, (
        find_opponent_piece(Board, Opponent, OppRow, OppCol),
        calculate_distance((Row,Col), (OppRow,OppCol), Distance)
    ), Distances),
    (Distances = [] -> Score = 0 ;
    min_list(Distances, MinDistance),
    % Convert distance to score (closer = higher score)
    Score is max(0, 7 - MinDistance)).

% Helper to find opponent pieces on the board
find_opponent_piece(Board, Opponent, Row, Col) :-
    between(1, 7, Row),
    between(1, 7, Col),
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

% Encontrar linhas de duas peças
find_lines_of_two(Board, Player, Lines) :-
    setof(Line, (valid_line_of_two(Board, Player, Line)), Lines), !.
find_lines_of_two(_, _, []).

check_diagonals_of_two(Board, Player, [(R1, C1), (R2, C2)]) :-
    between(1, 7, R1),
    between(1, 7, C1),
    between(1, 7, R2),
    between(1, 7, C2),
    abs(R2 - R1) =:= 1,          % As posições devem ser adjacentes na linha
    abs(C2 - C1) =:= 1,          % As posições devem ser adjacentes na coluna
    nth1(R1, Board, Row1),
    nth1(R2, Board, Row2),
    nth1(C1, Row1, Player),
    nth1(C2, Row2, Player),
    % Validar posição antes ou depois da sequência
    (   % Posição antes da sequência
        R3 is R1 - (R2 - R1),
        C3 is C1 - (C2 - C1),
        validate_diagonal_position(Board, R3, C3)
    ;   % Posição depois da sequência
        R3 is R2 + (R2 - R1),
        C3 is C2 + (C2 - C1),
        validate_diagonal_position(Board, R3, C3)
    ).

% Validar posições de uma diagonal
validate_diagonal_position(Board, Row, Col) :-
    Row > 0,
    Row =< 7,
    Col > 0,
    Col =< 7,
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
    (Row < 1 ; Col > 7).
diagonal_up_from(Board, Row, Col, [(Value,(Row,Col))|Rest]) :-
    Row >= 1,
    Col =< 7,
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



