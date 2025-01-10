main_menu :-
    display_logo_banner,
    get_valid_board_size.


display_logo_banner :-
    nl,nl,
    write('\e[33m+------------------------------------------------------------------------+\e[0m'),nl,
    write('\e[33m|                                                                        |\e[0m'),nl,
    write('\e[33m|   \e[34mLLLLLLLLLLL                  OOOOOOOOO     TTTTTTTTTTTTTTTTTTTTTTT   \e[33m|\e[0m'),nl,
    write('\e[33m|   \e[34mL:::::::::L                OO:::::::::OO   T:::::::::::::::::::::T   \e[33m|\e[0m'),nl,
    write('\e[33m|   \e[34mL:::::::::L              OO:::::::::::::OO T:::::::::::::::::::::T   \e[33m|\e[0m'),nl,
    write('\e[33m|   \e[34mLL:::::::LL             O:::::::OOO:::::::OT:::::TT:::::::TT:::::T   \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[34mL:::::L               O::::::O   O::::::OTTTTTT  T:::::T  TTTTTT   \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[32mL:::::L               O:::::O     O:::::O        T:::::T           \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[32mL:::::L               O:::::O     O:::::O        T:::::T           \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[32mL:::::L               O:::::O     O:::::O        T:::::T           \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[32mL:::::L               O:::::O     O:::::O        T:::::T           \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[36mL:::::L               O:::::O     O:::::O        T:::::T           \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[36mL:::::L               O:::::O     O:::::O        T:::::T           \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[36mL:::::L         LLLLLLO::::::O   O::::::O        T:::::T           \e[33m|\e[0m'),nl,
    write('\e[33m|   \e[36mLL:::::::LLLLLLLLL:::::LO:::::::OOO:::::::O      TT:::::::TT         \e[33m|\e[0m'),nl,
    write('\e[33m|   \e[35mL::::::::::::::::::::::L OO:::::::::::::OO       T:::::::::T         \e[33m|\e[0m'),nl,
    write('\e[33m|   \e[35mL::::::::::::::::::::::L   OO:::::::::OO         T:::::::::T         \e[33m|\e[0m'),nl,
    write('\e[33m|   \e[35mLLLLLLLLLLLLLLLLLLLLLLLL     OOOOOOOOO           TTTTTTTTTTT         \e[33m|\e[0m'),nl,
    write('\e[33m|                                                                        |\e[0m'),nl,
    write('\e[33m|          Please enter the board size (minimum 5, maximum 30):          |\e[0m'),nl,
    write('\e[33m|                                                                        |\e[0m'),nl,
    write('\e[33m+------------------------------------------------------------------------+\e[0m'),nl,nl.

get_valid_board_size :-
    write('\e[37mEnter board size: \e[0m'),
    read_board_size(Size),
    process_board_size(Size).

read_board_size(Size) :-
    catch(read(Size), Error, (
        write('Invalid input. Please enter a number.'), nl,
        get_valid_board_size
    )).

process_board_size(Size) :-
    validate_board_size(Size).

validate_board_size(Size) :-
    integer(Size),
    Size >= 5,
    Size =< 30,
    !,
    show_game_options(Size).

validate_board_size(_) :-
    write('Board size must be between 5 and 30. Please try again.'), nl,
    get_valid_board_size.

% Display an intermediate state
intermediate_state_1(State, black) :-
    State = [[white, black, white, empty, empty, empty, empty],
             [empty, white, empty, empty, empty, empty, empty],
             [white, black, black, empty, empty, empty, empty],
             [empty, empty, empty, empty, empty, empty, empty],
             [empty, empty, empty, empty, empty, empty, empty],
             [empty, empty, empty, empty, empty, empty, empty],
             [empty, empty, empty, empty, empty, empty, empty]].

% Display a near-final state where black is almost winning
near_final_state_1(State, white) :-
    State = [[white, black, empty, empty, empty, empty, empty],
             [empty, empty, empty, empty, empty, empty, empty],
             [8, black, black, empty, empty, empty, empty],
             [empty, empty, empty, empty, empty, empty, empty],
             [empty, empty, empty, empty, empty, empty, empty],
             [empty, empty, empty, empty, empty, empty, empty],
             [empty, empty, empty, empty, empty, empty, empty]].


% Final state where white has won by forming a horizontal line of 3 pieces
final_state_white_victory(State, white) :-
    State = [[white, black, empty, empty, empty, empty, empty],
             [empty, empty, empty, empty, empty, empty, empty],
             [8,     empty, empty,     x, empty, empty, empty],
             [empty, 8    , black,     x, empty, empty, empty],
             [empty, empty,     8, empty, black, empty, empty],
             [empty, empty, white, empty, empty, empty, empty],
             [empty, empty, black, empty, empty, empty, empty]].


% Show game options with updated intermediate state handling
show_game_options(Size) :-
    nl, nl,
    write('\e[33m+------------------------------------------------------------------------+\e[0m'), nl,
    write('\e[33m|                                                                        |\e[0m'), nl,
    write('\e[33m|                      1) Player vs Player                               |\e[0m'), nl,
    write('\e[33m|                      2) Player vs Computer                             |\e[0m'), nl,
    write('\e[33m|                      3) Computer vs Computer                           |\e[0m'), nl,
    write('\e[33m|                      4) Show intermediate state 1                      |\e[0m'), nl,
    write('\e[33m|                      5) After finding line of 3 (white)                |\e[0m'), nl,
    write('\e[33m|                      6) Show final state (White wins)                  |\e[0m'), nl,
    write('\e[33m|                      7) Exit                                           |\e[0m'), nl,
    write('\e[33m|                                                                        |\e[0m'), nl,
    write('\e[33m|                      --ENTER OPTION--                                  |\e[0m'), nl,
    write('\e[33m|                                                                        |\e[0m'), nl,
    write('\e[33m+------------------------------------------------------------------------+\e[0m'), nl, nl,
    write('\e[37mEnter your option: \e[0m'),
    read(Choice),
    handle_menu_choice(Choice, Size).

% Handle menu option selection
handle_menu_choice(1, Size) :- start_game(human, human, Size).
handle_menu_choice(2, Size) :- select_difficulty(Difficulty), start_game(human, computer(Difficulty), Size).
handle_menu_choice(3, Size) :- select_difficulty(D1), select_difficulty(D2), start_game(computer(D1), computer(D2), Size).
handle_menu_choice(4, _) :- 
    intermediate_state_1(State, black),
    write('Showing intermediate state 1:'), nl,
    display_game(state(State, black)),
    write('Enter your move as (Row, Col): |: (1, 3).'), nl,
    write('Lines of three found: [[(3,1),(2,2),(1,3)]]'), nl,
    write('Enter position to stack (e.g., (Rs, Cs)) : (3, 1)'),nl,
    show_game_options(7).
handle_menu_choice(5, _) :- 
    near_final_state_1(State, white),
    write('Showing after finding line of 3 state:'), nl,
    display_game(state(State, white)),
    show_game_options(7).
handle_menu_choice(6, _) :- 
    final_state_white_victory(State, white),
    write('Showing final state (White wins):'), nl,
    display_game(state(State, white)),
    nl, write('Game over! Winner: white'), nl,
    show_game_options(7).
handle_menu_choice(7, _) :- nl, write('Exiting the game. See you later!'), nl.
handle_menu_choice(_, Size) :- nl, write('Invalid option, please try again.'), nl, show_game_options(Size).


% Difficulty selection
select_difficulty(Difficulty) :-
    nl,nl,
    write('\e[33m+------------------------------------------------------------------------+\e[0m'),nl,
    write('\e[33m|                                                                        |\e[0m'),nl,
    write('\e[33m|                         Select difficulty level:                       |\e[0m'),nl,
    write('\e[33m|                                                                        |\e[0m'),nl,
    write('\e[33m|                      1) Easy (Random moves)                            |\e[0m'),nl,
    write('\e[33m|                      2) Hard (Greedy moves)                            |\e[0m'),nl,
    write('\e[33m|                                                                        |\e[0m'),nl,
    write('\e[33m|                         --INSERT OPTION--                              |\e[0m'),nl,
    write('\e[33m|                                                                        |\e[0m'),nl,
    write('\e[33m+------------------------------------------------------------------------+\e[0m'),nl,nl,
    write('\e[37mEnter your choice: \e[0m'),
    read(Choice),
    validate_difficulty(Choice, Difficulty).

validate_difficulty(1, 1) :-
    nl, write('You selected Easy (Random moves).'), nl, !.

validate_difficulty(2, 2) :-
    nl, write('You selected Hard (Greedy moves).'), nl, !.

validate_difficulty(_, Difficulty) :-
    nl, write('Invalid choice. Please select 1 (Easy) or 2 (Hard).'), nl,
    select_difficulty(Difficulty).
    

