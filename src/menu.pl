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

% Modified show_game_options to include board size
show_game_options(Size) :-
    nl,nl,
    write('\e[33m+------------------------------------------------------------------------+\e[0m'),nl,
    write('\e[33m|                                                                        |\e[0m'),nl,
    write('\e[33m|                      1) Human vs Human                                 |\e[0m'),nl,
    write('\e[33m|                      2) Human vs Computer                              |\e[0m'),nl,
    write('\e[33m|                      3) Computer vs Computer                           |\e[0m'),nl,
    write('\e[33m|                      4) Exit                                           |\e[0m'),nl,
    write('\e[33m|                                                                        |\e[0m'),nl,
    write('\e[33m|                      --INSERT OPTION--                                 |\e[0m'),nl,
    write('\e[33m|                                                                        |\e[0m'),nl,
    write('\e[33m+------------------------------------------------------------------------+\e[0m'),nl,nl,
    write('\e[37mEnter your choice: \e[0m'),
    read(Choice),
    handle_menu_choice(Choice, Size).

% Modified handle_menu_choice to include board size
handle_menu_choice(1, Size) :- start_game(human, human, Size).
handle_menu_choice(2, Size) :- select_difficulty(Difficulty), start_game(human, computer(Difficulty), Size).
handle_menu_choice(3, Size) :- select_difficulty(D1), select_difficulty(D2), start_game(computer(D1), computer(D2), Size).
handle_menu_choice(4, _) :- nl, write('Exiting game. Goodbye!'), nl.
handle_menu_choice(_, Size) :- nl, write('Invalid choice, try again.'), nl, show_game_options(Size).


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

