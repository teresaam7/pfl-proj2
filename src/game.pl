:- use_module(library(lists)).
:- use_module(library(random)).


:-ensure_loaded(menu).
:-ensure_loaded(state).
:-ensure_loaded(interface).

play :-
    main_menu.