
% some trivia about Prolog
trivia(prolog, 10, 'When was Prolog first developed?', 1972).
trivia(prolog, 10, 'What does Prolog stand for?', 'Programming in Logic').
trivia(prolog, 10, 'What is the name of the most popular Prolog implementation?', 'SWI-Prolog').
trivia(prolog, 20, 'Who is the creator of Prolog?', 'Alain Colmerauer').
trivia(prolog, 20, 'What is the name of the first Prolog interpreter?', 'Dec-10 Prolog').
trivia(prolog, 30, 'What is the name of the first Prolog compiler?', 'Warren Abstract Machine').
trivia(prolog, 30, 'When was the first version of SWI-Prolog released?', 1987).
trivia(prolog, 30, 'Who is the creator of SWI-Prolog?', 'Jan Wielemaker').

% some trivia about Haskell
trivia(haskell, 10, 'When was Haskell first developed?', 1990).
trivia(haskell, 10, 'Who was Haskell named after?', 'Haskell Curry').
trivia(haskell, 20, 'What is the full name of the most popular Haskell implementation?', 'Glasgow Haskell Compiler').
trivia(haskell, 20, 'Who is the creator of Haskell?', 'Simon Peyton Jones').
trivia(haskell, 30, 'Where was Haskell first developed?', 'University of Glasgow').
trivia(haskell, 30, 'What is the name of the first Haskell interpreter?', 'Hugs').

% some trivia about ubc
trivia(ubc, 10, 'When was UBC founded?', 1908).
trivia(ubc, 10, 'What is the name of the UBC mascot?', 'Thunderbird').
trivia(ubc, 10, 'What is the name of the UBC motto?', 'Tuum Est').
trivia(ubc, 20, 'What is the name of the UBC president?', 'Benoit-Antoine Bacon').
trivia(ubc, 20, 'What is the name of the UBC chancellor?', 'Steven Lewis Point').
trivia(ubc, 30, 'How many presidents has UBC had before Bacon?', 16).
trivia(ubc, 30, 'How many chancellors has UBC had before Point?', 18).

get_random_question(Category, Score, Question) :- 
    findall(Q-A, trivia(Category, Score, Q, A), Qs),
    random_member(Question, Qs).

assert_question(Category, Score, QuestionId) :-
    get_random_question(Category, Score, Q-A),
    assert(question(Category, Score, QuestionId, Q, A, unanswered)).

init_game :-
    %question(Category, Score, QuestionId, Question, Answer, Status)
    assert_question(prolog, 10, 1),
    assert_question(prolog, 20, 4),
    assert_question(prolog, 30, 7),

    assert_question(haskell, 10, 2),
    assert_question(haskell, 20, 5),
    assert_question(haskell, 30, 8),
    
    assert_question(ubc, 10, 3),
    assert_question(ubc, 20, 6),
    assert_question(ubc, 30, 9).

% Display all questions grouped by score and category with their status.
display_questions :-
    write('Score |  Prolog  | Haskell | UBC'), nl,
    write('----------------------------------'), nl,
    findall(Score-Category-QID-Status, question(Category, Score, QID, _, _, Status), Questions),
    sort(Questions, SortedQuestions),
    group_by_score(SortedQuestions, Grouped),
    maplist(display_question_group, Grouped).

% Recursive case for grouping: group questions by their score.
group_by_score([H|T], Grouped) :-
    H = Score-_-_-_,
    take_questions([Score-_-_-_]>>true, [H|T], CurrentGroup, Rest),
    group_by_score(Rest, Groups),
    Grouped = [CurrentGroup|Groups].
group_by_score([], []).

% If the head of the list matches the predicate, continue taking.
take_questions(P, [H|T], [H|Yes], No) :- call(P, H), !, take_questions(P, T, Yes, No).
take_questions(_, No, [], No).

% Base case for grouping: when the list is empty, result is also an empty list.
display_question_group(Questions) :-
    Questions = [Score-_-_-_|_],
    format('  ~w   |', [Score]),
    process_category_questions(prolog, Questions, PrologStr),
    process_category_questions(haskell, Questions, HaskellStr),
    process_category_questions(ubc, Questions, UBCStr),
    format('   ~w   |    ~w    |   ~w   ', [PrologStr, HaskellStr, UBCStr]), nl.

% Find all questions for a given category and prepare a string for display.
process_category_questions(Category, Questions, ResultStr) :-
    findall(Status-QID, member(_-Category-QID-Status, Questions), CatQuestions),
    maplist(question_status_to_string, CatQuestions, StrList),
    list_to_string(StrList, ResultStr).

% Convert question status and QID to a display string.
question_status_to_string(unanswered-QID, QIDStr) :- format(atom(QIDStr), '~w', [QID]).
question_status_to_string(answered-_, '*').

% Convert a list of strings to a single comma-separated string.
list_to_string(List, Str) :-
    atomic_list_concat(List, ', ', Str).


% To start the game.
start_game :-
    init_game,
    play_game(team1, 0, team2, 0).

% play game logic
play_game(Team1, Score1, Team2, Score2) :-
    display_questions,
    write(Team1), write(', select a question by entering its number: '),
    read(QuestionId),
    (   question(Category, Score, QuestionId, Question, Answer, unanswered) ->
        write(Question), nl,
        write('Your answer'),
        read(UserAnswer),
        retract(question(Category, Score, QuestionId, Question, Answer, unanswered)), % Remove the unanswered question
        assert(question(Category, Score, QuestionId, Question, Answer, answered)), % Add as answered
        (   UserAnswer = Answer -> NewScore1 is Score1 + Score,
            format('Correct! ~w scores ~w points.~n', [Team1, Score]),
            check_game_over(Score1, Score2),
            play_game(Team2, Score2, Team1, NewScore1)
        ;   format('Wrong answer! No points.~n'),
            check_game_over(Score1, Score2),
            play_game(Team2, Score2, Team1, Score1)
        )
    ;   format('Invalid input or question already answered.~n'),
        play_game(Team1, Score1, Team2, Score2)
    ).

% Check if there are any unanswered questions left, otherwise end the game
check_game_over(Score1, Score2) :-
    \+ question(_, _, _, _, _, unanswered),!,  
    game_over_actions(Score1, Score2).
check_game_over(_, _).

% Game over info
game_over_actions(Score1, Score2) :-
    write('Game over.'), nl,
    write('Team 1 scored '), write(Score1), nl,
    write('Team 2 scored '), write(Score2), nl,
    determine_winner(Score1, Score2),
    halt.

% Determine the winner of the game.
determine_winner(Score1, Score2) :-
    Score1 > Score2, write('Team 1 wins!'), nl;
    Score1 < Score2, write('Team 2 wins!'), nl;
    Score1 = Score2, write('It is a tie!'), nl.

% Play game
play :-
    retractall(question(_, _, _, _, _, _)), % Clear any previous game state
    start_game.
