:- dynamic team1/1.
:- dynamic team2/1.

init_game :-
    %question(Category, Score, QuestionId, Question, Answer, Status)
    assert(question(math, 10, 1, '1 + 1 = ?', 2, unanswered)),
    assert(question(math, 20, 4, '1 + 2 = ?', 3, unanswered)),
    assert(question(math, 30, 7, '1 + 3 = ?', 4, unanswered)),

    assert(question(english, 10, 2, '1 + 1 = ?', 2, unanswered)),
    assert(question(english, 20, 5, '2 + 1 = ?', 3, unanswered)),
    assert(question(english, 30, 8, '3 + 1 = ?', 4, unanswered)),

    assert(question(physics, 10, 3, '10 + 10 = ?', 20, unanswered)),
    assert(question(physics, 20, 6, '10 + 20 = ?', 30, unanswered)),
    assert(question(physics, 30, 9, '10 + 30 = ?', 40, unanswered)).

% Display all questions grouped by score and category with their status.
display_questions :-
    write('Score |  Math  | English | Physics'), nl,
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
    process_category_questions(math, Questions, MathStr),
    process_category_questions(english, Questions, EngStr),
    process_category_questions(physics, Questions, PhysStr),
    format('   ~w   |    ~w    |   ~w   ', [MathStr, EngStr, PhysStr]), nl.

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
    format('rules:~n'),
    format('1. enter the number in the cell to select the question.~n'),
    format('2. if you do not know the answer, just random guess.~n'),
    format('3. if you want to quit or giveup, just type \'giveup\' when answering.~n'),
    format('4. during the answering part, type \'giveup\' will give you the first letter as a hint, and the score will be half.'),
    teams_build_up(),
    team1(A),
    team2(B),
    play_game(A, 0, B, 0).

% teams build up, setting the team name for each team
teams_build_up() :-
    write('Please give a team name for the first team: '), nl,
    write('team 1 name: '),
    read_line_to_string(user_input, String1),
    atom_string(A, String1),
    assertz(team1(A)),
    format("name: ~w~n", [A]),
    write('Please give a team name for the second team: '), nl,
    write('team 2 name: '),
    read_line_to_string(user_input, String2),
    atom_string(B, String2),
    assertz(team2(B)),
    format("name: ~w~n", [B]).

% play game logic
play_game(Team1, Score1, Team2, Score2) :-
    display_questions,
    write(Team1), write(', select a question by entering its number: '),
    read(QuestionId),
    (   question(Category, Score, QuestionId, Question, Answer, unanswered) ->
        format("Category: ~w, score: ~w, q: ~w, a: ~w, ~n", [Category, Score, Question, Answer]),
        write(Question), nl,
        write('Your answer'),
        read(UserAnswer),
        retract(question(Category, Score, QuestionId, Question, Answer, unanswered)), % Remove the unanswered question
        assert(question(Category, Score, QuestionId, Question, Answer, answered)), % Add as answered
        (   UserAnswer = Answer -> NewScore1 is Score1 + Score,
            format('Correct! ~w scores ~w points.~n', [Team1, Score]),
            format('total score: ~w: ~w points; ~w: ~w points.~n', [Team1, NewScore1, Team2, Score2]), nl,
            check_game_over(Team1, Score1, Team2, Score2),
            play_game(Team2, Score2, Team1, NewScore1)
        ;   UserAnswer = 'giveup' -> 
            format('are you sure you want to quit the game?~n 1 = yes, 2 = some hint please, 3 = no'),
            read(Confirmation),
            (Confirmation = 1 -> format('~w giveup.~n', [Team1]),
                game_over_actions(Team1, Score1, Team2, Score2)
            ;   Confirmation = 2 -> sub_atom(Answer, 0, 1, _, FirstLetter),
                format('Here is a hint:~nthe first letter of the question \'~w\' is : ~w~n', [Question, FirstLetter]),
                write('please answer:(include the first letter)'), nl,
                read(UserAns),
                (   UserAns = Answer -> Half_score is Score / 2,
                    NewScore1 is Score1 + Half_score,
                    format('Correct! ~w scores ~w points.~n', [Team1, Half_score]),
                    format('total score: ~w: ~w points; ~w: ~w points.~n', [Team1, NewScore1, Team2, Score2]), nl,
                    check_game_over(Team1, Score1, Team2, Score2),
                    play_game(Team2, Score2, Team1, NewScore1)
                ;   format('Wrong answer! No points.~n'),
                    format('total score: ~w: ~w points; ~w: ~w points.~n', [Team1, Score1, Team2, Score2]), nl,
                    check_game_over(Team1, Score1, Team2, Score2),
                    play_game(Team2, Score2, Team1, Score1)
                )
            ;   format('Let\'s try again!~n'),
                write(Question),
                read(UserAn),
                (   UserAn = Answer -> NewScore1 is Score1 + Score,
                    format('Correct! ~w scores ~w points.~n', [Team1, Score]),
                    format('total score: ~w: ~w points; ~w: ~w points.~n', [Team1, NewScore1, Team2, Score2]), nl,
                    check_game_over(Team1, Score1, Team2, Score2),
                    play_game(Team2, Score2, Team1, NewScore1)
                ;   format('Wrong answer! No points.~n'),
                    format('total score: ~w: ~w points; ~w: ~w points.~n', [Team1, Score1, Team2, Score2]), nl,
                    check_game_over(Team1, Score1, Team2, Score2),
                    play_game(Team2, Score2, Team1, Score1)
                )
            )
        ;   format('Wrong answer! No points.~n'),
            format('total score: ~w: ~w points; ~w: ~w points.~n', [Team1, Score1, Team2, Score2]), nl,
            check_game_over(Team1, Score1, Team2, Score2),
            play_game(Team2, Score2, Team1, Score1)
        )
    ;   format('Invalid input or question already answered.~n'),
        play_game(Team1, Score1, Team2, Score2)
    ).

give_up_actions(Score, Question, Answer, Team1, Score1, Team2, Score2) :-
    format('entering'),
    write('~w, are you sure you want to quit the game? 1 = yes, 2 = some hint please, 3 = no'),
    read(Confirmation),
    (Confirmation = 1 -> format('~w giveup.~n', [Team1]),
        game_over_actions(Score1, Score2)
    ;   Confirmation = 2 -> format('Here is a hint:'),
        sub_atom(Answer, 0, 1, _, FirstLetter),
        write('the first letter of the question \'~w\' is : ~w~n', [Question, FirstLetter]),
        write('please answer:(include the first letter) '),
        read(UserAnswer),
        (   UserAnswer = Answer -> NewScore1 is Score1 + Score / 2,
            format('Correct! ~w scores ~w points.~n', [Team1, Score]),
            format('total score: ~w: ~w points; ~w: ~w points.~n', [Team1, NewScore1, Team2, Score2]), nl,
            check_game_over(Score1, Score2),
            play_game(Team2, Score2, Team1, NewScore1)
        ;   format('Wrong answer! No points.~n'),
            format('total score: ~w: ~w points; ~w: ~w points.~n', [Team1, Score1, Team2, Score2]), nl,
            check_game_over(Score1, Score2),
            play_game(Team2, Score2, Team1, Score1)
        )
    ;   format('Let\'s try again!'),
        write('~w~n', [Question]),
        read(UserAnswer),
        (   UserAnswer = Answer -> NewScore1 is Score1 + Score,
            format('Correct! ~w scores ~w points.~n', [Team1, Score]),
            format('total score: ~w: ~w points; ~w: ~w points.~n', [Team1, NewScore1, Team2, Score2]), nl,
            check_game_over(Score1, Score2),
            play_game(Team2, Score2, Team1, NewScore1)
        ;   format('Wrong answer! No points.~n'),
            format('total score: ~w: ~w points; ~w: ~w points.~n', [Team1, Score1, Team2, Score2]), nl,
            check_game_over(Score1, Score2),
            play_game(Team2, Score2, Team1, Score1)
        )
    )

% Check if there are any unanswered questions left, otherwise end the game
check_game_over(Team1, Score1, Team2, Score2) :-
    \+ question(_, _, _, _, _, unanswered),!,  
    game_over_actions(Team1, Score1, Team2, Score2).
check_game_over(_, _, _, _).

% Game over info
game_over_actions(Team1, Score1, Team2, Score2) :-
    write('Game over.'), nl,
    format('~w scored ~w. ~n', [Team1, Score1]),
    format('~w scored ~w. ~n', [Team2, Score2]),
    determine_winner(Team1, Score1, Team2, Score2),
    halt.

% Determine the winner of the game.
determine_winner(Team1, Score1, Team2, Score2) :-
    Score1 > Score2, write(Team1), write(' wins');
    Score1 < Score2, write(Team2), write(' wins');
    Score1 = Score2, write('It is a tie!'), nl.

% Play game
play :-
    retractall(question(_, _, _, _, _, _)), % Clear any previous game state
    start_game.
