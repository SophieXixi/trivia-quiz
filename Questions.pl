
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
  