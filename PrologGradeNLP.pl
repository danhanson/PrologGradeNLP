%% The final project will be used to make prolog do some natural language
%% processing.

%% BEFORE YOU START, take a look at the book chapter "Prolog Grammar Rules"
%% on Moodle.

%% The basic ideal will be to build a system with knowledge of student
%% grades, and a processing system that respond to human queries about
%% grade results.  You can see what the knowledge looks like here:

grade(steve,boy,97).
grade(anne,girl,97).
grade(sally,girl,88).
grade(mike,boy,77).
grade(cathy,girl,81).

% Stage A: Writing a basic parse
%
% Stage A of this project will focus on the design of a single parse
% function.  This is the heart of our NLP engine, and we'll want to
% make it as extensible as possible.
%
% Parse will be used like this:
%
% ?- parse([what,is,the,highest,grade],Result).
% Result = 97
%
% Note that the input is a list of atoms, all lowercase.  The type of
% result will vary depending on the question asked, but will always
% be relatively simple.  Certian questions, however, can have more
% than one valid response (e.g. WHO has the highest grade will return
% two responses...but this can be handled in the usual prolog way).

% Stage A1 [5 points].  Make the example parse above work.

same(highest,largest).
same(largest,biggest).
same(biggest,best).
same(lowest,smallest).
same(smallest,worst).
same(worst,icky-est).
same(girl,girls).
same(boy,boys).
same(students,people).
same(students,young-uns).
transitive_same(X,Y,Z) :- (same(X,Y);same(Y,X)), \+ member(Y,Z).
transitive_same(X,Y,Z) :- (same(X,Q);same(Q,X)), \+ member(Q,Z), transitive_same(Q,Y,[Q|Z]).
synonym(X,Y) :- X = Y; transitive_same(X,Y,[X]).

is_word(Word) :-
	(
		Word = who;
		Word = has;
		Word = the;
		Word = grade;
		Word = for;
		Word = all
	),!.
is_word(X) :- (synonym(X,_); grade(X,_,_)),!.

parse(Query,Result) :-
  splitter(Query,Noun,Adj,Restrictions),
  grade(Person,Gender,Grade),
  maplist(satisfies(Person,Gender,Grade),Restrictions),
  forall(
    maplist(satisfies(_,_,OtherGrade),Restrictions),
    ( synonym(Adj,highest), Grade >= OtherGrade;
      synonym(Adj,lowest), Grade =< OtherGrade)
  ),
  ( synonym(Noun,who),  Result = Person;
    synonym(Noun,what), Result = Grade).

% all restrictions are two words, but 'for A students' is 3 words so we remove the 'for'
% for consistency
normalize_restrictions([],[]).
normalize_restrictions([for,X,students|T1],[X,students|T2]) :- normalize_restrictions(T1,T2), !.
normalize_restrictions([H|T1],[H|T2]) :- normalize_restrictions(T1,T2).

splitter([],Noun,Adj,[[]]) :- atom(Noun), atom(Adj). % see comment in satisfies/4 for double nested list
splitter([H|T],Noun,Adj,Restrictions) :- \+atom(Noun), is_subject(H), splitter(T,H,Adj,Restrictions), Noun = H, !.
splitter([H,Object|T],Noun,H,Restrictions) :- is_adj(H), synonym(Object,grade), splitter([Object|T],Noun,H,Restrictions), !.
splitter([Object,H1,H2|T],Noun,Adj,[[H1,H2]|Restrictions]) :- % first restriction clause
  synonym(Object,grade), is_restriction(H1,H2), splitter(T,Noun,Adj,Restrictions).
splitter([who,are,H1,H2|T],Noun,Adj,[[H1,H2]|Restrictions]) :- % all other restrictions clauses
  is_restriction(H1,H2), splitter(T,Noun,Adj,Restrictions), !.
splitter([_|T],Noun,Adj,Restrictions) :- splitter(T,Noun,Adj,Restrictions), !.

is_subject(X) :- member(X,[who,what]).
is_adj(X) :- synonym(X,lowest); synonym(X,highest).
is_gender(X) :- synonym(X,boys); synonym(X,girls).
is_restriction(X,Y) :- synonym(X,for), is_gender(Y).
is_restriction(X,Y) :- (synonym(X,below); synonym(X,above)), (grade(Y,_,_); number(Y)).
is_restriction(X,Y) :- synonym(Y,students), letter_grade(X,_,_).

letter_grade(a,90,101).
letter_grade(b,80,90).
letter_grade(c,70,80).
letter_grade(d,60,70).
letter_grade(f,0,60).


satisfies(Per,Gen,Gra,[]) :- grade(Per,Gen,Gra). % otherwise maplist doesn't bind the value
satisfies(Per,Gen,Gra,[For,Gender]) :- synonym(For,for), synonym(Gen,Gender), grade(Per,Gen,Gra).
satisfies(Per,Gen,Gra,[Prep,Clause]) :-
  grade(Per,Gen,Gra),
  ( 
    ( % 'for girls'
      synonym(Prep,for), synonym(Gen,Clause), grade(Per,Gen,_)
    );
    (
      ( number(Clause), Grade is Clause;  % 'above 87'
        grade(Clause,_,Grade)),           % 'below mike'
      ( synonym(Prep,above), Gra > Grade;
        synonym(Prep,below), Gra < Grade)
    );
    ( % 'b students'
      Clause = students, 
      letter_grade(Prep,LowerBound,UpperBound),
      Gra >= LowerBound, Gra < UpperBound
    )
  ).

% Stage A2 [5 points].  Modify the code so that it will also return
% the lowest grade.

% Stage A3 [5 points].  Modify the code above so you can use a variety
% of synomyns for highest/lowest (largest, biggest, best, or whatever).

% Stage A4 [10 points].  Modify the code so that you can ask
% [who,has,the,highest,grade] and simliar queries (should return a
% name).  You'll want to be careful to prevent too much duplication.

%
% Stage A5 [5 points].  Modify the code so that you can restrict the
% range of the search.  If I say [...,above,82] it should restrict the
% search to grades above 82.  If If I say [...,above,mike] and mike is
% a name in the db, it should restrict the grades to grades above that
% student's grade.
%
% Stage A6 [5 points].  Same as above, but now if I say [...,for,girls]
% it should restrict the search to girls.  If I say [...,for,a,students]
% it should restrict the grades for students who have a grade in the 90s.
%
% Stage A7 [5 points].  Add the ability to have unlimited restrictions
% with "who are".  So I can say
% [...,for,girls,who,are,b,students,who,are,above,85].
%
% That last one may be tricky - remember it's only worth 5 points if you
% skip it.
%
% Stage B [30 points]: Handling Input

% We would like to be able to access this function without using
% prolog's strange interface.  Instead, I'd like to be able to run a
% prolog function that puts prolog into an input mode where I can
% just type questions and it will answer.
%
% For example:
%% ?- do_nlp(start). % I couldn't get do_nlp to compile without a parameter
%% |: what is the highest grade?
%% 97
%% |: what is the lowest grade?
%% 77
%% |: done?
%% bye
%% true .
% Some restrictions:
%
% Questions will always be just a series of words seperated by spaces
% - no commas or other strangeness.
%
% Questions will always end with a ?
%
% Everything is always is lowercase
%
% done? ends the loop
%
% hint: note that once you finish parsing a question, a '\n' is likely still
% stored in input and will be returned as a character when you next call
% get_char.  Make sure your input ignores it or your 2nd parse may be messed
% up.
%
get_string(X) :- get_string_helper(Y), string_codes(X,Y).
get_string_helper(X) :- get_code(Y),(Y = 63,get_code(10),X = []; get_string_helper(Z), X = [Y|Z]), !.
get_words(Q) :- get_string(Y), atomic_list_concat(X,' ',Y), maplist(downcase_atom,X,Z), maplist(numerize,Z,Q).

numerize(X,Y) :- atom_number(X,Y), !.
numerize(X,X).

do_nlp(Start) :-
  get_words(Words),
  parse(Words,Result),
  write(Result),
  write('\n'),
  do_nlp(Start).

% Stage C [30 points]: Improved parsing
%
% It's up to you!  Expand the NLP parser to be able to answer a
% greater variety of questions.  You should at least include 2 new
% structurally different questions (e.g. count the students that, is
% it true that) that have some internal variations.  Beyond that, have
% a good time.  Feel free to improve output, do error handling - just
% make some signifigant improvements of yur own.
%
% Include in the comments a fairly complete description of the kinds
% of new questions you support and any other features you added.


