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
same(guys,boys).
same(guy,boys).
same(men,boys).
same(women,girls).
same(have,has).
same(are,is).
same(student,students).
same(students,people).
same(students,young-uns).
same(grade,grades).
same(over,above).
same(under,below).
same(who,which).
transitive_same(X,Y,Z) :- (same(X,Y);same(Y,X)), \+ member(Y,Z).
transitive_same(X,Y,Z) :- (same(X,Q);same(Q,X)), \+ member(Q,Z), transitive_same(Q,Y,[Q|Z]).
synonym(X,Y) :- X = Y; transitive_same(X,Y,[X]).

parse([count,the,number,of|T],Result) :-
  parse([how,many|T],Result).

parse([how,Many|T],Result) :-
  synonym(Many,many),
  (
    noun(T,Nouns,T2),
    Q = [who | T2],
    (
      synonym(Nouns,grades),Q2 = Q,!;
      append(Q,[who,are,Nouns],Q2)
    );
    Q2 = [who | T]
  ),
  findall(Result,parse(Q2,Result),List),
  list_to_set(List,Set),
  length(Set,Result),!.


parse(Query,Result) :-
  splitter(Query,Subject,_,Adj,Noun,Restrictions),
  (
    (
      synonym(Noun,grade),
      synonym(Adj,average),
      aggregate_all(bag(G),maplist(satisfies(_,_,G),Restrictions),AllG),
      average(AllG,Grade)
    );
    (
      synonym(Noun,grade),
      synonym(Adj,median),
      aggregate_all(bag(G),maplist(satisfies(_,_,G),Restrictions),AllG),
      median(AllG,Grade)
    );
    (
      synonym(Noun,grade),
      grade(Person,Gender,Grade),
      maplist(satisfies(Person,Gender,Grade),Restrictions),
      forall(
        maplist(satisfies(_,_,OtherGrade),Restrictions),
        (
          synonym(Adj,highest), Grade >= OtherGrade;
          synonym(Adj,lowest), Grade =< OtherGrade
        )
      )
    );
    (
      is_gender(Noun),
      grade(Person,Gender,Grade),
      maplist(satisfies(Person,Gender,Grade),[[Noun]|Restrictions])
    )
  ),
  ( synonym(Subject,who),  Result = Person;
    synonym(Subject,what), Result = Grade).

splitter(List,Subject,Verb,Adj,Noun,Restrictions) :-
	subject(List,Subject,T),
	verb(T,Verb,T2),
	article(T2,_,T3),
	adjective(T3,Adj,T4),
	noun(T4,Noun,T5),
	restrictions(T5,Restrictions).

splitter(List,Subject,Verb,_,Noun,Restrictions) :-
	subject(List,Subject,T),
	verb(T,Verb,T2),
	article(T2,_,T3),
	noun(T3,Noun,T4),
	restrictions(T4,Restrictions).

splitter(List,Subject,Verb,_,Noun,Restrictions) :-
  subject(List,Subject,T),
  verb(T,Verb,T2),
  Noun = grade,
  restrictions(T2,Restrictions).

splitter(List,Subject,Verb,_,Noun,Restrictions) :-
  subject(List,Subject,T),
  verb(T,Verb,T2),
  noun(T2,Noun,T3),
  restrictions(T3,Restrictions).

subject([Who|T],who,T) :- synonym(who,Who).

subject([What,Grades|T],what,T) :- synonym(What,what), synonym(Grades,grades).

subject([What,Student|T],who,T) :- synonym(What,what), synonym(Student,student).

subject([Which,Student|T],who,T) :- synonym(Which,which), synonym(Student,student).

subject([What|T],what,T) :- synonym(What,what).

verb([Is|T],is,T) :- synonym(Is,is).

verb([Has|T],has,T) :- synonym(Has,has).

article([the|T],the,T).

article([a|T],a,T).

adjective([Highest|T],highest,T) :- synonym(Highest,highest).

adjective([Lowest|T],lowest,T) :- synonym(Lowest,lowest).

adjective([Average|T],average,T) :- synonym(Average,average).

adjective([Median|T],median,T) :- synonym(Median,median).

noun([Grade|T],grade,T) :- synonym(Grade,grade).

noun([Gender|T],Gender,T) :- is_gender(Gender).

noun([Student|T],student,T) :- synonym(Student,student).

noun(Grade) :- synonym(Grade,grade).

noun(Gender) :- is_gender(Gender).

noun(Student) :- synonym(Student,student).

restrictions([],[[]]).

restrictions(T,[Restriction|Restrictions]) :-
	is_restriction(T,Restriction,T2),restrictions(T2,Restrictions).

restrictions([for|T],[Restriction|Restrictions]) :-
	is_restriction(T,Restriction,T2),restrictions(T2,Restrictions).

restrictions([and|T],[Restriction|Restrictions]) :-
	is_restriction(T,Restriction,T2),restrictions(T2,Restrictions).

restrictions([with|T],[Restriction|Restrictions]) :-
	is_restriction(T,Restriction,T2),restrictions(T2,Restrictions).

restrictions([Who,Are|T],[Restriction|Restrictions]) :-
  synonym(Are,are),
  (synonym(Who,who);synonym(Who,and)),
	is_restriction(T,Restriction,T2),restrictions(T2,Restrictions).

is_gender(X) :- synonym(X,boys); synonym(X,girls).

is_restriction([Gender|Tail],[Gender],Tail) :- is_gender(Gender).
is_restriction([Gender|Tail],[Gender],Tail) :- is_gender(Gender).
is_restriction([Students|Tail],[],Tail) :- synonym(Students,students).
is_restriction([Letter,Students|Tail],[Letter,students],Tail) :- synonym(Students,students),letter_grade(Letter,_,_).
is_restriction([Above,Grade|Tail],[above,Grade],Tail) :- synonym(Above,above), number(Grade).
is_restriction([Below,Grade|Tail],[below,Grade],Tail) :- synonym(Below,below), number(Grade).
is_restriction([Above,Student|Tail],[above,Grade],Tail) :- synonym(Above,above), grade(Student,_,Grade).
is_restriction([Below,Student|Tail],[below,Grade],Tail) :- synonym(Below,below), grade(Student,_,Grade).

letter_grade(a,90,101).
letter_grade(b,80,90).
letter_grade(c,70,80).
letter_grade(d,60,70).
letter_grade(f,0,60).

satisfies(Per,Gen,Gra,[]) :- grade(Per,Gen,Gra). % otherwise maplist doesn't bind the value

satisfies(Per,Gen,Gra,[Gender|Tail]) :-
  synonym(Gen,Gender),
  satisfies(Per,Gen,Gra,Tail).

satisfies(Per,Gen,Gra,[Above,Number|Tail]) :-
  synonym(Above,above),
  number(Number),
  satisfies(Per,Gen,Gra,Tail),
  Gra > Number.

satisfies(Per,Gen,Gra,[Below,Number|Tail]) :-
  synonym(Below,below),
  number(Number),
  satisfies(Per,Gen,Gra,Tail),
  Gra < Number.

satisfies(Per,Gen,Gra,[Letter,Students|Tail]) :-
  synonym(Students,students),
  letter_grade(Letter,Bot,Top),
  satisfies(Per,Gen,Gra,Tail),
  Gra >= Bot, Gra < Top.

average(L,X) :- length(L,S), sumlist(L,T), S \= 0, X is T / S.
median(A,X) :- sort(A,B), length(B,S),
  ( S mod 2 =:= 1,
    I is (S + 1) / 2,
    nth1(I,B,X);
    S mod 2 =:= 0,
    I is S / 2,
    J is I + 1,
    nth1(I,B,Y),
    nth1(J,B,Z),
    X is (Y + Z) / 2).
stddev(L,X) :-
  length(L,Size),
  sumlist(L,Sum1),
  maplist(sqr,L,Squares),
  sumlist(Squares,Sum2),
  V is (Sum2 - (Sum1*Sum1/Size)) / (Size - 1),
  sqrt(V,X).
sqr(X,Y) :- Y is X*X.

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
get_string_helper(X) :- get_code(Y),(Y = 63,get_code(10),X = []; Y = 10,get_string_helper(X); get_string_helper(Z), X = [Y|Z]), !.
get_words(Q) :- get_string(Y), atomic_list_concat(X,' ',Y), maplist(downcase_atom,X,Z), maplist(numerize,Z,Q).

numerize(X,Y) :- atom_number(X,Y), !.
numerize(X,X).

writeln(X) :- write(X),write('\n').

do_nlp(Start) :-
  get_words(Words),
  (
    member(done,Words),!;
    (
      findall(Result,parse(Words,Result),Results),
      list_to_set(Results,UniqueResults),
      (
        length(Results,0),writeln(none);
        maplist(writeln,UniqueResults)
      ),
      do_nlp(Start)
    )
  ),!.

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

% FEATURES WE IMPLEMENTED (shown by example)
%
% |: What is the average grade?
% 88
% 
% |: Who are the girls?
% anne
% sally
% cathy
%
% |: What is the median grade for boys?
% 87
%
% |: What is the median grade for girls?
% 88
%
