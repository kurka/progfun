%SELF-REFERENTIAL APTITUDE TEST, by Jim Propp (jimpropp at gmaildotcom)
%
%The solution to the following puzzle is unique; in some cases the 
%knowledge that the solution is unique may actually give you a short-cut 
%to finding the answer to a particular question.  (Thanks to Andy Latto 
  %for bringing this subtlety to my attention.)
%
%I should mention that if you don't agree with me about the answer to #20, 
%you will get a different solution to the puzzle than the one I had in mind.  
%But I should also mention that if you don't agree with me about the answer 
%to #20, you are just plain wrong.  :-)
%
%You may now begin work.



% 1. The first question whose answer is B is question
%    (A) 1
%    (B) 2
%    (C) 3
%    (D) 4
%    (E) 5

%q1(a).
q1(b) :- q2(b).
q1(c) :- q3(b).
q1(d) :- q4(b).
q1(e) :- q5(b).

% 2. The only two consecutive questions with identical answers are questions
%    (A) 6 and 7
%    (B) 7 and 8
%    (C) 8 and 9
%    (D) 9 and 10
%    (E) 10 and 11

q2(a) :- q6(X), q7(X).
q2(b) :- q7(X), q8(X).
q2(c) :- q8(X), q9(X).
q2(d) :- q9(X), q10(X).
q2(e) :- q10(X), q11(X).


% 3. The number of questions with the answer E is
%    (A) 0
%    (B) 1
%    (C) 2
%    (D) 3
%    (E) 4

q3(a) :- all_but_q3(L), count(e, L, 0, 0).
q3(b) :- all_but_q3(L), count(e, L, 0, 1).
q3(c) :- all_but_q3(L), count(e, L, 0, 2).
q3(d) :- all_but_q3(L), count(e, L, 0, 3).
q3(e) :- all_but_q3(L), count(e, L, 0, 3). %count itself

% 4. The number of questions with the answer A is
%    (A) 4
%    (B) 5
%    (C) 6
%    (D) 7
%    (E) 8

q4(a) :- all_but_q4(L), count(a, L, 0, 3).
q4(b) :- all_but_q4(L), count(a, L, 0, 5).
q4(c) :- all_but_q4(L), count(a, L, 0, 6).
q4(d) :- all_but_q4(L), count(a, L, 0, 7).
q4(e) :- all_but_q4(L), count(a, L, 0, 8).

% 5. The answer to this question is the same as the answer to question
%    (A) 1
%    (B) 2
%    (C) 3
%    (D) 4
%    (E) 5

q5(a) :- q1(a).
q5(b) :- q2(b).
q5(c) :- q3(c).
q5(d) :- q4(d).
q5(e).

% 6. The answer to question 17 is 
%    (A) C
%    (B) D
%    (C) E
%    (D) none of the above
%    (E) all of the above

q6(a) :- q17(c).
q6(b) :- q17(d).
q6(c) :- q17(e).
q6(d) :- q17(a).
q6(d) :- q17(b).
%q6(e). - impossible?

% 7. Alphabetically, the answer to this question and the answer to the
%    following question are
%    (A) 4 apart
%    (B) 3 apart
%    (C) 2 apart
%    (D) 1 apart
%    (E) the same

q7(a) :- q8(e).
q7(b) :- q8(e).
q7(c) :- q8(a).
q7(c) :- q8(e).
q7(d) :- q8(c).
q7(d) :- q8(e).
q7(e) :- q8(e).

% 8. The number of questions whose answers are vowels is
%    (A) 4
%    (B) 5
%    (C) 6
%    (D) 7
%    (E) 8

q8(a) :- all_but_q8(L), count(a, L, 0, As), count(e, L, 0, Es),  VWs is As+Es, VWs == 3.
q8(b) :- all_but_q8(L), count(a, L, 0, As), count(e, L, 0, Es),  VWs is As+Es, VWs == 5.
q8(c) :- all_but_q8(L), count(a, L, 0, As), count(e, L, 0, Es),  VWs is As+Es, VWs == 6.
q8(d) :- all_but_q8(L), count(a, L, 0, As), count(e, L, 0, Es),  VWs is As+Es, VWs == 7.
q8(e) :- all_but_q8(L), count(a, L, 0, As), count(e, L, 0, Es),  VWs is As+Es, VWs == 7.


% 9. The next question with the same answer as this one is question
%    (A) 10
%    (B) 11
%    (C) 12
%    (D) 13
%    (E) 14

q9(a) :- q10(a).
q9(b) :- q11(b).
q9(c) :- q12(c).
q9(d) :- q13(d).
q9(e) :- q14(e).


%10. The answer to question 16 is
%    (A) D
%    (B) A
%    (C) E
%    (D) B
%    (E) C

q10(a) :- q16(d).
q10(b) :- q16(a).
q10(c) :- q16(e).
q10(d) :- q16(b).
q10(e) :- q16(c).

%11. The number of questions preceding this one with the answer B is
%    (A) 0
%    (B) 1
%    (C) 2
%    (D) 3
%    (E) 4

q11(a) :- all_before_q11(L), count(b, L, 0, 0).
q11(b) :- all_before_q11(L), count(b, L, 0, 1).
q11(c) :- all_before_q11(L), count(b, L, 0, 2).
q11(d) :- all_before_q11(L), count(b, L, 0, 3).
q11(e) :- all_before_q11(L), count(b, L, 0, 4).


%12. The number of questions whose answer is a consonant is
%    (A) an even number
%    (B) an odd number
%    (C) a perfect square
%    (D) a prime
%    (E) divisible by 5

q12(a) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds) mod 2, CSs == 1.
q12(b) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds) mod 2, CSs == 1.
%perfect squares below 20: 1,4,9,16
q12(c) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds), CSs == 0.
q12(c) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds), CSs == 3.
q12(c) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds), CSs == 8.
q12(c) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds), CSs == 15.
%primes below 20: 1,2,3,5,7,11,13,17,19
q12(d) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds), CSs == 0.
q12(d) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds), CSs == 1.
q12(d) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds), CSs == 2.
q12(d) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds), CSs == 4.
q12(d) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds), CSs == 6.
q12(d) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds), CSs == 10.
q12(d) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds), CSs == 12.
q12(d) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds), CSs == 16.
q12(d) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds), CSs == 18.
q12(e) :- all_but_q12(L), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), CSs is (Bs+Cs+Ds) mod 5, CSs == 0.


%13. The only odd-numbered problem with answer A is
%    (A) 9
%    (B) 11
%    (C) 13
%    (D) 15
%    (E) 17

q13(a) :- q9(a).
q13(b) :- q11(a).
%q13(c) :- q13(a). - contradiction!
q13(d) :- q15(a).
q13(e) :- q17(a).

%14. The number of questions with answer D is
%    (A) 6
%    (B) 7
%    (C) 8
%    (D) 9
%    (E) 10

q14(a) :- all_but_q14(L), count(d, L, 0, 6).
q14(b) :- all_but_q14(L), count(d, L, 0, 7).
q14(c) :- all_but_q14(L), count(d, L, 0, 8).
q14(d) :- all_but_q14(L), count(d, L, 0, 8). %count itself
q14(e) :- all_but_q14(L), count(d, L, 0, 10).

%15. The answer to question 12 is
%    (A) A
%    (B) B
%    (C) C
%    (D) D
%    (E) E

q15(a) :- q12(a).
q15(b) :- q12(b).
q15(c) :- q12(c).
q15(d) :- q12(d).
q15(e) :- q12(e).

%16. The answer to question 10 is
%    (A) D
%    (B) C
%    (C) B
%    (D) A
%    (E) E

q16(a) :- q10(d).
q16(b) :- q10(c).
q16(c) :- q10(b).
q16(d) :- q10(a).
q16(e) :- q10(e).

%17. The answer to question 6 is
%    (A) C
%    (B) D
%    (C) E
%    (D) none of the above
%    (E) all of the above

q17(a) :- q6(c).
q17(b) :- q6(d).
q17(c) :- q6(e).
q17(d) :- q6(a).
q17(d) :- q6(b).
%q17(e) :- q6(e). - impossible?

%18. The number of questions with answer A equals the number of questions 
%    with answer
%    (A) B
%    (B) C
%    (C) D
%    (D) E
%    (E) none of the above

q18(a) :- all_but_q18(L), count(a, L, 0, As), count(b, L, 0, Os), As == Os.
q18(b) :- all_but_q18(L), count(a, L, 0, As), count(c, L, 0, Os), As == Os.
q18(c) :- all_but_q18(L), count(a, L, 0, As), count(d, L, 0, Os), As == Os.
q18(d) :- all_but_q18(L), count(a, L, 0, As), count(e, L, 0, Os), As == Os.
q18(e) :- all_but_q18(L), count(a, L, 0, As), count(b, L, 0, Bs), count(c, L, 0, Cs), count(d, L, 0, Ds), count(e, L, 0, Es),
As \= Bs, As \= Cs, As \= Ds, As \= Es.


    %19. The answer to this question is:
    %    (A) A
    %    (B) B
    %    (C) C
    %    (D) D
    %    (E) E

    q19(a).
  q19(b).
q19(c).
    q19(d).
  q19(e).

%20. Standardized test is to intelligence as barometer is to
%    (A) temperature (only)
%    (B) wind-velocity (only)
%    (C) latitude (only)
%    (D) longitude (only)
%    (E) temperature, wind-velocity, latitude, and longitude

q20(e).

%aux functions:
count(E, [E|T], Acc, Total) :- 
  Acc2 is Acc + 1,
  count(E, T, Acc2, Total).

count(E, [H|T], Acc, Total) :- 
  H \= E,
  count(E, T, Acc, Total).

count(_, [], Acc, Acc).


all_but_q3([Q1,Q2,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20]):-
  q1(Q1),q2(Q2),q4(Q4),q5(Q5),q6(Q6),q7(Q7),q8(Q8),q9(Q9),q10(Q10),q11(Q11),q12(Q12),q13(Q13),q14(Q14),q15(Q15),q16(Q16),q17(Q17),q18(Q18),q19(Q19),q20(Q20).

all_but_q4([Q1,Q2,Q3,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20]):-
  q1(Q1),q2(Q2),q3(Q3),q5(Q5),q6(Q6),q7(Q7),q8(Q8),q9(Q9),q10(Q10),q11(Q11),q12(Q12),q13(Q13),q14(Q14),q15(Q15),q16(Q16),q17(Q17),q18(Q18),q19(Q19),q20(Q20).

all_but_q8([Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20]):-
  q1(Q1),q2(Q2),q3(Q3),q4(Q4),q5(Q5),q6(Q6),q7(Q7),q9(Q9),q10(Q10),q11(Q11),q12(Q12),q13(Q13),q14(Q14),q15(Q15),q16(Q16),q17(Q17),q18(Q18),q19(Q19),q20(Q20).

all_but_q12([Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20]):-
  q1(Q1),q2(Q2),q3(Q3),q4(Q4),q5(Q5),q6(Q6),q7(Q7),q8(Q8),q9(Q9),q10(Q10),q11(Q11),q13(Q13),q14(Q14),q15(Q15),q16(Q16),q17(Q17),q18(Q18),q19(Q19),q20(Q20).

all_but_q14([Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q15,Q16,Q17,Q18,Q19,Q20]):-
  q1(Q1),q2(Q2),q3(Q3),q4(Q4),q5(Q5),q6(Q6),q7(Q7),q8(Q8),q9(Q9),q10(Q10),q11(Q11),q12(Q12),q13(Q13),q15(Q15),q16(Q16),q17(Q17),q18(Q18),q19(Q19),q20(Q20).

all_but_q18([Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q19,Q20]):-
  q1(Q1),q2(Q2),q3(Q3),q4(Q4),q5(Q5),q6(Q6),q7(Q7),q8(Q8),q9(Q9),q10(Q10),q11(Q11),q12(Q12),q13(Q13),q14(Q14),q15(Q15),q16(Q16),q17(Q17),q19(Q19),q20(Q20).

all_before_q11([Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10]):-
  q1(Q1),q2(Q2),q3(Q3),q4(Q4),q5(Q5),q6(Q6),q7(Q7),q8(Q8),q9(Q9),q10(Q10).

all_questions([Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20]):-
  q1(Q1),q2(Q2),q3(Q3),q4(Q4),q5(Q5),q6(Q6),q7(Q7),q8(Q8),q9(Q9),q10(Q10),q11(Q11),q12(Q12),q13(Q13),q14(Q14),q15(Q15),q16(Q16),q17(Q17),q18(Q18),q19(Q19),q20(Q20).

