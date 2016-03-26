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

%q1(a/b, _, _, _, _).
q1(b, b, _, _, _).
q1(c, _, b, _, _).
q1(d, _, _, b, _).
q1(e, _, _, _, b).


% 2. The only two consecutive questions with identical answers are questions
%    (A) 6 and 7
%    (B) 7 and 8
%    (C) 8 and 9
%    (D) 9 and 10
%    (E) 10 and 11
%q2(a, X, X, _, _, _, _).
q2(a, a, a, _, _, _, _).
q2(a, b, b, _, _, _, _).
q2(a, c, c, _, _, _, _).
q2(a, d, d, _, _, _, _).
q2(a, e, e, _, _, _, _).
%q2(b, _, X, X, _, _, _).
q2(b, _, a, a, _, _, _).
q2(b, _, b, b, _, _, _).
q2(b, _, c, c, _, _, _).
q2(b, _, d, d, _, _, _).
q2(b, _, e, e, _, _, _).
%q2(c, _, _, X, X, _, _).
q2(c, _, _, a, a, _, _).
q2(c, _, _, b, b, _, _).
q2(c, _, _, c, c, _, _).
q2(c, _, _, d, d, _, _).
q2(c, _, _, e, e, _, _).
%q2(d, _, _, _, X, X, _).
q2(d, _, _, _, a, a, _).
q2(d, _, _, _, b, b, _).
q2(d, _, _, _, c, c, _).
q2(d, _, _, _, d, d, _).
q2(d, _, _, _, e, e, _).
%q2(e, _, _, _, _, X, X).
q2(e, _, _, _, _, a, a).
q2(e, _, _, _, _, b, b).
q2(e, _, _, _, _, c, c).
q2(e, _, _, _, _, d, d).
q2(e, _, _, _, _, e, e).

% 3. The number of questions with the answer E is
%    (A) 0
%    (B) 1
%    (C) 2
%    (D) 3
%    (E) 4
q3(a, L) :- count(e, L, 0).
q3(b, L) :- count(e, L, 1).
q3(c, L) :- count(e, L, 2).
q3(d, L) :- count(e, L, 3).
q3(e, L) :- count(e, L, 4).

% 4. The number of questions with the answer A is
%    (A) 4
%    (B) 5
%    (C) 6
%    (D) 7
%    (E) 8
q4(a, L):- count(a, L, 4).
q4(b, L):- count(a, L, 5).
q4(c, L):- count(a, L, 6).
q4(d, L):- count(a, L, 7).
q4(e, L):- count(a, L, 8).

% 5. The answer to this question is the same as the answer to question
%    (A) 1
%    (B) 2
%    (C) 3
%    (D) 4
%    (E) 5
q5(a, a, _, _, _).
q5(b, _, b, _, _).
q5(c, _, _, c, _).
q5(d, _, _, _, d).
q5(e, _, _, _, _).

% 6. The answer to question 17 is
%    (A) C
%    (B) D
%    (C) E
%    (D) none of the above
%    (E) all of the above
q6(a, c).
q6(b, d).
q6(c, e).
q6(d, a).
q6(d, b).
%q6(e, ?). - impossible?

% 7. Alphabetically, the answer to this question and the answer to the
%    following question are
%    (A) 4 apart
%    (B) 3 apart
%    (C) 2 apart
%    (D) 1 apart
%    (E) the same
q7(a, e).
q7(b, e).
q7(c, a).
q7(c, e).
q7(d, c).
q7(d, e).
q7(e, e).

% 8. The number of questions whose answers are vowels is
%    (A) 4
%    (B) 5
%    (C) 6
%    (D) 7
%    (E) 8
q8(a, L) :- count(a, L, As), count(e, L, Es),  VWs is As+Es, VWs == 3.
q8(b, L) :- count(a, L, As), count(e, L, Es),  VWs is As+Es, VWs == 5.
q8(c, L) :- count(a, L, As), count(e, L, Es),  VWs is As+Es, VWs == 6.
q8(d, L) :- count(a, L, As), count(e, L, Es),  VWs is As+Es, VWs == 7.
q8(e, L) :- count(a, L, As), count(e, L, Es),  VWs is As+Es, VWs == 7.


% 9. The next question with the same answer as this one is question
%    (A) 10
%    (B) 11
%    (C) 12
%    (D) 13
%    (E) 14
q9(a, a, _, _, _, _).
q9(b, _, b, _, _, _).
q9(c, _, _, c, _, _).
q9(d, _, _, _, d, _).
q9(e, _, _, _, _, e).


%10. The answer to question 16 is
%    (A) D
%    (B) A
%    (C) E
%    (D) B
%    (E) C
q10(a, d).
q10(b, a).
q10(c, e).
q10(d, b).
q10(e, c).

%11. The number of questions preceding this one with the answer B is
%    (A) 0
%    (B) 1
%    (C) 2
%    (D) 3
%    (E) 4
q11(a, F10) :- count(b, F10, 0).
q11(b, F10) :- count(b, F10, 1).
q11(c, F10) :- count(b, F10, 2).
q11(d, F10) :- count(b, F10, 3).
q11(e, F10) :- count(b, F10, 4).


%12. The number of questions whose answer is a consonant is
%    (A) an even number
%    (B) an odd number
%    (C) a perfect square
%    (D) a prime
%    (E) divisible by 5
q12(a, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds) mod 2, CSs == 0.
q12(b, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds) mod 2, CSs == 1.
%perfect squares below 20: 1,4,9,16
q12(c, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds), CSs == 1.
q12(c, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds), CSs == 4.
q12(c, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds), CSs == 9.
q12(c, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds), CSs == 16.
%primes below 20: 1,2,3,5,7,11,13,17,19
q12(d, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds), CSs == 1.
q12(d, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds), CSs == 2.
q12(d, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds), CSs == 3.
q12(d, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds), CSs == 5.
q12(d, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds), CSs == 7.
q12(d, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds), CSs == 11.
q12(d, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds), CSs == 13.
q12(d, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds), CSs == 17.
q12(d, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds), CSs == 19.
q12(e, L) :- count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), CSs is (Bs+Cs+Ds) mod 5, CSs == 0.

%13. The only odd-numbered problem with answer A is
%    (A) 9
%    (B) 11
%    (C) 13
%    (D) 15
%    (E) 17
q13(a, a, _, _, _).
q13(b, _, a, _, _).
%q13(c/a, _, _, _, _). - contradiction!
q13(e, _, _, a, _).
q13(d, _, _, _, a).


%14. The number of questions with answer D is
%    (A) 6
%    (B) 7
%    (C) 8
%    (D) 9
%    (E) 10
%q14(Q14, L),
q14(a, L) :- count(d, L, 6).
q14(b, L) :- count(d, L, 7).
q14(c, L) :- count(d, L, 8).
q14(d, L) :- count(d, L, 9).
q14(e, L) :- count(d, L, 10).

%15. The answer to question 12 is
%    (A) A
%    (B) B
%    (C) C
%    (D) D
%    (E) E
q15(a, a).
q15(b, b).
q15(c, c).
q15(d, d).
q15(e, e).

%16. The answer to question 10 is
%    (A) D
%    (B) C
%    (C) B
%    (D) A
%    (E) E
q16(a, d).
q16(b, c).
q16(c, b).
q16(d, a).
q16(e, e).

%17. The answer to question 6 is
%    (A) C
%    (B) D
%    (C) E
%    (D) none of the above
%    (E) all of the above
q17(a, c).
q17(b, d).
q17(c, e).
q17(d, a).
q17(d, b).
%q17(e) :- q6(e). - impossible?

%18. The number of questions with answer A equals the number of questions
%    with answer
%    (A) B
%    (B) C
%    (C) D
%    (D) E
%    (E) none of the above
q18(a, L) :- count(a, L, As), count(b, L, Os), As == Os.
q18(b, L) :- count(a, L, As), count(c, L, Os), As == Os.
q18(c, L) :- count(a, L, As), count(d, L, Os), As == Os.
q18(d, L) :- count(a, L, As), count(e, L, Os), As == Os.
q18(e, L) :- count(a, L, As), count(b, L, Bs), count(c, L, Cs), count(d, L, Ds), count(e, L, Es),
As \== Bs, As \== Cs, As \== Ds, As \== Es.


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
% q20(a).
% q20(b).
% q20(c).
% q20(d).
q20(e).

%aux functions:
count(E, [E|T], Total) :-
    count(E, T, PTotal),
    Total is PTotal + 1.

count(E, [H|T], Total) :-
    dif(H, E), %%H \= E,
    count(E, T, Total).

count(_, [], 0).

all_questions([Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20]):-
  L = [Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20],
  F10 = [Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10],
  q1(Q1, Q2, Q3, Q4, Q5),
  q2(Q2, Q6, Q7, Q8, Q9, Q10, Q11),
  q3(Q3, L), %count
  q4(Q4, L), %count
  q5(Q5, Q1, Q2, Q3, Q4),
  q6(Q6, Q17),
  q7(Q7, Q8),
  q8(Q8, L), %3 counts
  q9(Q9, Q10, Q11, Q12, Q13, Q14),
  q10(Q10, Q16),
  q11(Q11, F10), %1/2 count
  q12(Q12, L), %3 counts!
  q13(Q13, Q9, Q11, Q15, Q17),
  q15(Q15, Q12),
  q16(Q16, Q10),
  q17(Q17, Q6),
  q18(Q18, L), %many counts (5)
  q19(Q19),
  q20(Q20).

all_questions_faster([Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20]):-
  L = [Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20],
  F10 = [Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10],
  q19(Q19),
  q20(Q20),
  q6(Q6, Q17),
  q7(Q7, Q8),
  q10(Q10, Q16),
  q17(Q17, Q6),
  q15(Q15, Q12),
  q16(Q16, Q10),
  q1(Q1, Q2, Q3, Q4, Q5),
  q5(Q5, Q1, Q2, Q3, Q4),
  q13(Q13, Q9, Q11, Q15, Q17),
  q2(Q2, Q6, Q7, Q8, Q9, Q10, Q11),
  q9(Q9, Q10, Q11, Q12, Q13, Q14),
  q11(Q11, F10), %1/2 count
  q3(Q3, L), %count
  q4(Q4, L), %count
  q8(Q8, L), %3 counts
  q12(Q12, L), %3 counts!
  q18(Q18, L). %muitos counts (5)
