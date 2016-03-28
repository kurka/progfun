%Version 1: https://en.wikipedia.org/wiki/Zebra_Puzzle

%01 - There are five houses.
%02 - The Englishman lives in the red house.
%03 - The Spaniard owns the dog.
%04 - Coffee is drunk in the green house.
%05 - The Ukrainian drinks tea.
%06 - The green house is immediately to the right of the ivory house.
%07 - The Old Gold smoker owns snails.
%08 - Kools are smoked in the yellow house.
%09 - Milk is drunk in the middle house.
%10 - The Norwegian lives in the first house.
%11 - The man who smokes Chesterfields lives in the house next to the man with the fox.
%12 - Kools are smoked in the house next to the house where the horse is kept.
%13 - The Lucky Strike smoker drinks orange juice.
%14 - The Japanese smokes Parliaments.
%15 - The Norwegian lives next to the blue house.
%The question is, who owns the zebra?

%5 houses
%Nationalities: englishman, spaniard, ukrainian, norwegian, japanese
%Pets: dog, snails, fox, horse, Zebra
%Colours: red, green, ivory, yellow, blue
%Drinks: coffee, tea, milk, orange_juice, water
%Cigarrete: old_gold, kools, chesterfields, lucky_strike, parliaments
%left, right, first..



%01 - There are five houses.
all_houses(H1, H2, H3, H4, H5) :-
  H1 = [H11, H12, H13, H14, H15],
  H2 = [H21, H22, H23, H24, H25],
  H3 = [H31, H32, H33, H34, H35],
  H4 = [H41, H42, H43, H44, H45],
  H5 = [H51, H52, H53, H54, H55],
  nat(H11), nat(H21), nat(H31), nat(H41), nat(H51),
  pet(H12), pet(H22), pet(H32), pet(H42), pet(H52),
  col(H13), col(H23), col(H33), col(H43), col(H53),
  dri(H14), dri(H24), dri(H34), dri(H44), dri(H54),
  cig(H15), cig(H25), cig(H35), cig(H45), cig(H55),

  house(H1), house(H2), house(H3), house(H4), house(H5),
  H1 \= H2,
  H1 \= H3,
  H1 \= H4,
  H1 \= H5,
  H2 \= H3,
  H2 \= H4,
  H2 \= H5,
  H3 \= H4,
  H3 \= H5,
  H4 \= H5.

%H = (Nat, Pet, Col, Dri, Cig)
%Nationalities: englishman, spaniard, ukrainian, norwegian, japanese
%Pets: dog, snails, fox, horse, Zebra
%Colours: red, green, ivory, yellow, blue
%Drinks: coffee, tea, milk, orange_juice, water
%Cigarrete: old_gold, kools, chesterfields, lucky_strike, parliaments
%left, right, first..
nat(englishman).
nat(spaniard).
nat(ukranian).
nat(norwegian).
nat(japanese).

pet(dog).
pet(snails).
pet(fox).
pet(horse).
pet(zebra).

col(red).
col(green).
col(ivory).
col(yellow).
col(blue).

dri(coffee).
dri(tea).
dri(milk).
dri(orange_juice).
dri(water).

cig(old_good).
cig(kools).
cig(chesterfields).
cig(lucky_strike).
cig(parliaments).

%H = (Nat, Pet, Col, Dri, Cig)

%02 - The Englishman lives in the red house.
house([englsh, _, red, _, _]).
%03 - The Spaniard owns the dog.
house([spaniard, dog, _, _, _]).
%04 - Coffee is drunk in the green house.
house([_, _ , green, coffee, _]).
%05 - The Ukrainian drinks tea.
house([ukranian, _, _, tea, _]).
%06 - The green house is immediately to the right of the ivory house.
%TODO
% house([_, _, green, _, _]).
% house([_, _, ivory, _, _]).
%07 - The Old Gold smoker owns snails.
house([_, snails, _, _, old_gold]).
%08 - Kools are smoked in the yellow house.
house([_, _, yellow, _, kools]).
%09 - Milk is drunk in the middle house.
%house([_, _, _, milk, _]).

%TODO
%10 - The Norwegian lives in the first house.
%TODO
%11 - The man who smokes Chesterfields lives in the house next to the man with the fox.
%12 - Kools are smoked in the house next to the house where the horse is kept.
%13 - The Lucky Strike smoker drinks orange juice.
%14 - The Japanese smokes Parliaments.
%15 - The Norwegian lives next to the blue house.
% house([_, _, _, water, _]).
% house([_, zebra, _, _, _]).

positions(Houses, P1, P2, P3, P4, P5) :-
    house_where_drink(Houses, milk, P3).

house_where_drink([H|HH], D, H) :-
  H = [_, _, _, D, _].

house_where_drink([[_,_,_,ND,_]|HH], D, Address) :-
  ND \= D,
  house_where_drink(HH, D, Adress).



check_position(_,_,[_, _, milk, _],_,_).









%Version 2: http://rosettacode.org/wiki/Zebra_puzzle

%01 - There are five houses.
%02 - The English man lives in the red house.
%03 - The Swede has a dog.
%04 - The Dane drinks tea.
%05 - The green house is immediately to the left of the white house.
%06 - They drink coffee in the green house.
%07 - The man who smokes Pall Mall has birds.
%08 - In the yellow house they smoke Dunhill.
%09 - In the middle house they drink milk.
%10 - The Norwegian lives in the first house.
%11 - The man who smokes Blend lives in the house next to the house with cats.
%12 - In a house next to the house where they have a horse, they smoke Dunhill.
%13 - The man who smokes Blue Master drinks beer.
%14 - The German smokes Prince.
%15 - The Norwegian lives next to the blue house.
%16 - They drink water in a house next to the house where they smoke Blend.

% Who owns the zebra?
% Additionally, list the solution for all the houses. Optionally, show the solution is unique.

%Structure:
%5 houses
%Nationalities: english, swede, dane, norwegian, german
%Pets: dog, bird, cat, horse, zebra
%Colours: red, green, white, yellow, blue
%Drinks: tea, coffee, milk, beer, water
%Cigarrete: Pall Mall, Dunhill, Blend, Blue master, Prince
%left, right, first...
