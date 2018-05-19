from itertools import product
# import ipdb


def read_input():
    # T test cases
    T = int(input())
    for i in range(1, T+1):
        N, R, P, S = tuple(map(int, input().split()))
        yield (N, R, P, S)


def solve(P):
    (N, R, P, S) = P
    for poss in "PRS":
        game = succ(poss, N)

        SP = 0
        SR = 0
        SS = 0
        for c in game:
            if c == "P":
                SP += 1
            if c == "R":
                SR += 1
            if c == "S":
                SS += 1
        if SP==P and SR==R and SS==S:
            return game
    
    return "IMPOSSIBLE"
        # check if game is valid


def succ(X, N):
    if N==0:
        return X

    if X == "P":
        succ1 = succ("P", N-1)
        succ2 = succ("R", N-1)
    if X == "R":
        succ1 = succ("R", N-1)
        succ2 = succ("S", N-1)
    if X == "S":
        succ1 = succ("P", N-1)
        succ2 = succ("S", N-1)

    if succ1 < succ2:
        return succ1 + succ2
    else:
        return succ2 + succ1



def print_solutions(solutions):
    # output:
    # Case #X: min_number_of_hacks|impossible
    for i, s in enumerate(solutions):
        print("Case #{}: {}".format(i+1, s))


if __name__ == '__main__':
    solutions = [solve(p) for p in read_input()]
    print_solutions(solutions)
