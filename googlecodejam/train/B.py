from itertools import combinations, permutations
import operator, functools

def read_input():
    # T test cases
    T = int(input())
    for i in range(1, T+1):
        N, K = tuple(map(int, input().split()))
        Ps = list(map(float, input().split()))
        yield (N, K, Ps)


def solve(P):
    (N, K, Ps) = P

    max_prob = 0.0
    for comb in combinations(Ps, K):
        p = 0.0
        for truthvalues in set(permutations("T"*(K//2)+"F"*(K//2))):
            pi = 1.0
            for (tv, cp) in zip(truthvalues, comb):
                if tv == "T":
                    pi *= cp
                else:
                    pi *= (1-cp)
            
            p += pi
        if p > max_prob:
            max_prob = p
        
    return max_prob
        
    
    


def print_solutions(solutions):
    # output:
    # Case #X: min_number_of_hacks|impossible
    for i, s in enumerate(solutions):
        print("Case #{}: {}".format(i+1, s))


if __name__ == '__main__':
    # solutions = [solve(p) for p in read_input()]
    for (i, p) in enumerate(read_input()):
        print("Case #{}: {}".format(i+1, solve(p)))
    # print_solutions(solutions)
