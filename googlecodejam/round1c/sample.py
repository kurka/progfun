from collections import defaultdict

def read_input():
    # T test cases
    T = int(input())
    for i in range(1, T+1):
        N, L = tuple(map, int, input().split())
        Ls = {}
        Slots = [set() for l in range(L)]
        for n in range(1, N+1):
            Li = frozenset(map, int, input().split())
            for l in Li:
                Slots[n].add(l)
            Ls.add(Li)
        yield (N, Ls, Slots)


def solve(P):
    (N, Ls, Slots) = P
    
    # solving for n=2
    options = [(a,b) for a in Slots[0] for b in Slots[1]]
    for option in options:
        if option in Ls:
            return "".join(option)
        
    return "-"
    
    


def print_solutions(solutions):
    # output:
    # Case #X: min_number_of_hacks|impossible
    for i, s in enumerate(solutions):
        print("Case #{}: {}".format(i+1, s))


if __name__ == '__main__':
    solutions = [solve(p) for p in read_input()]
    print_solutions(solutions)
