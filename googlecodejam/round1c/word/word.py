from itertools import product
from collections import defaultdict

def tree()

def read_input():
    # T test cases
    T = int(input())
    for i in range(1, T+1):
        N, L = tuple(map(int, input().split()))
        Ls = set()
        # tree = defaultdict(defaultdict)
        tree = dict()
        Slots = [set() for l in range(L)]
        for n in range(N):
            rawL = tuple(input())
            Li = rawL
            for li, l in enumerate(rawL):
                Slots[li].add(l)
                tree[l]
            Ls.add(Li)
        yield (N, Ls, Slots)


def solve(P):
    (N, Ls, Slots) = P
    
    wordsize = len(Slots)
    subtree = {w[-1]:{w[-1]} for w in Ls}
    for i in range(wordsize-1, 0, -1):
        subtree = defaultdict(set)
        tree = defaultdict(set)
        for w in Ls:
            tree[w[i-1]].add(subtree[w[i]])
        subtree = tree

    # case L = 1
    if len(Slots) == 1:
        return "-"

    # solving for n=2
    for i in range(wordsize):
        
    options = product(*Slots)
    for option in options:
        if option not in Ls:
            return "".join(option)
        
    return "-"
    
def navigate(subtree, level, max_level):
    if level == max_level:
        if subtree:
            return [subtree.pop()]
        else:
            return False
    for st of subtree:
        ret = navigate(st, level+1, max_level)
        if ret:
            return [subtree.pop()] + ret
        else:
            return False


def print_solutions(solutions):
    # output:
    # Case #X: min_number_of_hacks|impossible
    for i, s in enumerate(solutions):
        print("Case #{}: {}".format(i+1, s))


if __name__ == '__main__':
    solutions = [solve(p) for p in read_input()]
    print_solutions(solutions)
