def read_input():
    # T test cases
    t = int(input())
    for i in range(1, t+1):
        # R C H V
        r, c, h, v = list(map(int, input().split()))
        w = [input() for _ in range(r)]
        yield (r, c, h, v, w)


def solve(P):
    (R, C, H, V, W) = P
    
    pieces = (H+1)*(V+1)
    # treat corner cases
    # if number of choc is not divisible by number of pieces
    n_choc = sum(c for r in W for c in r if c == '@')
    if not n_choc % pieces:
        return "IMPOSSIBLE"
    
    ch_per_p = n_choc // pieces
    # chocolates per row and per column
    ch_r = [sum(c for c in r if c == '@') for r in W]
    ch_c = [sum(r for r in c if r == '@') for c in zip(*W)]
    
    check_rows = False
    gen_count = 0
    for count in ch_r:
        gen_count += count
        if gen_count
        

    
    


def score(p):
    pass


def print_solutions(solutions):
    # output:
    # Case #X: min_number_of_hacks|impossible
    # ord_cases = sorted(solutions.keys())
    # for case in ord_cases:
    #     print("Case #{}: {}".format(case, solutions[case]))
    pass


if __name__ == '__main__':
    solutions = [solve(p) for p in read_input()]
    print_solutions(solutions)
