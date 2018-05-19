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
    n_choc = sum(1 for r in W for c in r if c == '@')
    
    if n_choc == 0:
        return "POSSIBLE"
    
    if n_choc % pieces != 0:
        return "IMPOSSIBLE"

    ch_per_p = n_choc // pieces
    # chocolates per row and per column
    ch_r = [sum(1 for c in r if c == '@') for r in W]

    gen_count = 0
    target = n_choc // (H+1)
    it = 0
    hcuts = [-1]
    for r, count in enumerate(ch_r):
        gen_count += count
        if gen_count > target:
            return "IMPOSSIBLE"
        if gen_count == target:
            gen_count = 0
            it += 1
            hcuts.append(r)

    # right number of iterations, gen_count is rounded to 0
    if not (it == (H+1) and gen_count == 0):
        return ("IMPOSSIBLE", (it, gen_count, target, n_choc, H, V))

    ch_c = [sum(1 for r in c if r == '@') for c in zip(*W)]
    gen_count = 0
    target = n_choc // (V+1)
    it = 0
    vcuts = [-1]
    for c, count in enumerate(ch_c):
        gen_count += count
        if gen_count > target:
            return "IMPOSSIBLE"
        if gen_count == target:
            gen_count = 0
            it += 1
            vcuts.append(c)

    # right number of iterations, gen_count is rounded to 0
    if not (it == (V+1) and gen_count == 0):
        return "IMPOSSIBLE"

    # check if quadrants are coherent
    print("NEW")
    print(hcuts, vcuts)
    for h in range(len(hcuts)-1):
        for v in range(len(vcuts)-1):
            quadcount = sum(1
                            for r in range(hcuts[h]+1, hcuts[h+1]+1)
                            for c in range(vcuts[v]+1, vcuts[v+1]+1)
                            if W[r][c] == '@')
            
            quadrant = [(r,c, W[r][c])
                        for r in range(hcuts[h]+1, hcuts[h+1]+1)
                        for c in range(vcuts[v]+1, vcuts[v+1]+1)]
            print(quadrant)
            print(h, v, quadcount, ch_per_p)
            if quadcount != ch_per_p:
                return "IMPOSSIBLE"
            
    return "POSSIBLE"


def print_solutions(solutions):
    # output:
    # Case #X: min_number_of_hacks|impossible
    for i, s in enumerate(solutions):
        print("Case #{}: {}".format(i+1, s))


if __name__ == '__main__':
    solutions = [solve(p) for p in read_input()]
    print_solutions(solutions)
