def read_input():
    # T test cases
    t = int(input())
    for i in range(1, t+1):
        # R C H V
        R, B, C = list(map(int, input().split()))
        # (Id, Mi, Si, Pi)
        cashiers = [tuple(map(int, input().split()))
                    for _ in range(C)]
        yield (R, B, C, cashiers)


def solve(P):
    (R, B, C, cashiers) = P

    def time(C, n):
        (Mi, Si, Pi) = C
        return min(Mi, n)*Si + Pi
    
    # choose cashier with min time to process remaining
    remaining = B
    max_time = 0
    
    print("new")
    print(P)
    while remaining > 0:
        best = min(cashiers, key=lambda c: time(c, remaining))
        (Mb, Sb, Pb) = best
        time_best = time(best, remaining)
        print(best, time_best, remaining)
        if time_best > max_time:
            max_time = time_best
        remaining -= min(Mb, remaining)
        cashiers.remove(best)
        
    return max_time


def print_solutions(solutions):
    # output:
    # Case #X: min_number_of_hacks|impossible
    for i, s in enumerate(solutions):
        print("Case #{}: {}".format(i+1, s))


if __name__ == '__main__':
    solutions = [solve(p) for p in read_input()]
    print_solutions(solutions)
