def read_input():
    # T test cases
    t = int(input())
    all_cases = []
    for i in range(1, t+1):
        # two lines:
        # N
        n = int(input())
        # values
        vs = [int(v) for v in input().split()]
        all_cases.append((n, vs))
    return all_cases


def solve(n, vs):
    "find solution for a single case"

    # split the original lists in two
    v1 = vs[::2]
    v2 = vs[1::2]

    # sort both lists
    v1.sort()
    v2.sort()

    # deal with odd numbers
    if n % 2:
        v2.append(v1[-1])

    last_e2 = v1[0]
    for par, (e1, e2) in enumerate(zip(v1, v2)):
        if e2 < e1:
            return (par*2+1)-1
        if e1 < last_e2:
            return (par*2)-1
        last_e2 = e2

    return "OK"


def print_solutions(solutions):
    for i, sol in enumerate(solutions):
        print("Case #{}: {}".format(i+1, sol))


if __name__ == '__main__':
    all_cases = read_input()
    solutions = [solve(*p) for p in all_cases]
    print_solutions(solutions)
