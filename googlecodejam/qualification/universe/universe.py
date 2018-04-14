def read_input():
    # T test cases
    t = int(input())
    all_cases = {}
    for i in range(1, t+1):
        # D CCSSCCCSSCS
        dstr, pstr = input().split()
        d = int(dstr)
        p = list(pstr)
        all_cases[i] = (d, p)
    return all_cases


def solve(d, p):
    "find solution for a single case"

    # first check if a solution is possible
    min_damage = len([s for s in p if s == 'S'])
    if min_damage > d:
        return "IMPOSSIBLE"

    # then check if defense by itself is enough (no moves)
    init_score = score(p)
    if init_score <= d:
        return 0

    # create a data structure to compute number of S in each slot of C
    slots = []
    slot_count = 0
    for c in p:
        if c == 'S':
            slot_count += 1
        elif c == 'C':
            # move to next slot
            slots.append(slot_count)
            slot_count = 0
    slots.append(slot_count)

    new_score = init_score
    moves = 0
    for b in range(len(slots)-1, 0, -1):  # stop b in 1 (can't move 0)
        shoots_in_bin = slots[b]
        for m in range(1, shoots_in_bin+1):
            moves += 1
            new_score -= 2**(b-1)
            slots[b-1] += 1  # move shoot to slot
            if new_score <= d:
                return moves

    return moves


def score(p):
    "compute the damage of a program"
    cur_strength = 1
    damage = 0
    for c in p:
        if c == 'S':
            damage += cur_strength
        elif c == 'C':
            cur_strength *= 2

    return damage


def print_solutions(solutions):
    # output:
    # Case #X: min_number_of_hacks|impossible
    ord_cases = sorted(solutions.keys())
    for case in ord_cases:
        print("Case #{}: {}".format(case, solutions[case]))


if __name__ == '__main__':
    all_cases = read_input()
    solutions = {k: solve(*v) for (k, v) in all_cases.items()}
    print_solutions(solutions)
