
def read_input():
    # T test cases
    T = int(input())
    for i in range(1, T+1):
        S = int(input())
        Signs = []
        for j in range(1, S+1):
            (Di, Ai, Bi) = tuple(map(int, input().split()))
            Signs.append((Di, Ai, Bi))
        yield Signs


def solve(P):
    Signs = P

    if len(P) == 1:  # case there is just one sign, return 1
        return (1, 1)

    MNs = [(Di+Ai, Di-Bi) for (Di, Ai, Bi) in Signs]

    closedsets = []
    opensets = []  # sets of sets

    maxseq = 1
    for (signidp, MN) in enumerate(MNs[1:]):
        (M, N) = MN
        for poss in opensets:
            ids, (free, decisions) = poss
            if free == 0:
                if M == decisions[0] and N == decisions[1]:
                    newslots = [(0, (M, N))]
                else:
                    newslots = False  # close set
            elif free == 1:
                if M == decisions:
                    newslots = [(1, decisions)]  # keep at it is
                else:
                    newslots = [(0, (decisions, N))]
            elif free == 2:
                if N == decisions:
                    newslots = [(2, decisions)]
                else:
                    newslots = [(0, (M, decisions))]
            elif free == 3:
                if M == decisions[0] and N == decisions[1]:
                    newslots = [(3, decisions)]  # keep as it is
                elif M == decisions[0] and N != decisions[1]:
                    newslots = [(1, M)]
                elif N == decisions[1] and M != decisions[0]:
                    newslots = [(2, N)]
                else:
                    newslots = [  # two possibilities
                        (0, (decisions[0], N)),
                        (0, (M, decisions[1]))
                    ]

            if not newslots:  # remove group from opensets
                # add ids to close slots
                seq_size = len(ids)
                if seq_size < maxseq:
                    pass
                else:
                    if seq_size > maxseq:
                        closedsets = set()
                    # FIXME: sets would replace this check
                    if ids not in closedsets:
                        closedsets.append(ids)
                    maxseq = seq_size
                opensets.remove(poss)
            else:
                for sl in newslots:
                    opensets.append((ids.add(signidp+1), sl))

        # start new set from current position
        (Mp, Np) = MNs[signidp]
        # slot open:
        # C O - 1
        # O C - 2
        # C C - 0
        # O O - 3
        if M == Mp and N == Np:
            slots = (3, (M, N))
        elif M == Mp:
            slots = (1, M)
        elif N == Np:
            slots = (2, N)
        elif M != Mp and N != Np:
            slots = (0, (M, N))
        opensets.append(({signidp, signidp+1}, slots))  # start new sign with s

    return (len(closedsets), len(closedsets[0]))


def print_solutions(solutions):
    # output:
    # Case #X: min_number_of_hacks|impossible
    for i, s in enumerate(solutions):
        print("Case #{}: {} {}".format(i+1, s[0], s[1]))


if __name__ == '__main__':
    solutions = [solve(p) for p in read_input()]
    print_solutions(solutions)
