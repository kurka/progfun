
def read_input():
    # T test cases
    T = int(input())
    for i in range(1, T+1):
        N = int(input())
        Ws = list(map(int, input().split()))
        yield (N, Ws)


def solve(P):
    (N, Ws) = P
    poss = dict()  # list of members, current weight, capacity

    for wi in Ws:
        newsols = dict()
        for (sizep, weightp) in poss.items():
            # check if can increase p with wi
            if weightp <= 6*wi:
                if (sizep+1 in newsols):
                    if (weightp+wi < newsols[sizep+1]):
                        newsols[sizep+1] = weightp+wi
                else:
                    newsols[sizep+1] = weightp+wi
                        
                newsols[sizep+1] = weightp+wi

        # add wi alone as option
        if (1 in newsols):
            if (wi < newsols[1]):
                newsols[1] = wi
        else:
            newsols[1] = wi
        for (nsn, nsw) in newsols.items():
            if nsn in poss:
                if nsw < poss[nsn]:
                    poss[nsn] = nsw
            else:
                poss[nsn] = nsw
            
    return max(poss.keys())


def print_solutions(solutions):
    # output:
    # Case #X: min_number_of_hacks|impossible
    for i, s in enumerate(solutions):
        print("Case #{}: {}".format(i+1, s))


if __name__ == '__main__':
    solutions = [solve(p) for p in read_input()]
    print_solutions(solutions)
