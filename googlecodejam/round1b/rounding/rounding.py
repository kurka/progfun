from decimal import Decimal, ROUND_HALF_UP

def read_input():
    # T test cases
    T = int(input())
    for i in range(1, T+1):
        N, L = tuple(map(int, input().split()))
        Cs = list(map(int, input().split()))
        yield (N, L, Cs)


def solve(P):
    (N, L, Csi) = P
    TotAns = sum(Csi)
    RemAns = N-TotAns
    
    PossLang = L+RemAns
    Cs = Csi + [0]*RemAns
    #TODO: create array with PossLang size?
    
    # how to distribute RemAns so we can maximize the percentages?
    ContrWeight = myround(100/n)
    
    # find where the jump is
    jump = 0 # or 10
    for j in range(1, n+1):
        myround((j*100)/n) > j*myround
        jump = j
        break
    
    if jump == 0:
        return 100
    
    # try to complete all cases closest possible to jump
    Rem = RemAns
    while Rem:
        closestjump = max(Cs, key=lambda c: c%jump)
        for (i,c) in enumerate(Cs):
            if i == closestjump:
                Cs[i] += 

    


def myround(x):
    return Decimal(x).quantize(0, ROUND_HALF_UP) 

def print_solutions(solutions):
    # output:
    # Case #X: min_number_of_hacks|impossible
    for i, s in enumerate(solutions):
        print("Case #{}: {}".format(i+1, s))


if __name__ == '__main__':
    solutions = [solve(p) for p in read_input()]
    print_solutions(solutions)
