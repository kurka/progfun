#!/usr/bin/env python


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


def tsort(L):
    done = False
    while not done:
        done = True
        for i in range(len(L)-2):
            if L[i] > L[i+2]:
                done = False
                L[i], L[i+2] = L[i+2], L[i]


def check_error(C, L):
    # print(L)
    error = "OK"
    for i in range(len(L)-1):
        if L[i+1] < L[i]:
            error = i+1
            break

    print("Case #{} {}".format(C, error))


if __name__ == '__main__':
    for i, (n, vs) in enumerate(read_input()):
        tsort(vs)
        check_error(i+1, vs)
