import sys
import math


def play(a):
    field_side = 1000

    # Choose a square of size A inside the field to be filled.
    # Look for a central region
    # FIXME: dirty
    a_side1 = math.ceil(math.sqrt(a))
    a_side2 = a_side1-1 if a_side1*(a_side1-1) >= a else a_side1

    build_region = ((field_side//2)-((a_side1//2)),
                    (field_side//2)-((a_side2//2)))

    # (r1, c1), (r2, c2)
    target_limits = ((build_region[0]+1, build_region[1]+1),
                     (build_region[0]+a_side1-2, build_region[1]+a_side2-2))
    targets = {(tr, tc): 9  # target coordinates and score
               for tr in range(target_limits[0][0], target_limits[1][0]+1)
               for tc in range(target_limits[0][1], target_limits[1][1]+1)}

    filled = set()

    attempts = 1
    while play_round(targets, filled, target_limits):
        attempts += 1

    return attempts


def play_round(targets, filled, target_limits):
    # play on best score possible
    ((tr, tc), _score) = max(targets.items(), key=lambda t: t[1])

    # send target
    print(tr, tc)
    sys.stdout.flush()
    pr, pc = list(map(int, input().split()))

    if (pr, pc) == (-1, -1):
        return False  # end game here

    if (pr, pc) == (0, 0):
        return False

    # update scores of targets around tree planted
    if not (pr, pc) in filled:
        # for every neighbour of field
        ((tlr1, tlc1), (tlr2, tlc2)) = target_limits
        neighs = [(r, c)
                  for r in range(pr-1, pr+2)
                  for c in range(pc-1, pc+2)
                  if tlr1 <= r <= tlr2 and tlc1 <= c <= tlc2]

        for n in neighs:
            targets[n] -= 1
        filled.add((pr, pc))
    return True


if __name__ == '__main__':
    for _ in range(int(input())):  # T test cases
        a = int(input())  # minimum required area
        play(a)
