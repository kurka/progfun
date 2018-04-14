import math


def solve(a, case):
    # try to rotate axis xy first (a will vary from 1-1.41)
    if a > math.sqrt(2):  # FIXME
        a = 1.414213

    # find angle that gives a
    theta = 2*math.atan((1-math.sqrt(2-a*a))/(a+1))
    # theta = 2*math.atan(((math.sqrt(2-a*a)+1)/(a+1)))
    print(theta)
    # print(theta1)


    # find position of rotated face (xy)
    x = (math.cos(theta)*0.5, math.sin(theta)*0.5, 0.0)
    y = (math.sin(theta)*-0.5, math.cos(theta)*0.5, 0.0)
    z = (0, 0, 0.5)

    print("Case #{}:".format(case))
    print(x[0], x[1], x[2])
    print(y[0], y[1], y[2])
    print(z[0], z[1], z[2])


if __name__ == '__main__':
    t = int(input())
    for i, _ in enumerate(range(t)):
        solve(float(input()), i+1)
    # as = [float(input()) for i in range(t)]
    # solutions = [solve(a, i+1) for i, a in enumerate(as)]
