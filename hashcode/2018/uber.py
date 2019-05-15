import sys

INF = 10e30  # bad practice!

# assign rides to vehicles
class Car:
    def __init__(self, ttime, maxdist):
        self.cur_pos = (0, 0)
        self.curtime = 0
        self.gametime = ttime
        self.maxdist = maxdist
        self.rides = []

    # @profile
    def checkviability(self, ride):
        rideid, pstart, pend, start, end, ridedistance = ride
        # check 1: do I have enough time to do it (from game perspective)?
        # time to target
        time2target = distance(self.cur_pos, pstart)
        waittime = max(0, start - time2target)
        totaldrive = time2target + waittime + ridedistance
        if self.curtime + totaldrive > self.gametime:  # FIXME: check if > or >=
            return False  # don't allocate if there is enough time

        # check 2 - is there enough time from riders perspective?
        if self.curtime + totaldrive > end:
            return False  # no points here


        return True

    def assignride(self, ride):
        rideid, pstart, pend, start, end, ridedistance = ride
        # assign ride if possible (i.e. if has enough time remaining and if it's worth)
        # if not self.checkviability(ride):
        #     return False

        self.rides.append(rideid)
        # update position and time
        self.cur_pos = pend
        time2ride = distance(self.cur_pos, pstart)
        waittime = max(0, start - time2ride)
        self.curtime += time2ride + waittime + ridedistance
        return True

    # @profile
    def orderrides(self, rides):
        # order by proximity
        # scored_rides = [(r, self.score(r)) for r in rides]
        # return [rs[0] for rs in sorted(scored_rides, key=lambda rs: rs[1])]
        bestride = min(rides, key=self.scoredisttime)
        return (bestride, self.scoredisttime(bestride))

    def scoredist(self, ride):
        # score by proximity
        return distance(self.cur_pos, ride[1])

    # @profile
    def scoredisttime(self, ride):
        _id, posstart, posend, start, end, distride = ride
        dist = distance(self.cur_pos, posstart)
        normdist = dist / self.maxdist
        timebeforestart = start - self.curtime
        normtimebeforestart = timebeforestart / self.gametime
        ridelength = distance(posstart, posend)
        bonus = 1-int(self.curtime + distance(self.cur_pos, posstart) <= start)
        # score = (bonus, (0.5*normdist + 0.5*normtimebeforestart), ridelength)
        # score = (bonus, dist, ridelength, timebeforestart)
        # score = (ridelength, dist, bonus, timebeforestart)
        score = (timebeforestart),# - ridelength,)
        # score = (0.9*normtimebeforestart + 0.1*normdist + 0.2*(ridelength/self.maxdist),)
        if self.checkviability(ride):
            return score
        else:
            return (INF,) + score[1:]   # don't consider this ride

    def printoutput(self):
        totalrides = len(self.rides)
        print(" ".join(map(str, [totalrides] + self.rides)))


def process_input():
    rows, columns, ncars, nrides, bonus, ttime = list(map(int, input().strip().split(' ')))

    # Rides descriptions
    # Ride = tuple(id, PosStart, PosEnd, start, end, distride)
    # Pos = (row, col)
    rides = []
    for ride in range(nrides):
        rstart, cstart, rend, cend, start, end = list(map(int, input().strip().split(' ')))
        rides.append((ride, (rstart, cstart), (rend, cend), start, end, distance((rstart, cstart), (rend, cend))))
        if end < start:
            print("Hey! Got end {end} and start {start!!}", file=sys.stderr)

    # analyse if bonus is important
    # create the cars
    maxdist = rows+columns
    cars = [Car(ttime, maxdist) for _ in range(ncars)]

    return (rows, columns), cars, rides, bonus, ttime


# @profile
def solve(cars, rides, bonus, ttime):

    ncars = len(cars)
    # sort rides #TODO: implement something good here
    # sorted_rides = sorted(rides, key=lambda r: r[3])  # sorting by initial time
    # for ride in sorted_rides:
    #     cars[ride[0] % ncars].assignride(ride)

    remaining_rides = rides
    new_allocations = True
    while(new_allocations):
        old_rides = len(remaining_rides)
        for car in cars:
            best_ride, score = car.orderrides(remaining_rides)
            if score[0] < INF:
                car.assignride(best_ride)
                remaining_rides.remove(best_ride)  # TODO: check time!
                if not remaining_rides:
                    break
        new_allocations = len(remaining_rides) < old_rides and len(remaining_rides) > 0


def print_solution(cars):
    for car in cars:
        car.printoutput()


def distance(pos1, pos2):
    rp1, cp1 = pos1
    rp2, cp2 = pos2

    return abs(rp2-rp1) + abs(cp2-cp1)


if __name__ == '__main__':
    # gridsize = (nrows, ncols)
    # Rides = list of rides
    # Ride = tuple(id, PosStart, PosEnd, start, end, distride)
    # Pos = (row, col)
    _, cars, rides, bonus, ttime = process_input()
    solve(cars, rides, bonus, ttime)
    print_solution(cars)

