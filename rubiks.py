# magic cube

# colours: red, green, blue, white, orange, yellow
import copy
import heapq

colours = ["R", "G", "B", "W", "O", "Y"]
# long names correspond to bloomberg's rubiks cube I have at my desk
black = R = r = "R"
yellow = G = g = "G"
blue = B = b = "B"
white = W = w = "W"
green = O = o = "O"
pink = Y = y = "Y"


class Cube:
    def __init__(self):
        self.sides = {
            W: Side(W, B, R, G, O, Y),
            R: Side(R, B, Y, G, W, O),
            Y: Side(Y, B, O, G, R, W),
            O: Side(O, B, W, G, Y, R),
            B: Side(B, W, O, Y, R, G),
            G: Side(G, W, R, Y, O, B)
        }

    def neighbours(self):
        for side in self.sides.values():
            for rotation, name in [(self.rotate_1, "clockwise"),
                                   (self.rotate_3, "anti-clockwise")]:
                yield (rotation(side).to_state(),
                       "Turn {} (with {} up) {}".format(side.colour,
                                                         side.up,
                                                         name)
                       )

    def to_state(self):
        all_strings = [self.sides[c].get_keys_string() for c in colours]
        return "".join(all_strings)

    def copy(self):
        new_cube = Cube()
        new_cube.sides = copy.deepcopy(self.sides)
        return new_cube

    def print_cube(self):
        for c in colours:
            self.sides[c].print_side()
            self.sides[c].check_corners()

    def get_distance(self):
        visit_order = ["WOB", "WB", "WBR", "WR", "WRG", "WG", "WGO", "WO",
                       "OB", "BR", "RG", "GO",
                       "YRB", "YB", "YBO", "YO", "YOG", "YG", "YGR", "YR"]

        for v in visit_order:
            a = self.get_colours_from_pos(v)
            distances[a][v]

        return sum(distances[self.get_colours_from_pos(v)][v]
                   for v in visit_order)

    def get_colours_from_pos(self, pos):
        if len(pos) == 2:
            c1 = self.sides[pos[0]].find_pos_with_neighbour(pos[1])
            c2 = self.sides[pos[1]].find_pos_with_neighbour(pos[0])
            return c1+c2
        elif len(pos) == 3:
            c1 = self.sides[pos[0]].find_pos_with_neighbour(pos[1]+pos[2])
            c2 = self.sides[pos[1]].find_pos_with_neighbour(pos[2]+pos[0])
            c3 = self.sides[pos[2]].find_pos_with_neighbour(pos[0]+pos[1])
            return c1+c2+c3

    def rotate_1(self, side):
        # rotate the whole side once clockwise

        # 1 - the side itself
        cube = self.copy()
        new_side = side.copy_side()
        up, right, down, left = (side.up, side.right, side.down, side.left)
        new_side.keys[up] = side.keys[left]
        new_side.keys[right] = side.keys[up]
        new_side.keys[down] = side.keys[right]
        new_side.keys[left] = side.keys[down]
        cube.sides[side.colour] = new_side

        # 2 - the edges
        new_up = cube.sides[side.left].keys[side.colour]
        new_right = cube.sides[side.up].keys[side.colour]
        new_down = cube.sides[side.right].keys[side.colour]
        new_left = cube.sides[side.down].keys[side.colour]

        cube.sides[side.up].change_keys(side.colour, new_up)
        cube.sides[side.right].change_keys(side.colour, new_right)
        cube.sides[side.down].change_keys(side.colour, new_down)
        cube.sides[side.left].change_keys(side.colour, new_left)

        return cube

    def rotate_3(self, side):
        # rotate the whole side clockwise three times (or once anti-clockwise)

        # 1 - the side itself
        cube = self.copy()
        new_side = side.copy_side()
        up, right, down, left = (side.up, side.right, side.down, side.left)
        new_side.keys[up] = side.keys[right]
        new_side.keys[right] = side.keys[down]
        new_side.keys[down] = side.keys[left]
        new_side.keys[left] = side.keys[up]
        cube.sides[side.colour] = new_side

        # 2 - the edges
        new_up = cube.sides[side.right].keys[side.colour]
        new_right = cube.sides[side.down].keys[side.colour]
        new_down = cube.sides[side.left].keys[side.colour]
        new_left = cube.sides[side.up].keys[side.colour]

        cube.sides[side.up].change_keys(side.colour, new_up)
        cube.sides[side.right].change_keys(side.colour, new_right)
        cube.sides[side.down].change_keys(side.colour, new_down)
        cube.sides[side.left].change_keys(side.colour, new_left)

        return cube

    # def rotate_2(side):
    #     # rotate the whole side twice clockwise

    #     # 1 - the side itself
    #     new_side = side.copy_side()
    #     up, right, down, left = (side.up, side.right, side.down, side.left)
    #     new_side.keys[up] = side.keys[down]
    #     new_side.keys[right] = side.keys[left]
    #     new_side.keys[down] = side.keys[up]
    #     new_side.keys[left] = side.keys[right]
    #     cube[side.colour] = new_side

    #     # 2 - the edges
    #     new_up = cube[side.down].keys[side.colour]
    #     new_right = cube[side.left].keys[side.colour]
    #     new_down = cube[side.up].keys[side.colour]
    #     new_left = cube[side.right].keys[side.colour]

    #     cube[side.up].change_keys(side.colour, new_up)
    #     cube[side.right].change_keys(side.colour, new_right)
    #     cube[side.down].change_keys(side.colour, new_down)
    #     cube[side.left].change_keys(side.colour, new_left)


class Side:
    def __init__(self, colour, up, right, down, left, back):
        self.colour = colour
        self.up = up
        self.down = down
        self.right = right
        self.left = left
        self.back = back
        # self.keys = [[None, None, None],
        #              [None, colour, None],
        #              [None, None, None]]
        self.keys = {
            up: [None, None, None],
            right: [None, None, None],
            down: [None, None, None],
            left: [None, None, None]
        }

        self.directions = [up, right, down, left]

    def neighbours(self, up_reference=None):
        if not up_reference:
            return self.directions
        else:
            reference_index = self.directions.index(up_reference)
            return self.directions[reference_index:] + self.directions[:reference_index]

    def find_pos_with_neighbour(self, neighbours):
        if len(neighbours) == 1:
            if neighbours == self.up:
                d = self.up
            elif neighbours == self.right:
                d = self.right
            elif neighbours == self.down:
                d = self.down
            elif neighbours == self.left:
                d = self.left
            return self.keys[d][1]

        elif len(neighbours) == 2:
            if neighbours[0] == self.left and neighbours[1] == self.up:
                d = self.up
            elif neighbours[0] == self.up and neighbours[1] == self.right:
                d = self.right
            elif neighbours[0] == self.right and neighbours[1] == self.down:
                d = self.down
            elif neighbours[0] == self.down and neighbours[1] == self.left:
                d = self.left
            return self.keys[d][0]

        # cube.sides[r].read_side([r, g, r, g, r, o, r, w, y])
        #         self.sides = {
        #     W: Side(W, B, R, G, O, Y),
        #     R: Side(R, B, Y, G, W, O),
        #     Y: Side(Y, B, O, G, R, W),
        #     O: Side(O, B, W, G, Y, R),
        #     B: Side(B, W, O, Y, R, B),
        #     G: Side(G, W, R, Y, O, G)
        # }

    def copy_side(self):
        return Side(self.colour, self.up, self.right,
                    self.down, self.left, self.back)

    def change_keys(self, direction, new_values):
        self.keys[direction] = new_values

        if direction == self.up:
            a, b = self.left, self.right
        elif direction == self.right:
            a, b = self.up, self.down
        elif direction == self.down:
            a, b = self.right, self.left
        elif direction == self.left:
            a, b = self.down, self.up

        self.keys[a][-1] = new_values[0]
        self.keys[b][0] = new_values[-1]

    def read_side(self, pieces):
        # pieces:
        #   (up)
        # [0,1,2,
        #  3,4,5,
        #  6,7,8]
        self.keys[self.up] = [pieces[0], pieces[1], pieces[2]]
        self.keys[self.right] = [pieces[2], pieces[5], pieces[8]]
        self.keys[self.down] = [pieces[8], pieces[7], pieces[6]]
        self.keys[self.left] = [pieces[6], pieces[3], pieces[0]]

    def get_keys_string(self):
        return ("".join(self.keys[self.up]) + self.keys[self.left][1] +
                self.colour + self.keys[self.right][1] +
                "".join(self.keys[self.down][::-1]))

    def print_side(self):
        print("       {}".format(self.up))
        print("       ^")
        print("       |")
        print("     {}|{}|{}".format(*self.keys[self.up]))
        print("{} <- {}|{}|{} ->  {}".format(
              self.left, self.keys[self.left][1],
              self.colour, self.keys[self.right][1],
              self.right))
        print("     {}|{}|{}".format(*self.keys[self.down][::-1]))
        print("       |")
        print("       v")
        print("       {}".format(self.down))

    def check_corners(self):
        condition = all(self.keys[self.directions[i]][0] ==
                        self.keys[self.directions[i-1]][-1] for i in range(4))

        if condition:
            print("corners ok!")
        else:
            print(">>>ERROR!")
            print(self.keys)


def all_distances():
    distances = {}

    cube = {
        W: Side(W, B, R, G, O, Y),
        R: Side(R, B, Y, G, W, O),
        Y: Side(Y, B, O, G, R, W),
        O: Side(O, B, W, G, Y, R),
        B: Side(B, W, O, Y, R, G),
        G: Side(G, W, R, Y, O, B)
    }

    # first: middle pieces
    for base in colours:
        for side_colour in cube[base].neighbours():
            reference = base+side_colour
            dist_from_reference = {}

            rotated_neighbours = cube[base].neighbours(side_colour)

            dist_from_reference[base+rotated_neighbours[0]] = 0
            dist_from_reference[base+rotated_neighbours[1]] = 1
            dist_from_reference[base+rotated_neighbours[2]] = 2
            dist_from_reference[base+rotated_neighbours[3]] = 1

            # "up"
            up_base = rotated_neighbours[0]
            up_neighbours = cube[up_base].neighbours(base)
            dist_from_reference[up_base+up_neighbours[0]] = 3
            dist_from_reference[up_base+up_neighbours[1]] = 2
            dist_from_reference[up_base+up_neighbours[2]] = 3
            dist_from_reference[up_base+up_neighbours[3]] = 2

            # "right"
            right_base = rotated_neighbours[1]
            right_neighbours = cube[right_base].neighbours(base)
            dist_from_reference[right_base+right_neighbours[0]] = 2
            dist_from_reference[right_base+right_neighbours[1]] = 1
            dist_from_reference[right_base+right_neighbours[2]] = 2
            dist_from_reference[right_base+right_neighbours[3]] = 3

            # "down"
            down_base = rotated_neighbours[2]
            down_neighbours = cube[down_base].neighbours(base)
            dist_from_reference[down_base+down_neighbours[0]] = 3
            dist_from_reference[down_base+down_neighbours[1]] = 2
            dist_from_reference[down_base+down_neighbours[2]] = 3
            dist_from_reference[down_base+down_neighbours[3]] = 2

            # "left"
            left_base = rotated_neighbours[3]
            left_neighbours = cube[left_base].neighbours(base)
            dist_from_reference[left_base+left_neighbours[0]] = 2
            dist_from_reference[left_base+left_neighbours[1]] = 3
            dist_from_reference[left_base+left_neighbours[2]] = 2
            dist_from_reference[left_base+left_neighbours[3]] = 1

            # back
            back_base = cube[base].back
            back_neighbours = cube[back_base].neighbours(left_base)  # arbitrary, but ok
            dist_from_reference[back_base+back_neighbours[0]] = 3
            dist_from_reference[back_base+back_neighbours[1]] = 2
            dist_from_reference[back_base+back_neighbours[2]] = 3
            dist_from_reference[back_base+back_neighbours[3]] = 4

            distances[reference] = dist_from_reference

    # second: corners
    for base in colours:
        neighbours_colours = cube[base].neighbours()
        edges_colours = [neighbours_colours[i-1]+neighbours_colours[i]
                         for i in range(4)]
        for edge_colours in edges_colours:
            reference = base+edge_colours
            dist_from_reference = {}

            rotated_neighbours = "".join(list(cube[base].neighbours(edge_colours[-1])[-1]) + cube[base].neighbours(edge_colours[-1]))

            dist_from_reference[base+rotated_neighbours[0:2]] = 0
            dist_from_reference[base+rotated_neighbours[1:3]] = 1
            dist_from_reference[base+rotated_neighbours[2:4]] = 2
            dist_from_reference[base+rotated_neighbours[3:5]] = 1

            # "up"
            up_base = rotated_neighbours[0]
            up_neighbours = "".join(list(cube[up_base].neighbours(base)[-1]) + cube[up_base].neighbours(base))
            dist_from_reference[up_base+up_neighbours[0:2]] = 3
            dist_from_reference[up_base+up_neighbours[1:3]] = 2
            dist_from_reference[up_base+up_neighbours[2:4]] = 1
            dist_from_reference[up_base+up_neighbours[3:5]] = 2

            # "right"
            right_base = rotated_neighbours[1]
            right_neighbours = "".join(list(cube[right_base].neighbours(base)[-1]) + cube[right_base].neighbours(base))
            dist_from_reference[right_base+right_neighbours[0:2]] = 2
            dist_from_reference[right_base+right_neighbours[1:3]] = 1
            dist_from_reference[right_base+right_neighbours[2:4]] = 2
            dist_from_reference[right_base+right_neighbours[3:5]] = 3

            # "down"
            down_base = rotated_neighbours[2]
            down_neighbours = "".join(list(cube[down_base].neighbours(base)[-1]) + cube[down_base].neighbours(base))
            dist_from_reference[down_base+down_neighbours[0:2]] = 1
            dist_from_reference[down_base+down_neighbours[1:3]] = 2
            dist_from_reference[down_base+down_neighbours[2:4]] = 2
            dist_from_reference[down_base+down_neighbours[3:5]] = 3

            # "left"
            left_base = rotated_neighbours[3]
            left_neighbours = "".join(list(cube[left_base].neighbours(base)[-1]) + cube[left_base].neighbours(base))
            dist_from_reference[left_base+left_neighbours[0:2]] = 2
            dist_from_reference[left_base+left_neighbours[1:3]] = 3
            dist_from_reference[left_base+left_neighbours[2:4]] = 2
            dist_from_reference[left_base+left_neighbours[3:5]] = 1

            # back
            back_base = cube[base].back
            back_neighbours = "".join(list(cube[back_base].neighbours(left_base)[-1]) + cube[back_base].neighbours(left_base))
            dist_from_reference[back_base+back_neighbours[0:2]] = 3
            dist_from_reference[back_base+back_neighbours[1:3]] = 2
            dist_from_reference[back_base+back_neighbours[2:4]] = 3
            dist_from_reference[back_base+back_neighbours[3:5]] = 4

            distances[reference] = dist_from_reference

    return distances


distances = all_distances()


def a_star(start, goal):
    g_score = dict()
    #f_score = dict()
    g_score[start] = 0
    f_score = g_score[start] + state_to_cube(start).get_distance()
    gods_number = 26

    closedset = set()
    openset = {start}
    openheap = []
    heapq.heappush(openheap, (f_score, start))
    came_from = dict()

    iterations = 0
    while openset is not None:
        current_score, current = heapq.heappop(openheap)#min(openset, key=lambda x: f_score[x])
        if not iterations % 1024:
            print("Already visited {} states, current_score: {}".format(iterations, current_score))
        # print(current_score)
        if current == goal:
            print("Found the solution after visiting {} states!".format(iterations))
            return reconstruct_path(came_from, goal)

        openset.remove(current)
        closedset.add(current)

        for (neighbour, instruction) in state_to_cube(current).neighbours():
            if neighbour in closedset:
                continue
            t_g_score = (g_score[current] +
                         8 if (g_score[current] < gods_number*8) else 1000)
            if neighbour not in openset or t_g_score < g_score[neighbour]:
                came_from[neighbour] = (current, instruction)
                g_score[neighbour] = t_g_score
                f_score = (g_score[neighbour] +
                                      state_to_cube(neighbour).get_distance()) #FIXME: quando t_g_score < g_score, nao grava o resultado (certo?)
                # print("Father: {}, son: {}, f: {}".format(current_score, f_score, f_score-g_score[neighbour]))
                if neighbour not in openset:
                    openset.add(neighbour)
                    heapq.heappush(openheap, (f_score, neighbour))

        iterations += 1

    return None


def reconstruct_path(came_from, current):
    total_path = [current]
    while current in came_from:
        current, instruction = came_from[current]
        total_path.insert(0, instruction)
    return total_path


def state_to_cube(state):
    cube = Cube()
    for i, c in enumerate(colours):
        cube.sides[c].read_side(state[i*9:(i+1)*9])

    return cube


# W: Side(W, B, R, G, O, Y),
# R: Side(R, B, Y, G, W, O),
# Y: Side(Y, B, O, G, R, W),
# O: Side(O, B, W, G, Y, R),
# B: Side(B, W, O, Y, R, G),
# G: Side(G, W, R, Y, O, B)
if __name__ == '__main__':
    cube = Cube()
    #w = white
    #r = black
    #y = pink
    #o = green
    #b = blue
    #g = yellow
    cube.sides[w].read_side([white, white, white, white, white, white, white, white, white])
    cube.sides[r].read_side([black, pink, green, black, black, green, black, black, blue])
    cube.sides[y].read_side([pink, green, pink, yellow, pink, pink, green, black, yellow])
    cube.sides[o].read_side([blue, black, green, blue, green, green, black, pink, green])
    cube.sides[b].read_side([blue, blue, blue, yellow, blue, blue, yellow, blue, black])
    cube.sides[g].read_side([yellow, yellow, yellow, green, yellow, pink, pink, yellow, pink])

    # cube.sides[w].read_side([b, b, b, b, w, b, b, b, b])
    # cube.sides[r].read_side([w, w, w, w, r, w, w, w, w])
    # cube.sides[y].read_side([g, g, g, g, y, g, g, g, g])
    # cube.sides[o].read_side([y, y, y, y, o, y, y, y, y])
    # cube.sides[b].read_side([r, r, r, r, w, r, r, r, r])
    # cube.sides[g].read_side([o, o, o, o, g, o, o, o, o])


    # cube.sides[w].read_side([y, y, y, w, w, w, w, w, w])
    # cube.sides[r].read_side([o, o, o, r, r, r, r, r, r])
    # cube.sides[y].read_side([w, w, w, y, y, y, y, y, y])
    # cube.sides[o].read_side([r, r, r, o, o, o, o, o, o])
    # cube.sides[b].read_side([b, b, b, b, b, b, b, b, b])
    # cube.sides[g].read_side([g, g, g, g, g, g, g, g, g])

    goal_cube = Cube()

    goal_cube.sides[w].read_side([w, w, w, w, w, w, w, w, w])
    goal_cube.sides[r].read_side([r, r, r, r, r, r, r, r, r])
    goal_cube.sides[y].read_side([y, y, y, y, y, y, y, y, y])
    goal_cube.sides[o].read_side([o, o, o, o, o, o, o, o, o])
    goal_cube.sides[b].read_side([b, b, b, b, b, b, b, b, b])
    goal_cube.sides[g].read_side([g, g, g, g, g, g, g, g, g])

    print(cube.to_state())
    cube.print_cube()

    import ipdb; ipdb.set_trace()

    result = a_star(cube.to_state(), goal_cube.to_state())

    import ipdb; ipdb.set_trace()

    for l in result:
        print(l)



# cube = {
#     W: Side(W, B, R, G, O),
#     R: Side(R, B, Y, G, W),
#     Y: Side(Y, B, O, G, R),
#     O: Side(O, B, W, G, Y),
#     B: Side(B, W, O, Y, R),
#     G: Side(G, W, R, Y, O)
# }


# WRG
# x
