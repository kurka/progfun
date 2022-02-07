package main

import (
	"fmt"
	"math"
)

const numRooms = 4
const empty = '.'

type State struct {
	//h0 h1  h2  h3  h4  h5 h6
	//     r0  r1  r2  r3
	// #..#.#.#.#..#
	// ###.#.#.#.###
	hallway [numRooms + 3]rune
	rooms   [numRooms]Room
}

func (s State) String() string {
	// add hallway
	sString := fmt.Sprintf("#%c%c.%c.%c.%c.%c%c#\n", s.hallway[0], s.hallway[1], s.hallway[2], s.hallway[3], s.hallway[4], s.hallway[5], s.hallway[6])
	// add rooms
	for d := range s.rooms[0].slots {
		sString += "###"
		for i := range s.rooms {
			sString += fmt.Sprintf("%c#", s.rooms[i].slots[d])
		}
		sString += "##\n"
	}
	return sString
}

func (m Move) String() string {
	return fmt.Sprintf("M(%v -> %v)", m.from, m.to)
}

type Room struct {
	slots []rune
}

type Move struct {
	from, to Pos
}

type Pos struct {
	ltype      rune
	pos, depth int
}

var hallIdx2Pos = map[int]int{
	0: 0,
	1: 1,
	2: 3,
	3: 5,
	4: 7,
	5: 9,
	6: 10}

func intPow(base, exp int) int {
	result := 1
	for i := 0; i < exp; i++ {
		result *= base
	}
	return result
}

func intAbs(x int) int {
	if x >= 0 {
		return x
	} else {
		return -x
	}
}

func readInput23() (initState State) {

	for i := 0; i < numRooms; i++ {
		initState.rooms[i].slots = make([]rune, 2)
	}
	fmt.Println(initState)
	fmt.Scanf("#############\n")
	fmt.Scanf("#...........#\n")
	fmt.Scanf("###%c#%c#%c#%c###\n",
		&initState.rooms[0].slots[0],
		&initState.rooms[1].slots[0],
		&initState.rooms[2].slots[0],
		&initState.rooms[3].slots[0])
	fmt.Scanf("  #%c#%c#%c#%c#\n",
		&initState.rooms[0].slots[1],
		&initState.rooms[1].slots[1],
		&initState.rooms[2].slots[1],
		&initState.rooms[3].slots[1])
	fmt.Scanf("  #########\n")

	for i := range initState.hallway {
		initState.hallway[i] = empty
	}
	return
}

func minimum(list []int) (int, int) {
	if len(list) == 0 {
		panic("Trying to find minimum of an empty list!")
	}
	min := list[0]
	minIdx := 0
	for i := 1; i < len(list); i++ {
		if li := list[i]; li < min {
			min = li
			minIdx = i
		}
	}
	return min, minIdx
}

func isFinalState(state State) bool {
	for i, room := range state.rooms {
		for _, slot := range room.slots {
			if slot != rune('A'+i) {
				return false
			}
		}
	}
	return true
}

func getPossibleMoves(state State) (possibleMoves []Move) {
	// evaluate hallway -> room moves
	clearPath := func(hallPos, roomPos int) bool {
		// check if it's possible to move from hallPos to roomPos
		roomPos2hallPos := roomPos + 1
		if hallPos <= roomPos {
			for i := hallPos + 1; i <= roomPos2hallPos; i++ {
				if state.hallway[i] != empty {
					return false
				}
			}
		} else {
			for i := hallPos - 1; i > roomPos2hallPos; i-- {
				if state.hallway[i] != empty {
					return false
				}
			}
		}
		return true
	}

	restSolved := func(room Room, pos int, expectedContent rune) bool {
		for i := pos + 1; i < len(room.slots); i++ {
			if room.slots[i] != expectedContent {
				return false
			}
		}
		return true
	}

	for hallPos, hallContent := range state.hallway {
		if hallContent != empty {
			desiredRoomIdx := int(hallContent - 'A')
			desiredRoom := state.rooms[desiredRoomIdx]
			for depth := len(desiredRoom.slots) - 1; depth >= 0; depth-- { // try to move to bottom of room first
				if desiredRoom.slots[depth] == empty && clearPath(hallPos, desiredRoomIdx) && restSolved(desiredRoom, depth, hallContent) {
					possibleMoves = append(possibleMoves, Move{Pos{'h', hallPos, -1}, Pos{'r', desiredRoomIdx, depth}})
					break
				}
			}
		}
	}

	// evaluate room -> hallway moves
	findHallwayMoves := func(roomPos, roomDepth int) {
		// find empty hallways positions on the left of room
		for i := roomPos + 1; i >= 0; i-- {
			if state.hallway[i] == empty {
				possibleMoves = append(possibleMoves, Move{Pos{'r', roomPos, roomDepth}, Pos{'h', i, -1}})
			} else {
				// stop looking for spaces as soon as the hallway is busy
				break
			}
		}
		// find empty hallways positions on the right of room
		for i := roomPos + 2; i < numRooms+3; i++ {
			if state.hallway[i] == empty {
				possibleMoves = append(possibleMoves, Move{Pos{'r', roomPos, roomDepth}, Pos{'h', i, -1}})
			} else {
				// stop looking for spaces as soon as the hallway is busy
				break
			}
		}
	}

	prevEmpty := func(room Room, depth int) bool {
		for i := depth - 1; i >= 0; i-- {
			if room.slots[i] != empty {
				return false
			}
		}
		return true
	}

	roomNotReady := func(room Room, startDepth int, expectedValue rune) bool {
		for i := startDepth; i < len(room.slots); i++ {
			if room.slots[i] != expectedValue {
				return true
			}
		}
		return false
	}

	for roomPos, room := range state.rooms {
		for depth, roomValue := range room.slots {
			if roomValue != empty && prevEmpty(room, depth) && roomNotReady(room, depth, rune('A'+roomPos)) {
				findHallwayMoves(roomPos, depth)
			}
		}
	}

	return
}

func makeMove(state *State, move Move) (cost int) {
	// change state and compute cost
	switch move.from.ltype {
	case 'h': // hallway -> room move
		(*state).rooms[move.to.pos].slots[move.to.depth] = (*state).hallway[move.from.pos]
		(*state).hallway[move.from.pos] = empty
		vertDist := move.to.depth + 1
		costPerMove := intPow(10, move.to.pos)
		horizDist := intAbs((2 + move.to.pos*2) - hallIdx2Pos[move.from.pos])
		cost = costPerMove * (horizDist + vertDist)

	case 'r': // room -> hallway move
		(*state).hallway[move.to.pos] = (*state).rooms[move.from.pos].slots[move.from.depth]
		(*state).rooms[move.from.pos].slots[move.from.depth] = empty
		costPerMove := intPow(10, int((*state).hallway[move.to.pos]-'A'))
		vertDist := move.from.depth + 1
		horizDist := intAbs((2 + move.from.pos*2) - hallIdx2Pos[move.to.pos])
		cost = costPerMove * (horizDist + vertDist)
	}

	return cost
}

func searchAllPaths(state State, costSoFar int, moveChain []Move) int {
	// fmt.Println(state)
	if isFinalState(state) {
		// fmt.Println("final with cost", costSoFar)
		// fmt.Println(moveChain)
		return costSoFar
	}

	// freePieces := getFreePieces(state)
	possibleMoves := getPossibleMoves(state)
	if len(possibleMoves) == 0 {
		// fmt.Println("dead end")
		// fmt.Println(state)
		return math.MaxInt
	}

	allCosts := make([]int, 0, len(possibleMoves))
	for _, move := range possibleMoves {
		// fmt.Println(move)
		moveCost := makeMove(&state, move)
		pathCost := searchAllPaths(state, costSoFar+moveCost, append(moveChain, move))
		allCosts = append(allCosts, pathCost)
		// undo move to return state to original value
		makeMove(&state, Move{move.to, move.from})
	}

	minCost, _ := minimum(allCosts)
	// fmt.Println(possibleMoves[minMoveIdx])
	// fmt.Println(state)
	return minCost
}

func solve23(initState State) {
	shortestPath := searchAllPaths(initState, 0, []Move{})
	fmt.Println(shortestPath)
}

func main() {
	initState := readInput23()
	fmt.Println(initState)
	solve23(initState)
	var extendedState State
	for i := range initState.hallway {
		extendedState.hallway[i] = initState.hallway[i]
	}
	// #D#C#B#A#
	// #D#B#A#C#
	var extraSlots [2][4]rune
	extraSlots[0] = [4]rune{'D', 'C', 'B', 'A'}
	extraSlots[1] = [4]rune{'D', 'B', 'A', 'C'}

	for i := range initState.rooms {
		extendedState.rooms[i].slots = make([]rune, 4)
		extendedState.rooms[i].slots[0] = initState.rooms[i].slots[0]
		extendedState.rooms[i].slots[1] = extraSlots[0][i]
		extendedState.rooms[i].slots[2] = extraSlots[1][i]
		extendedState.rooms[i].slots[3] = initState.rooms[i].slots[1]
	}
	fmt.Println(extendedState)
	solve23(extendedState)
}
