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
	hallway := fmt.Sprintf("#%c%c.%c.%c.%c.%c%c#", s.hallway[0], s.hallway[1], s.hallway[2], s.hallway[3], s.hallway[4], s.hallway[5], s.hallway[6])
	rooms1 := fmt.Sprintf("###%c#%c#%c#%c###", s.rooms[0].fst, s.rooms[1].fst, s.rooms[2].fst, s.rooms[3].fst)
	rooms2 := fmt.Sprintf("###%c#%c#%c#%c###", s.rooms[0].snd, s.rooms[1].snd, s.rooms[2].snd, s.rooms[3].snd)
	return fmt.Sprintf("%s\n%s\n%s\n", hallway, rooms1, rooms2)
}

func (m Move) String() string {
	return fmt.Sprintf("M(%v -> %v)", m.from, m.to)
}

type Room struct {
	fst, snd rune
}

type Move struct {
	from, to Pos
}

type Pos struct {
	ltype string
	pos   int
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

	fmt.Println(initState)
	fmt.Scanf("#############\n")
	fmt.Scanf("#...........#\n")
	fmt.Scanf("###%c#%c#%c#%c###\n",
		&initState.rooms[0].fst,
		&initState.rooms[1].fst,
		&initState.rooms[2].fst,
		&initState.rooms[3].fst)
	fmt.Scanf("  #%c#%c#%c#%c#\n",
		&initState.rooms[0].snd,
		&initState.rooms[1].snd,
		&initState.rooms[2].snd,
		&initState.rooms[3].snd)
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
		if room.fst != rune('A'+i) || room.snd != rune('A'+i) {
			return false
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

	for hallPos, hallContent := range state.hallway {
		if hallContent != empty {
			desiredRoomIdx := int(hallContent - 'A')
			desiredRoom := state.rooms[desiredRoomIdx]
			if clearPath(hallPos, desiredRoomIdx) && desiredRoom.snd == empty { // try to move to bottom of room first
				possibleMoves = append(possibleMoves, Move{Pos{"h", hallPos}, Pos{"r2", desiredRoomIdx}})
			} else if clearPath(hallPos, desiredRoomIdx) && desiredRoom.snd == hallContent && desiredRoom.fst == empty { // or to first part
				possibleMoves = append(possibleMoves, Move{Pos{"h", hallPos}, Pos{"r1", desiredRoomIdx}})
			}
		}
	}

	// evaluate room -> hallway moves
	findHallwayMoves := func(roomPos int, roomType string) {
		// find empty hallways positions on the left of room
		for i := roomPos + 1; i >= 0; i-- {
			if state.hallway[i] == empty {
				possibleMoves = append(possibleMoves, Move{Pos{roomType, roomPos}, Pos{"h", i}})
			} else {
				// stop looking for spaces as soon as the hallway is busy
				break
			}
		}
		// find empty hallways positions on the right of room
		for i := roomPos + 2; i < numRooms+3; i++ {
			if state.hallway[i] == empty {
				possibleMoves = append(possibleMoves, Move{Pos{roomType, roomPos}, Pos{"h", i}})
			} else {
				// stop looking for spaces as soon as the hallway is busy
				break
			}
		}
	}

	for roomPos, room := range state.rooms {
		if room.fst != empty && (room.fst != rune('A'+roomPos) || room.snd != rune('A'+roomPos)) {
			findHallwayMoves(roomPos, "r1")
		} else if room.fst == empty && room.snd != empty && room.snd != rune('A'+roomPos) {
			findHallwayMoves(roomPos, "r2")
		}
	}

	return
}

func makeMove(state *State, move Move) (cost int) {
	// change state and compute cost
	switch move.from.ltype {
	case "h": // hallway -> room move
		var vertDist int
		switch move.to.ltype {
		case "r1":
			(*state).rooms[move.to.pos].fst = (*state).hallway[move.from.pos]
			vertDist = 1
		case "r2":
			(*state).rooms[move.to.pos].snd = (*state).hallway[move.from.pos]
			vertDist = 2
		}
		(*state).hallway[move.from.pos] = empty
		costPerMove := intPow(10, move.to.pos)
		horizDist := intAbs((2 + move.to.pos*2) - hallIdx2Pos[move.from.pos])
		cost = costPerMove * (horizDist + vertDist)

	case "r1": // room.fst -> hallway move
		(*state).hallway[move.to.pos] = (*state).rooms[move.from.pos].fst
		(*state).rooms[move.from.pos].fst = empty
		costPerMove := intPow(10, int((*state).hallway[move.to.pos]-'A'))
		horizDist := intAbs((2 + move.from.pos*2) - hallIdx2Pos[move.to.pos])
		cost = costPerMove * (horizDist + 1)
	case "r2": // room.snd -> hallway move
		(*state).hallway[move.to.pos] = (*state).rooms[move.from.pos].snd
		(*state).rooms[move.from.pos].snd = empty
		costPerMove := intPow(10, int((*state).hallway[move.to.pos]-'A'))
		horizDist := intAbs((2 + move.from.pos*2) - hallIdx2Pos[move.to.pos])
		cost = costPerMove * (horizDist + 2)
	}

	return cost
}

func searchAllPaths(state State, costSoFar int, moveChain []Move) int {
	// fmt.Println(state)
	if isFinalState(state) {
		// fmt.Println("final with cost", costSoFar)
		// fmt.Println(state)
		if costSoFar == 8519 {
			fmt.Println(moveChain)
		}
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
}
