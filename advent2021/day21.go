package main

import (
	"fmt"
)

func readInput21() (s1, s2 int) {
	fmt.Scanf("Player 1 starting position: %d", &s1)
	fmt.Scanf("Player 2 starting position: %d", &s2)
	return
}

const fieldSize, dieSize, finalScoreA, finalScoreB = 10, 100, 1000, 21

func solve21a(s1, s2 int) {
	score1, score2 := 0, 0

	last_roll, rolls := -1, 0
	pos1, pos2 := s1-1, s2-1

	for {
		if score1 >= finalScoreA {
			fmt.Println(score2 * rolls)
			break
		}
		if score2 >= finalScoreA {
			fmt.Println(score1 * rolls)
		}

		// if last_roll <
		roll := ((last_roll + 1) % dieSize) + 1 + ((last_roll + 2) % dieSize) + 1 + ((last_roll + 3) % dieSize) + 1
		last_roll = (last_roll + 3) % dieSize

		switch rolls % 2 {
		case 0:
			pos1 = (pos1 + roll) % fieldSize
			score1 += pos1 + 1
		case 1:
			pos2 = (pos2 + roll) % fieldSize
			score2 += pos2 + 1
		}
		rolls += 3
	}
}

func solve21b(s1, s2 int) {
	a, b := countQuantStates(0, 0, s1-1, s2-1, 0)
	fmt.Println(a, b)
	return

}

func countQuantStates(scoreA, scoreB, posA, posB, round int) (winsA, winsB int) {
	if scoreA >= finalScoreB {
		winsA = 1
		winsB = 0
		return
	}
	if scoreB >= finalScoreB {
		winsA = 0
		winsB = 1
		return
	}

	diceFreqs := []int{1, 3, 6, 7, 6, 3, 1}
	diceSums := []int{3, 4, 5, 6, 7, 8, 9}

	for i := range diceFreqs {
		var newPosA, newScoreA, newPosB, newScoreB int
		switch round % 2 {
		case 0:
			newPosA = (posA + diceSums[i]) % fieldSize
			newScoreA = scoreA + newPosA + 1
			newPosB = posB
			newScoreB = scoreB
		case 1:
			newPosA = posA
			newScoreA = scoreA
			newPosB = (posB + diceSums[i]) % fieldSize
			newScoreB = scoreB + newPosB + 1
		}
		futureWinsA, futureWinsB := countQuantStates(newScoreA, newScoreB, newPosA, newPosB, round+1)
		winsA += futureWinsA * diceFreqs[i]
		winsB += futureWinsB * diceFreqs[i]
	}
	return
}

func main() {
	s1, s2 := readInput21()
	solve21a(s1, s2)
	solve21b(s1, s2)
}
