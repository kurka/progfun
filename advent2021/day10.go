package main

import (
	"fmt"
	"io"
	"sort"
)

func readInput10() (lines []string) {
	var s string
	for {
		_, err := fmt.Scan(&s)
		if err == io.EOF {
			break
		}
		lines = append(lines, s)
	}
	return
}

func checkLine(line []rune, expectedChar rune) (points int, incomplete bool) {
	queue := make([]rune, 0)

	for _, c := range line {
		switch c {
		case '(':
			queue = append(queue, ')')
		case '[':
			queue = append(queue, ']')
		case '{':
			queue = append(queue, '}')
		case '<':
			queue = append(queue, '>')
		case ')', ']', '}', '>':
			expectedChar = queue[len(queue)-1]
			// dequeue
			queue = queue[:len(queue)-1]

			// if mismatch happened, compute score and return
			if expectedChar != c {
				switch c {
				case ')':
					points = 3
				case ']':
					points = 57
				case '}':
					points = 1197
				case '>':
					points = 25137
				}
				return
			}
		}
	}

	if len(queue) > 0 {
		incomplete = true
	}
	for i, value := len(queue)-1, 0; i >= 0; i-- {
		switch queue[i] {
		case ')':
			value = 1
		case ']':
			value = 2
		case '}':
			value = 3
		case '>':
			value = 4
		}
		points = 5*points + value
	}

	return
}

// O(n + nlogn) = O(nlogg) solution due to the sort. If a better data structure
// was used for storing the median (heaps), it would be O(n + logn) = O(n), but
// that's ok
func solve10(lines []string) (scoreCorrupted int, scoreIncomplete int) {
	scoresIncomplete := []int{}
	for i := range lines {
		strAsRuneLst := []rune(lines[i])
		points, incomplete := checkLine(strAsRuneLst, strAsRuneLst[0])
		if !incomplete {
			scoreCorrupted += points
		} else {
			scoresIncomplete = append(scoresIncomplete, points)
		}
	}

	// a more efficient solution would be to keep track of the median by using
	// two heaps... But I will leave like this for now
	sort.Sort(sort.IntSlice(scoresIncomplete))
	scoreIncomplete = scoresIncomplete[(len(scoresIncomplete)-1)/2]
	return
}


func main() {
	lines := readInput10()
	fmt.Println(solve10(lines))
}
