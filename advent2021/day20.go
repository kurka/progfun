package main

import (
	"fmt"
)

func readInput20() ([]rune, [][]int) {
	var l string

	// first read the algorithm line
	fmt.Scanln(&l)
	algorithm := []rune(l)

	// then read the grid, and store it in a dense matrix
	grid := make([][]int, 0)
	scanLine := func() (err error) { _, err = fmt.Scan(&l); return }
	for err, i := scanLine(), 0; err == nil; err, i = scanLine(), i+1 {
		line := make([]int, len(l))
		for j, c := range l {
			if c == '#' {
				line[j] = 1
			}
		}
		grid = append(grid, line)
	}

	return algorithm, grid
}

func solve20(algorithm []rune, grid [][]int) {

	var default_value int
	rounds := 50
	for r := 0; r < rounds; r++ {

		// define a default value for the coordinates outside the grid,
		// depending on the value of the rule 0, and the current round
		switch algorithm[0] {
		case '.':
			default_value = 0
		case '#':
			if r%2 == 0 { // even rounds
				default_value = 0
			} else { // odd rounds
				default_value = 1
			}
		}

		nRows := len(grid)
		nCols := len(grid[0])
		nextGrid := make([][]int, nRows+2)
		for i := 0; i < nRows+2; i++ {
			nextGrid[i] = make([]int, nCols+2)
		}

		total_on := 0
		for i := range nextGrid {
			for j := range nextGrid[i] {
				// find code
				code := 0
				for ii := i - 1; ii <= i+1; ii++ {
					for jj := j - 1; jj <= j+1; jj++ {
						code *= 2
						if ii < 1 || ii > nRows || jj < 1 || jj > nCols {
							code += default_value
						} else {
							code += grid[ii-1][jj-1]
						}
					}
				}

				if nextChar := algorithm[code]; nextChar == '#' {
					nextGrid[i][j] = 1
					total_on++
				}
			}
		}

		grid = nextGrid

		if r == 1 || r == 49 {
			fmt.Println(total_on)
		}

	}
}

func main() {
	alg, grid := readInput20()
	// fmt.Println(alg)
	// fmt.Println(len(alg))
	// fmt.Println(grid)
	solve20(alg, grid)
}
