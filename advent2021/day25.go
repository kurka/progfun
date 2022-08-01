package main

import (
	"fmt"
)

func readInput25() [][]rune {

	var l string

	grid := make([][]rune, 0)
	scanLine := func() (err error) { _, err = fmt.Scan(&l); return}
	for err := scanLine(); err == nil; err = scanLine() {
		line := []rune(l)
		grid = append(grid, line)
	}

	return grid
}

func solve25(grid [][]rune) int {
	nRounds := 0
	nRows := len(grid)
	nCols := len(grid[0])

	for {

		interGrid := make([][]rune, nRows)
		nextGrid := make([][]rune, nRows)
		for i := 0; i < nRows; i++ {
			interGrid[i] = make([]rune, nCols)
			nextGrid[i] = make([]rune, nCols)
			for j := 0; j < nCols; j++ {
				interGrid[i][j] = '.'
				nextGrid[i][j] = '.'
			}
		}

		// first make the east movements from grid to interGrid
		nChanges := 0
		for i := 0; i < nRows; i++ {
			for j := 0; j < nCols; j++ {
				if grid[i][j] == '>' && grid[i][(j+1)%nCols] == '.' {
					interGrid[i][(j+1)%nCols] = '>'
					nChanges++
				} else if grid[i][j] != '.' {
					interGrid[i][j] = grid[i][j]
				}

			}
		}
		// then make the south movements from interGrid to nextGrid
		for i := 0; i < nRows; i++ {
			for j := 0; j < nCols; j++ {
				if interGrid[i][j] == 'v' && interGrid[(i+1)%nRows][j] == '.' {
					nextGrid[(i+1)%nRows][j] = 'v'
					nChanges++
				} else if interGrid[i][j] != '.' {
					nextGrid[i][j] = interGrid[i][j]
				}

			}
		}

		nRounds++
		if nChanges == 0 {
			break
		}

		grid = nextGrid
		// fmt.Println(nRounds, nChanges)

		// for i := 0; i < nRows; i++ {
		// 	for j := 0; j < nCols; j++ {
		// 		fmt.Printf("%c", grid[i][j])
		// 	}
		// 	fmt.Print("\n")
		// }
	}

	return nRounds
}



func main() {
	grid := readInput25()
	fmt.Println(solve25(grid))
}
