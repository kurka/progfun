package main

import (
	"fmt"
	"io"
)

func readInput11() (grid [][]int) {
	var l string
	for {
		_, err := fmt.Scan(&l)
		if err == io.EOF {
			break
		}
		li := []int{}
		for _, c := range l {
			li = append(li, int(c)-48)
		}
		grid = append(grid, li)

	}
	return
}

func printMatrix(matrix [][]int) {
	fmt.Println("")
	for _, row := range matrix {
		fmt.Println(row)
	}
}

func max(a, b int) int {
	if a >= b {
		return a
	} else {
		return b
	}
}

func min(a, b int) int {
	if a <= b {
		return a
	} else {
		return b
	}
}

func solve11(grid [][]int) (hundredFlashes, allFlash int) {

	// create a grid just to store each turn's increments
	inc_grid := make([][]int, len(grid))
	flash_store := make([][]bool, len(grid))
	for i := range grid {
		inc_grid[i] = make([]int, len(grid[i]))
		flash_store[i] = make([]bool, len(grid[i]))
	}

	flashes := 0
	steps := 100
	for s := 0; ; s++ {
		// increment grid's scores
		for i := range grid {
			for j := range grid[i] {
				grid[i][j] += 1
			}
		}
		// flash octopuses
		for new_flash := true; new_flash; {
			new_flash = false
			// change flashes to 0 and store increments
			for i := range grid {
				for j := range grid[i] {
					if grid[i][j] > 9 && !flash_store[i][j] {
						// count flashes
						flashes += 1
						new_flash = true
						// loop over (i,j) neighbors and (i,j) itself
						for ii := max(i-1, 0); ii <= min(len(grid)-1, i+1); ii++ {
							for jj := max(j-1, 0); jj <= min(len(grid[i])-1, j+1); jj++ {
								if ii == i && jj == j {
									// store  cell that flashed
									flash_store[i][j] = true
								} else {
									// save increments
									inc_grid[ii][jj] += 1
								}
							}
						}
					}
				}
			}
			// apply increments
			for i := range grid {
				for j := range grid[i] {
					grid[i][j] += inc_grid[i][j]
					// reset inc_grid
					inc_grid[i][j] = 0
				}
			}
		}
		// check if flash_store is all true for problem 2
		allFlashed := true
		// clear flashed cells
		for i := range grid {
			for j := range grid[i] {
				if flash_store[i][j] {
					grid[i][j] = 0
					flash_store[i][j] = false
				} else {
					allFlashed = false
				}
			}
		}
		if s == steps {
			hundredFlashes = flashes
		}
		if allFlashed {
			allFlash = s + 1
			break
		}

		// fmt.Println(s)
		// printMatrix(grid)
	}
	return
}

func main() {
	grid := readInput11()
	fmt.Println(solve11(grid))
}
