package main

import (
	"fmt"
	"math"
)

type Coord struct {
	x,y int
}

func readInput20() ([]rune, map[Coord]struct{}) {
	var l string
	grid := make(map[Coord]struct{}, 0)

	// first read the algorithm line
	fmt.Scanln(&l)
	algorithm := []rune(l)

	scanLine := func() (err error) { _, err = fmt.Scan(&l); return }
	for err, i := scanLine(), 0; err == nil; err, i = scanLine(), i+1 {
		for j, c := range l {
			if c == '#' {
				grid[Coord{i,j}] = struct{}{}
			}
		}
	}

	return algorithm, grid
}

func solve20(algorithm []rune, grid map[Coord]struct{}) {
	// minX := fmt.print
	minX, maxX, minY, maxY := math.MaxInt, math.MinInt, math.MaxInt, math.MinInt
	for c := range grid {
		if c.x < minX {
			minX = c.x
		}
		if c.x > maxX {
			maxX = c.x
		}
		if c.y < minY {
			minY = c.y
		}
		if c.y > maxY {
			maxY = c.y
		}
	}

	rounds := 2
	for r := 0; r < rounds; r++ {
		nextGrid := make(map[Coord]struct{}, len(grid))
		for i := minX-2; i <= maxX; i++ {
			fmt.Println("done row", i, "/", maxX)
			for j := minY-2; j <= maxY; j++ {
				// find code
				code := 0
				for ii := i; ii < i+3; ii++ {
					for jj := j; jj < j+3; jj++ {
						code *= 2
						if _, ok := grid[Coord{ii, jj}]; ok {
							code += 1
						}
					}
				}

				if nextChar := algorithm[code]; nextChar == '#' {
					nextGrid[Coord{i+1,j+1}] = struct{}{}
					if i+1 < minX {
						minX = i+1
					}
					if i+1 > maxX {
						maxX = i+1
					}
					if j+1 < minY {
						minY = j+1
					}
					if j+1 > maxY {
						maxY = j+1
					}
				}
			}
		}
		grid = nextGrid
		fmt.Println(len(grid))
		// fmt.Println(grid)

	}
	fmt.Println(len(grid))
}

func main() {
	alg, grid := readInput20()
	// fmt.Println(alg)
	// fmt.Println(len(alg))
	// fmt.Println(grid)
	solve20(alg, grid)
}
