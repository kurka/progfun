package main

import (
	"fmt"
	"io"
)

type Coord struct {
	x, y int
}


type Line struct {
	x1, y1, x2, y2 int
}

func Direction(delta int) (d int) {
	// analyse the magnitude of a delta and return a value in {-1, 0, 1}
	switch {
	case delta > 0:
		d = 1
	case delta < 0:
		d = -1
	case delta == 0:
		d = 0
	}
	return
}

func PrintMatrix(matrix [][]int) {
	fmt.Println("")
	for _, row := range matrix {
		fmt.Println(row)
	}
}

func ReadInput5() (lines []Line) {
	var l Line
	for {
		_, err := fmt.Scanf("%d,%d -> %d,%d", &l.x1, &l.y1, &l.x2, &l.y2)

		if err == io.EOF {
			break
		}
		lines = append(lines, l)
	}
	return
}

// O(nm + n^2) solution, where n is the grid's side, and m the number of lines
func Solve5(lines []Line, skipdiagonals bool) (overlaps int) {
	// find grid limits
	var maxX, maxY int
	for _, l := range lines {
		if l.x1 > maxX {
			maxX = l.x1
		}
		if l.x2 > maxX {
			maxX = l.x2
		}
		if l.y1 > maxY {
			maxY = l.y1
		}
		if l.x2 > maxY {
			maxY = l.y2
		}
	}

	grid := make([][]int, maxY+1)
	for i := 0; i < len(grid); i++ {
		grid[i] = make([]int, maxX+1)
	}

	for _, l := range lines {
		// skip diagonal lines for part 1
		if skipdiagonals && !(l.x1 == l.x2 || l.y1 == l.y2) {
			continue
		}

		// find the direction between (x1,y1) and (x2,y2)
		dir := Coord{Direction(l.x2 - l.x1), Direction(l.y2 - l.y1)}

		// traverse a path from x1,y1 to x2,y2, filling the grid on the way
		for p := (Coord{l.x1, l.y1}); p.x != l.x2 || p.y != l.y2; p = (Coord{p.x + dir.x, p.y + dir.y}) {
			grid[p.y][p.x]++
		}
		// add 1 to final point too
		grid[l.y2][l.x2]++

	}

	// printMatrix(grid)
	for i := 0; i <= maxY; i++ {
		for j := 0; j <= maxX; j++ {
			if grid[i][j] > 1 {
				overlaps++
			}

		}
	}
	return
}

func main() {
	lines := ReadInput5()
	fmt.Println(Solve5(lines, true))
	fmt.Println(Solve5(lines, false))
}
