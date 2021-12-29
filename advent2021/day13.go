package main

import (
	"fmt"
)

type Coord struct {
	x, y int
}

type Fold struct {
	direction rune
	line      int
}

func readInput13() (dots map[Coord]struct{}, folds []Fold) {
	dots = make(map[Coord]struct{})
	var coord Coord
	scanCoord := func() (err error) { _, err = fmt.Scanf("%d,%d", &coord.x, &coord.y); return }
	for err := scanCoord(); err == nil; err = scanCoord() {
		dots[coord] = struct{}{}
	}

	var fold Fold
	scanFold := func() (err error) { _, err = fmt.Scanf("fold along %c=%d", &fold.direction, &fold.line); return }
	for err := scanFold(); err == nil; err = scanFold() {
		folds = append(folds, fold)
	}

	return
}

func solve13(dots map[Coord]struct{}, folds []Fold) (sizeFirstFold int) {
	var new_coord Coord
	var new_val int
	firstFold := true

	for _, fold := range folds {
		new_dots := make(map[Coord]struct{})

		for old_coord := range dots {
			switch fold.direction {
			case 'x':
				if old_coord.x <= fold.line {
					// keep value as it is
					new_val = old_coord.x
				} else {
					// reflect value to within the fold
					new_val = fold.line - (old_coord.x - fold.line)
				}
				new_coord = Coord{new_val, old_coord.y}
			case 'y':
				if old_coord.y <= fold.line {
					// keep value as it is
					new_val = old_coord.y
				} else {
					// reflect value to within the fold
					new_val = fold.line - (old_coord.y - fold.line)
				}
				new_coord = Coord{old_coord.x, new_val}
			}
			// using map as set, to eliminate duplicated values
			new_dots[new_coord] = struct{}{}
		}
		dots = new_dots
		if firstFold {
			sizeFirstFold = len(dots)
			firstFold = false
		}
	}

	// print the paper so we can see the code
	maxX, maxY := 0, 0
	for coord := range dots {
		if coord.x > maxX {
			maxX = coord.x
		}
		if coord.y > maxY {
			maxY = coord.y
		}
	}

	for y := 0; y <= maxY; y++ {
		for x := 0; x <= maxX; x++ {
			if _, ok := dots[Coord{x, y}]; ok {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Print("\n")
	}
	return

}

func main() {
	dots, folds := readInput13()
	fmt.Println(solve13(dots, folds))
}
