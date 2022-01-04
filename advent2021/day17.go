package main

import (
	"fmt"
)

func readInput17() (x1, x2, y1, y2 int) {
	fmt.Scanf("target area: x=%d..%d, y=%d..%d", &x1, &x2, &y1, &y2)
	return
}

func maximum(a,b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}
func checkValidSpeed(x, y, x1, x2, y1, y2 int) int {
	// check if an initial speed (x,y) falls within (x1,y1)-(x2,y2) rectangle

	sx := x
	sy := y
	x, y = 0, 0
	for x <= x2 && y >= y1 {
		if x >= x1 && y <= y2 {
			return 1
		}
		x += sx
		y += sy
		sx = maximum(sx-1, 0)
		sy -= 1

	}
	return 0
}

func solve17(x1, x2, y1, y2 int) {
	// Since the probe makes a parable starting in (0,0). a probe going up with
	// starting y speed will peak, and then go down until it has (-y+1) speed
	// when crossing the (_, 0) line downards. As we need to touch the target
	// area, the maximum value of (-y+1) should be equal to the lower end of
	// target (y1), thus y = -y1-1.
	// NOTE: assuming that y1 < y2 and y1 < 0
	maxYSpeed := -y1 - 1

	// part 1: compute maxHeight. This is just the sum(maxYspeed..1)
	maxHeight := 0
	for i := maxYSpeed; i > 0; i-- {
		maxHeight += i
	}
	fmt.Println(maxHeight)

	// part B
	// min y speed is equal to the upper limit of target
	minYSpeed := y1

	maxXSpeed := x2
	minXSpeed := 0
	cumSpeed := 0
	for i := 1; cumSpeed < x1; i++ {
		cumSpeed += i
		minXSpeed = i
	}

	// for each possible (x,y) speed, check if it reaches the target correctly
	speeds := 0
	for y := minYSpeed; y <= maxYSpeed; y++ {
		for x := minXSpeed; x <= maxXSpeed; x++ {
			speeds += checkValidSpeed(x, y, x1, x2, y1, y2)
		}
	}
	fmt.Println(speeds)

}

func main() {
	x1, x2, y1, y2 := readInput17()
	solve17(x1, x2, y1, y2)
}
