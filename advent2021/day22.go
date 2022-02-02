package main

import (
	"fmt"
)

type RebootStep struct {
	instruction            string
	x1, x2, y1, y2, z1, z2 int
}

type Pair struct {
	a, b int
}

func readInput22() (steps []RebootStep) {
	var rs RebootStep
	scanLine := func() (err error) {
		_, err = fmt.Scanf("%s x=%d..%d,y=%d..%d,z=%d..%d",
			&rs.instruction, &rs.x1, &rs.x2, &rs.y1, &rs.y2, &rs.z1, &rs.z2)
		return
	}
	for err := scanLine(); err == nil; err = scanLine() {
		steps = append(steps, rs)
	}

	return
}

func max(a, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

func min(a, b int) int {
	if a < b {
		return a
	} else {
		return b
	}
}

func removeStep(steps []RebootStep, step RebootStep) (newSteps []RebootStep) {
	newSteps = make([]RebootStep, 0, len(steps)-1)
	for i := range steps {
		if steps[i] == step {
			continue
		}
		newSteps = append(newSteps, steps[i])
	}
	return
}

func solve22a(steps []RebootStep) {
	var grid [101][101][101]int

	for _, step := range steps {

		x1 := max(step.x1, -50)
		x2 := min(step.x2, 50)
		y1 := max(step.y1, -50)
		y2 := min(step.y2, 50)
		z1 := max(step.z1, -50)
		z2 := min(step.z2, 50)

		// fmt.Println(step)
		// fmt.Println(step.instruction, x1, x2, y1, y2, z1, z2)

		for x := x1; x <= x2; x++ {
			for y := y1; y <= y2; y++ {
				for z := z1; z <= z2; z++ {
					var val int
					switch step.instruction {
					case "on":
						val = 1
					case "off":
						val = 0
					}
					grid[x+50][y+50][z+50] = val
				}
			}
		}
	}

	sum := 0
	for x := range grid {
		for y := range grid[x] {
			for z := range grid[x][y] {
				sum += grid[x][y][z]
			}
		}
	}

	fmt.Println(sum)
}

func hasIntersection(step RebootStep, steps []RebootStep) (inter RebootStep, found bool) {
	// {on -20 26 -21 17 7 28} {on -20 26 -36 17 -47 7}
	//
	for _, rs := range steps {
		interX := !((step.x1 < rs.x1 && step.x2 < rs.x1) || (step.x1 > rs.x2 && step.x2 > rs.x2))
		interY := !((step.y1 < rs.y1 && step.y2 < rs.y1) || (step.y1 > rs.y2 && step.y2 > rs.y2))
		interZ := !((step.z1 < rs.z1 && step.z2 < rs.z1) || (step.z1 > rs.z2 && step.z2 > rs.z2))

		if interX && interY && interZ {
			found = true
			inter = rs
			break
		}
	}
	return
}

func splitIntersection(newStep, existingStep RebootStep) (splits []RebootStep) {

	// {on -20 33 -21 23 -26 28} {on -20 26 -36 17 -47 7}
	// inter: {off, -20, 26, -21, 17, -26, 7}
	intersection := RebootStep{instruction: "off",
		x1: max(newStep.x1, existingStep.x1), x2: min(newStep.x2, existingStep.x2),
		y1: max(newStep.y1, existingStep.y1), y2: min(newStep.y2, existingStep.y2),
		z1: max(newStep.z1, existingStep.z1), z2: min(newStep.z2, existingStep.z2)}

	switch newStep.instruction {
	case "on":
		// equivalent to remove the intersection from newStep
		splits = removeCuboid(intersection, newStep)
	case "off":
		splits = removeCuboid(intersection, existingStep)
	}
	return splits
}

func removeCuboid(inner, outer RebootStep) (splits []RebootStep) {
	// inner: {off -20 26 -21 17 -26 7}
	// outer: {on -20 33 -21 23 -26 28}
	// xOptions: -20/26 26/33
	splits = []RebootStep{}

	xOptions := func() (options []Pair) {
		if outer.x1 < inner.x1 {
			options = append(options, Pair{outer.x1, inner.x1 - 1})
		}

		if inner.x1 <= inner.x2 {
			options = append(options, Pair{inner.x1, inner.x2})
		}

		if inner.x2 < outer.x2 {
			options = append(options, Pair{inner.x2 + 1, outer.x2})
		}
		return
	}

	yOptions := func() (options []Pair) {
		if outer.y1 < inner.y1 {
			options = append(options, Pair{outer.y1, inner.y1 - 1})
		}

		if inner.y1 <= inner.y2 {
			options = append(options, Pair{inner.y1, inner.y2})
		}

		if inner.y2 < outer.y2 {
			options = append(options, Pair{inner.y2 + 1, outer.y2})
		}
		return
	}

	zOptions := func() (options []Pair) {
		if outer.z1 < inner.z1 {
			options = append(options, Pair{outer.z1, inner.z1 - 1})
		}

		if inner.z1 <= inner.z2 {
			options = append(options, Pair{inner.z1, inner.z2})
		}

		if inner.z2 < outer.z2 {
			options = append(options, Pair{inner.z2 + 1, outer.z2})
		}
		return
	}
	// inner: {off -20 26 -21 17 -26 7}
	// outer: {on -20 33 -21 23 -26 28}
	// xOptions: -20/26 26/33
	//
	//
	//        -20       26          33
	//  -21
	//
	//  17
	//
	//  23
	//

	for _, optionX := range xOptions() {
		for _, optionY := range yOptions() {
			for _, optionZ := range zOptions() {
				if optionX.a == inner.x1 && optionX.b == inner.x2 &&
					optionY.a == inner.y1 && optionY.b == inner.y2 &&
					optionZ.a == inner.z1 && optionZ.b == inner.z2 {
					continue
				}

				splits = append(splits, RebootStep{"on",
					optionX.a, optionX.b, optionY.a, optionY.b,
					optionZ.a, optionZ.b})
			}
		}
	}

	return
}

func solve22(steps []RebootStep, limit bool) {
	regions := make([]RebootStep, 0, len(steps))

	for _, step := range steps {
		var normStep RebootStep
		if limit { // part A
			x1 := max(step.x1, -50)
			x2 := min(step.x2, 50)
			y1 := max(step.y1, -50)
			y2 := min(step.y2, 50)
			z1 := max(step.z1, -50)
			z2 := min(step.z2, 50)
			if x1 > x2 || y1 > y2 || z1 > z2 {
				continue
			}
			normStep = RebootStep{step.instruction, x1, x2, y1, y2, z1, z2}
		} else {
			normStep = step
		}

		subSteps := []RebootStep{normStep}
		// fmt.Println("kNEW:", normStep)
		for len(subSteps) > 0 {
			newSteps := make([]RebootStep, 0)

			for _, subStep := range subSteps {
				if inter, ok := hasIntersection(subStep, regions); ok {
					splits := splitIntersection(subStep, inter)
					if subStep.instruction == "off" {
						// remove inter from regions
						regions = removeStep(regions, inter)
						newSteps = append(newSteps, subStep)
					}
					newSteps = append(newSteps, splits...)

				} else {
					if subStep.instruction == "on" {
						regions = append(regions, subStep)
					}
				}
			}
			subSteps = newSteps
		}
	}
	// compute final sum
	sum := 0
	for _, region := range regions {
		sum += (region.x2 - region.x1 + 1) * (region.y2 - region.y1 + 1) * (region.z2 - region.z1 + 1)
	}
	fmt.Println(sum)
}

func main() {
	steps := readInput22()
	solve22(steps, true)
	solve22(steps, false)
}
