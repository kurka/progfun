package main

import (
	"fmt"
	"strings"
	"strconv"
)

func ReadInput6() map[int]int {
	var states string
	// first read the list of states
	fmt.Scan(&states)

	// then split it into integers and store the counts
	initstate := map[int]int{}
	for _, d := range strings.Split(states, ",") {
		state, _ := strconv.Atoi(d)
		initstate[state] += 1
		// state = append(initstate, state)
	}
	return initstate
}

func PrintMatrix(matrix [][]int) {
	fmt.Println("")
	for _, row := range matrix {
		fmt.Println(row)
	}
}

func ComputePopulation(start, freq, total_days int) (result int){
	result += freq // start result with initial population

	fmt.Println(start, freq)
	grid_size := 1 + (total_days - start - 1) / 7
	grid := make([][]int, grid_size)
	for i := 0; i < grid_size; i++ {
		grid[i] = make([]int, grid_size)
	}

    // fill first line
    for j := 0; j < grid_size; j++ {
		grid[0][j] = freq
	}

	for i := 1; i < grid_size; i++ {
		for j := i; j < grid_size; j++ {
			grid[i][j] = grid[i][j-1] + grid[i-1][j-1]
		}
	}
	PrintMatrix(grid)

	for i := 0; i < grid_size; i++ {
		for j := 0; j < grid_size; j++ {
			result += grid[i][j]
		}
	}

	fmt.Println(result)
	return
}

func solve6(states map[int]int, total_days int) (result int){
	inc_per_day := make([]int, total_days)

	// fill the fishes created by the original population
	for start, freq := range states {
		result += freq
		for i := start; i < total_days; i += 7 {
			inc_per_day[i] += freq

		}
	}

	// starting from day 0, filll future fishes for each fish encoutered
	for i := 0; i < total_days; i++ {
		if inc_per_day[i] > 0 {
			for j := i+9; j < total_days; j += 7 {
				inc_per_day[j] += inc_per_day[i]
				}
		}
	}

	for _, v := range inc_per_day {
		result += v
	}

	return
}

func main() {
	initial_state := ReadInput6()
	fmt.Println(solve6(initial_state, 80))
	fmt.Println(solve6(initial_state, 256))

}
