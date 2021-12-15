package main

import (
	"fmt"
	"io"
	"sort"
	"strconv"
)

func readinput() (arr []string) {
	var i string
	for {
		_, err := fmt.Scan(&i)
		if err == io.EOF {
			break
		}
		arr = append(arr, i)
	}
	return
}

// O(nk) solution (where n is the len of array, k the size of each string)
func solve1(arr []string) uint {
	counter := map[int]int{}
	for _, s := range arr {
		for i, c := range s {
			if c == '1' {
				counter[i] += 1
			}
		}
	}
	n := len(arr)
	var gamma uint
	var epsilon uint

	for i := 0; i < len(counter); i++ {
		gamma <<= 1
		epsilon <<= 1
		switch 2*counter[i] >= n {
		case true:
			gamma += 1
		case false:
			epsilon += 1
		}
	}

	return gamma * epsilon
}

// O(nlogn + nk) solution (where n is the len of array, k the size of each string)
func solve2(arr []string) int64 {
	// sort array (O(nlogn))
	sort.Strings(arr)
	lim_low, lim_high := 0, len(arr)

	// O(k*n) solution, but easily adapted to become O(k*log(n))
	for i := 0; i < len(arr[0]); i++ {
		if lim_low+1 == lim_high {
			break
		}
		// search for transition between 0 and 1
		// TODO: can use binary search instead of linear here
		for j, s := range arr[lim_low:lim_high] {
			if s[i] == '1' {
				switch 2*j > (lim_high - lim_low) {
				case true:
					lim_high = lim_low + j
				case false:
					lim_low += j
				}
				break
			}
		}
	}
	oxygen, _ := strconv.ParseInt(arr[lim_low], 2, 64)

	lim_low, lim_high = 0, len(arr)
	for i := 0; i < len(arr[0]); i++ {
		if lim_low+1 == lim_high {
			break
		}
		// search for transition between 0 and 1
		// TODO: can use binary search instead of linear here
		for j, s := range arr[lim_low:lim_high] {
			if s[i] == '1' {
				switch 2*j > (lim_high - lim_low) {
				case true:
					lim_low += j
				case false:
					lim_high = lim_low + j
				}
				break
			}
		}
	}
	co2, _ := strconv.ParseInt(arr[lim_low], 2, 64)
	return oxygen * co2
}

func main() {
	arr := readinput()
	fmt.Println(solve1(arr))
	fmt.Println(solve2(arr))
}
