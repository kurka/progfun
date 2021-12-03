package main

import (
	"fmt"
	"io"
)

type Instruction struct {
	direction string
	amount    int
}

func readinput() (arr []Instruction) {
	var i Instruction
	for {
		_, err := fmt.Scanf("%s %d", &i.direction, &i.amount)
		if err == io.EOF {
			break
		}
		arr = append(arr, i)
	}
	return
}

func solve1(arr []Instruction) int {
	var x, y = 0, 0
	for _, i := range arr {
		switch i.direction {
		case "forward":
			x += i.amount
		case "down":
			y += i.amount
		case "up":
			y -= i.amount
		default:
			panic("unrecognized direction!")
		}

	}
	return x * y
}

func solve2(arr []Instruction) int {
	var x, y, aim = 0, 0, 0
	for _, i := range arr {
		switch i.direction {
		case "forward":
			x += i.amount
			y += i.amount * aim
		case "down":
			aim += i.amount
		case "up":
			aim -= i.amount
		default:
			panic("unrecognized direction!")
		}

	}
	return x * y
}
func main() {
	arr := readinput()
	fmt.Println(solve1(arr))
	fmt.Println(solve2(arr))
}
