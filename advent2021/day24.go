package main

import (
	"fmt"
	"strconv"
)

type Instruction struct {
	op, a, b string
}

type Tuple struct {
	a, b int
}

func readInput24() (instructions [][]Instruction) {
	var i Instruction
	scanInp := func() (err error) {
		_, err = fmt.Scanf("%s %s", &i.op, &i.a)
		i.b = ""
		return
	}

	scanLine := func() (err error) {
		_, err = fmt.Scanf("%s %s %s", &i.op, &i.a, &i.b)
		return
	}

	for err := scanInp(); err == nil; err = scanInp() {
		nInstructions := 18
		instructionsSet := make([]Instruction, nInstructions)
		instructionsSet[0] = i
		for j := 0; j < nInstructions-1; j++ {
			scanLine()
			instructionsSet[j+1] = i
		}
		instructions = append(instructions, instructionsSet)
	}

	return
}

func solve24(instructions [][]Instruction) {
	registers := map[string]int{"w": 0, "x": 0, "y": 0, "z": 0}
	fmt.Println(registers)
	// ws := [14]int{1, 3, 5, 7, 9, 2, 4, 6, 8, 9, 9, 9, 9, 9}
	ws := [14]int{9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}
	count := 0
	for {
		for i, instructionsSet := range instructions {
			// registers["z"] = prev_z
			for _, instruct := range instructionsSet {
				ra := registers[instruct.a]
				var rb int
				switch instruct.b {
				case "w", "x", "y", "z":
					rb = registers[instruct.b]
				default:
					rb, _ = strconv.Atoi(instruct.b)
				}
				switch instruct.op {
				case "inp":
					registers[instruct.a] = ws[i]
				case "add":
					registers[instruct.a] = ra + rb
				case "mul":
					registers[instruct.a] = ra * rb
				case "div":
					registers[instruct.a] = ra / rb
				case "mod":
					registers[instruct.a] = ra % rb
				case "eql":
					if ra == rb {
						registers[instruct.a] = 1
					} else {
						registers[instruct.a] = 0
					}
				}
			}
		}

		if count%1000000 == 0 {
			fmt.Println(ws)
			fmt.Println(registers)
		}
		count++
		if registers["z"] == 0 {
			break
		}
		for i := len(ws) - 1; i >= 0; i-- {
			ws[i] -= 1
			if ws[i] == 0 {
				ws[i] = 9
			} else {
				break
			}
		}
	}

}

func main() {
	instructions := readInput24()
	solve24(instructions)
}
