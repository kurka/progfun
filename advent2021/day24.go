package main

import (
	"fmt"
	"strconv"
)

type ALUInstruction struct {
	op, a, b string
}

type Tuple struct {
	a, b int
}

func readInput24() (instructions [][]ALUInstruction) {
	var i ALUInstruction
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
		instructionsSet := make([]ALUInstruction, nInstructions)
		instructionsSet[0] = i
		for j := 0; j < nInstructions-1; j++ {
			scanLine()
			instructionsSet[j+1] = i
		}
		instructions = append(instructions, instructionsSet)
	}

	return
}

func runInstructionsSetFast(prev_z, w, r1, r2, r3 int) int {
	var t int
	if ((prev_z % 26) + r2) != w {
		t = 1
	} else {
		t = 0
	}

    return (prev_z / r1) * ((25 * t) + 1) + ((w + r3) * t)
}

func runInstructionsSet(pregisters *map[string]int, instructionsSet []ALUInstruction, prev_z, w int) int {
	registers := *pregisters
	registers["z"] = prev_z
	// fmt.Println(registers)
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
			registers[instruct.a] = w
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
		// fmt.Println(instruct, registers)
	}
	return registers["z"]

}

func solve24BruteForce(instructions [][]ALUInstruction) {
	registers := map[string]int{"w": 0, "x": 0, "y": 0, "z": 0}
	fmt.Println(registers)
	ws := [14]int{1, 3, 5, 7, 9, 2, 4, 6, 8, 9, 9, 9, 9, 9}
	// ws := [14]int{9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}
	count := 0
	for {
		for i, instructionsSet := range instructions {
			// registers["z"] = prev_z
			fmt.Println(i, ws[i])
			fmt.Println(registers)
			runInstructionsSet(&registers, instructionsSet, registers["z"], ws[i])
			fmt.Println(registers)
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


func solve24Backwards(instructions [][]ALUInstruction, reverse bool) int {
	n_instructions := len(instructions)
	possible_zs := make([]map[int]int, n_instructions+1)
	possible_zs[n_instructions] = map[int]int{0: -1}
	for i := n_instructions-1; i >= 0; i-- {
		z_ins := map[int]int{}
		r1, _ := strconv.Atoi(instructions[i][4].b)
		r2, _ := strconv.Atoi(instructions[i][5].b)
		r3, _ := strconv.Atoi(instructions[i][15].b)
		for z_out, _ := range possible_zs[i+1] {

			for w := 1; w <= 9; w++ {
				// t == 1 case
				for o := 0; o <= 25; o++{
					// z_out := (z_in_t1 / r1) * 26 + w + r3
					z_in_t1 := ((z_out - w - r3) / 26) * r1 + o
					z_out_check := (z_in_t1 / r1) * 26 + w + r3
					if z_out == z_out_check && ((z_in_t1 % 26) + r2 != w) {
						if prev_w, ok := z_ins[z_in_t1]; !ok || (!reverse && prev_w < w) || (reverse && prev_w > w) {
							z_ins[z_in_t1] = w
						}
					}
				}

				// t == 0 case
				z_in_t0 := r1 * z_out + w - r2
				if (z_in_t0 % 26) + r2 == w {
					if prev_w, ok := z_ins[z_in_t0]; !ok || (!reverse && prev_w < w) || (reverse && prev_w > w) {
						z_ins[z_in_t0] = w
					}
				}
			}
		}
		possible_zs[i] = z_ins
	}

	// traverse z_ins to build the solution
	z_i := 0
	w_i := possible_zs[0][z_i]
	// fmt.Println(0, z_i, w_i)
	ans := 0
	for i := 0; i < n_instructions; i++ {
		ans = 10*ans + w_i
		r1, _ := strconv.Atoi(instructions[i][4].b)
		r2, _ := strconv.Atoi(instructions[i][5].b)
		r3, _ := strconv.Atoi(instructions[i][15].b)
		z_i = runInstructionsSetFast(z_i, w_i, r1, r2, r3)
		w_i = possible_zs[i+1][z_i]
		// fmt.Println(i, z_i, w_i)
	}
	return ans
}

func main() {
	instructions := readInput24()
	ans1 := solve24Backwards(instructions, false)
	fmt.Println(ans1)
	ans2 := solve24Backwards(instructions, true)
	fmt.Println(ans2)
}
