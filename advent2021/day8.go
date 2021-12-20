package main

import (
	"fmt"
	"io"
	"sort"
)

type SegmentSet struct {
	signals [10]string
	output  [4]string
}

func ReadInput8() (segments []SegmentSet) {
	var s SegmentSet
	for {
		_, err := fmt.Scanf("%s %s %s %s %s %s %s %s %s %s | %s %s %s %s",
			&s.signals[0], &s.signals[1], &s.signals[2], &s.signals[3], &s.signals[4],
			&s.signals[5], &s.signals[6], &s.signals[7], &s.signals[8], &s.signals[9],
			&s.output[0], &s.output[1], &s.output[2], &s.output[3])
		if err == io.EOF {
			break
		}
		segments = append(segments, s)

	}
	return
}

func Solve8a(segments []SegmentSet) (result int){
	for _, s := range segments {
		for _, o := range s.output {
			if (len(o) == 2 || len(o) == 4 ||
				len(o) == 3 || len(o) == 7) {
				result += 1
			}
		}
	}
	return
}

func Solve8b(segments []SegmentSet) (result int) {
	for _, s := range segments {
		segmentMapping := findDigits(s.signals)
		secret := 0
		for _, o := range s.output {
			d := decodeDigit(o, segmentMapping)
			secret = 10*secret + d
		}
		result += secret
	}
	return
}

func strIntersection(a, b string) (interSize int, interChars string) {
	// check runes both in a and in b

	// create set of runes in a
	aRunes := make(map[rune]struct{})
	for _, c := range a {
		aRunes[c] = struct{}{}
	}
	// save intersection
	for _, c := range b {
		if _, ok := aRunes[c]; ok {
			interChars += string(c)
		}
	}
	interSize = len(interChars)
	return
}

func strDifference(a, b string) (diff []rune) {
	// check runes in a and, but not in b, assuming len(a) > len(b)

	// create set of runes in b
	aRunes := make(map[rune]struct{})
	for _, c := range b {
		aRunes[c] = struct{}{}
	}
	// save dofference
	for _, c := range a {
		if _, ok := aRunes[c]; !ok {
			diff = append(diff, c)
		}
	}
	return
}

func strContains(s string, q rune) (found bool) {
	for _, c := range s {
		if c == q {
			found = true
			break
		}
	}
	return
}

func findDigits(signals [10]string) (segmentMapping map[rune]rune) {
	var zero, one, two, three, four, five, seven, eight, nine string
	var zeroSixNine, twoThreeFive, twoFive, zeroSix []string // grounp ambiguous sequences together
	segmentMapping = make(map[rune]rune)

	// Algorithm:
	// - find 1, 4, 7, 8, (235), (069)
	for _, signal := range signals {
		switch len(signal) {
		case 2:
			one = signal
		case 3:
			seven = signal
		case 4:
			four = signal
		case 5:
			twoThreeFive = append(twoThreeFive, signal)
		case 6:
			zeroSixNine = append(zeroSixNine, signal)
		case 7:
			eight = signal
		}
	}
	// - find 3 as the only digit in (235) with intersections
	if len(twoThreeFive) != 3 {
		panic("twoThreeFive has len different of 3!")
	}
	inter01, _ := strIntersection(twoThreeFive[0], twoThreeFive[1])
	inter02, _ := strIntersection(twoThreeFive[0], twoThreeFive[2])
	inter12, _ := strIntersection(twoThreeFive[1], twoThreeFive[2])
	switch {
	case inter01>3 && inter02>3:
		three = twoThreeFive[0]
		twoFive = twoThreeFive[1:]
	case inter01>3 && inter12>3:
		three = twoThreeFive[1]
		twoFive = append(twoThreeFive[:1], twoThreeFive[2])
	case inter02>3 && inter12>3:
		three = twoThreeFive[2]
		twoFive = twoThreeFive[:2]
	}

	// - comparing 3 and (069) find 9 as the only digit with all in common
	if len(zeroSixNine) != 3 {
		panic("zeroSixNine has len different of 3!")
	}
	inter30, _ := strIntersection(three, zeroSixNine[0])
	inter31, _ := strIntersection(three, zeroSixNine[1])
	inter32, _ := strIntersection(three, zeroSixNine[2])
	switch {
	case inter30 == 5:
		nine = zeroSixNine[0]
		zeroSix = zeroSixNine[1:]
	case inter31 == 5:
		nine = zeroSixNine[1]
		zeroSix = append(zeroSixNine[:1], zeroSixNine[2])
	case inter32 == 5:
		nine = zeroSixNine[2]
		zeroSix = zeroSixNine[:2]
	}

	// - comparing 1 and (06) find 0 as the digit with intersection == 2
	switch inter10Or6, _ := strIntersection(one, zeroSix[0]); inter10Or6 == 2 {
	case true:
		zero, _ = zeroSix[0], zeroSix[1]
	case false:
		zero, _ = zeroSix[1], zeroSix[0]
	}

	// - From 8 and 9 we can find 'e' by exclusion
	e_coded := strDifference(eight, nine)[0]

	// - Find 2 inside (25) as the one with 'e', and 5 as the other
	switch strContains(twoFive[0], e_coded) {
	case true:
		two, five = twoFive[0], twoFive[1]
	case false:
		two, five = twoFive[1], twoFive[0]
	}

	// map random segment to original segment
	segmentMapping[strDifference(seven, one)[0]] = 'a'
	segmentMapping[strDifference(five, three)[0]] = 'b'
	segmentMapping[strDifference(one, five)[0]] = 'c'
	segmentMapping[strDifference(eight, zero)[0]] = 'd'
	segmentMapping[e_coded] = 'e'
	segmentMapping[strDifference(three, two)[0]] = 'f'
	segmentMapping[strDifference(string(strDifference(nine, four)), seven)[0]] = 'g'

	return
}

func decodeDigit(s string, segmentMapping map[rune]rune) int {
	// decode digit s using segmentMapping lookup table
	decoded := []rune{}
	for _, c := range s {
		decoded = append(decoded, segmentMapping[c])
	}
	// sort decoded so we can compare it
	sort.Slice(decoded, func(i, j int) bool {return decoded[i] < decoded[j]})

	switch string(decoded) {
	case "abcefg":
		return 0
	case "cf":
		return 1
	case "acdeg":
		return 2
	case "acdfg":
		return 3
	case "bcdf":
		return 4
	case "abdfg":
		return 5
	case "abdefg":
		return 6
	case "acf":
		return 7
	case "abcdefg":
		return 8
	case "abcdfg":
		return 9
	default:
		panic("Could not decode!")
	}
}
// 1 - 2 - cf
// 7 - 3 - acf
// 4 - 4 - bcdf
// 2 - 5 - acdeg
// 3 - 5 - acdfg
// 5 - 5 - abdfg
// 0 - 6 - abcefg
// 6 - 6 - abdefg
// 9 - 6 - abcdfg
// 8 - 7 - abcdefg

func main() {
	segments := ReadInput8()
	fmt.Println(Solve8a(segments))
	fmt.Println(Solve8b(segments))

}
