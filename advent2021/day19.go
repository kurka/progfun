package main

import (
	"fmt"
	"math"
	"sort"
)

type PairCoord3D struct {
	a, b Coord3D
}
type Coord3D struct {
	x, y, z int
}

type Nei struct {
	absolutePos, relativePos Coord3D
	magnitude                float64
}

func containsi(l []int, e int) (found bool) {
	for i := range l {
		if l[i] == e {
			found = true
			break
		}
	}
	return
}

func readInput19() (allScans [][]Coord3D) {
	var c Coord3D
	var i int
	scanCoordinate := func() (err error) {
		_, err = fmt.Scanf("%d,%d,%d\n", &c.x, &c.y, &c.z)
		return
	}
	for {
		_, err := fmt.Scanf("--- scanner %d ---\n", &i)
		if err != nil {
			break
		}
		scanner := make([]Coord3D, 0)
		for err := scanCoordinate(); err == nil; err = scanCoordinate() {
			scanner = append(scanner, c)
		}
		allScans = append(allScans, scanner)
	}
	return
}

func dist(a, b Coord3D) Coord3D {
	return Coord3D{b.x - a.x, b.y - a.y, b.z - a.z}
}

func magn(a, b Coord3D) float64 {
	math.Pow(float64(a.x), 2)
	return math.Sqrt(math.Pow(float64(b.x-a.x), 2) +
		math.Pow(float64(b.y-a.y), 2) +
		math.Pow(float64(b.z-a.z), 2))
}

func solve19(allScans [][]Coord3D) {
	fmt.Println(allScans)
	fmt.Println("\n\n")
	// store all data in special format
	scanData := make([]*map[Coord3D]*[]Nei, 0, len(allScans))
	for s := range allScans {
		scanS := allScans[s]
		scanMap := make(map[Coord3D]*[]Nei, len(scanS))
		for _, c := range scanS {
			// for each beacon, store the relative position of other beacons,
			// and the magnitude
			cNeis := make([]Nei, 0, len(scanS)-1)
			for _, cn := range scanS {
				if cn == c {
					continue
				}
				cNeis = append(cNeis, Nei{cn, dist(c, cn), magn(c, cn)})
			}
			// order by magnitude
			sort.Slice(cNeis, func(i, j int) bool { return cNeis[i].magnitude < cNeis[j].magnitude })
			// check if all are unique - we are taking advantage of it
			for i := 0; i < len(cNeis)-1; i++ {
				if cNeis[i].magnitude == cNeis[i+1].magnitude {
					panic("duplicated magnitudes!")
				}
			}
			// fmt.Println(c, cNeis)
			scanMap[c] = &cNeis
		}
		// fmt.Println(scanMap)
		scanData = append(scanData, &scanMap)
	}

	origin := Coord3D{0, 0, 0}
	alreadySolved := make([]int, 0, len(scanData)-1)
	beacons := make([]Coord3D, 0, len(scanData))
	// add scan0 beacon as 0,0,0 position
	beacons = append(beacons, origin)
	for {
		if len(alreadySolved) == len(scanData)-1 {
			break
		}
		// find scan with biggest similarity in magnitude to 0
		matchedScan, mCoord1, mCoord2 := findSimilarMagnitudes(&scanData, alreadySolved)
		// fmt.Println(matchedScan, mCoord1, mCoord2)
		// find mask using relative positions (i.e. offset == origin)
		rotation, direction, offset := findMask(&scanData, matchedScan, mCoord1, mCoord2)
		// fmt.Println(rotation, direction, offset)
		newc2 := translate(mCoord2, rotation, direction, origin)
		// now find offset by comparing mCoord1 and newc2 (equal to adjusted mCoord2)
		offset = Coord3D{newc2.x - mCoord1.x, newc2.y - mCoord1.y, newc2.z - mCoord1.z}
		// fmt.Println(offset)
		// fmt.Println(mCoord1, mCoord2, newc2, offset, translate(mCoord2, rotation, direction, offset))
		// add items from matchedScan into scan0
		for coord, neis := range *scanData[matchedScan] {
			coordAdjusted := translate(coord, rotation, direction, offset)
			if _, ok := (*scanData[0])[coordAdjusted]; ok {
				// just add to scan0 if not there already
				// TODO: merge lists?
				continue
			}
			// transpose list of neighbors
			for i := range *neis {
				nei := (*neis)[i]
				nei.absolutePos = translate(nei.absolutePos, rotation, direction, offset)
				nei.relativePos = translate(nei.relativePos, rotation, direction, origin)
				(*neis)[i] = nei
			}
			(*scanData[0])[coordAdjusted] = neis
		}
		// fmt.Println("found", matchedScan, "new len", len((*scanData[0])))
		alreadySolved = append(alreadySolved, matchedScan)
		beacons = append(beacons, translate(origin, rotation, direction, offset))
	}
	fmt.Println(len((*scanData[0])))

	// find biggest distance for part 2
	maxDist := 0.0
	for _, ci := range beacons {
		for _, cj := range beacons {
			mdist := math.Abs(float64(ci.x-cj.x)) + math.Abs(float64(ci.y-cj.y)) + math.Abs(float64(ci.z-cj.z))
			if mdist > maxDist {
				maxDist = mdist
			}
		}
	}
	fmt.Println(maxDist)

}

func findSimilarMagnitudes(scanData *[]*map[Coord3D]*[]Nei, alreadySolved []int) (matchedScan int, mcoord1, mcoord2 Coord3D) {
	bestMagSim := 0
	for c, neis := range *(*scanData)[0] {
		// fmt.Println(c, *neis)
		for s := 1; s < len(*scanData); s++ {
			if containsi(alreadySolved, s) {
				continue
			}
			// fmt.Println(s)
			for cc, nneis := range *(*scanData)[s] {
				// fmt.Println(cc, *neis)
				// fmt.Println(cc)
				magSim := magSimilarity(*neis, *nneis)
				if magSim > bestMagSim {
					// fmt.Println(magSim, s, cc)
					bestMagSim = magSim
					matchedScan = s
					mcoord1 = c
					mcoord2 = cc
				}
				// if magSim >= 11 {
				// 	return
				// }
			}
		}
	}

	if bestMagSim < 11 {
		fmt.Println(bestMagSim)
		fmt.Println("couldn't find anything useful!!")
	}
	return
}

func magSimilarity(neisA, neisB []Nei) (similars int) {
	i, j := 0, 0
	for {
		if math.Abs(neisA[i].magnitude-neisB[j].magnitude) < 1e-6 {
			similars++
			i++
			j++
		} else if neisA[i].magnitude < neisB[j].magnitude {
			i++
		} else if neisA[i].magnitude > neisB[j].magnitude {
			j++
		}
		if i == len(neisA) || j == len(neisB) {
			break
		}
	}
	return
}

func findMask(scanData *[]*map[Coord3D]*[]Nei, matchedScan int, mCoord1, mCoord2 Coord3D) (rotation, direction, offset Coord3D) {
	// offset = Coord3D{mCoord2.x - mCoord1.x, mCoord2.y - mCoord1.y, mCoord2.z - mCoord1.z}
	offset = Coord3D{0, 0, 0}
	rotations := []Coord3D{
		Coord3D{0, 1, 2},
		Coord3D{0, 2, 1},
		Coord3D{1, 0, 2},
		Coord3D{1, 2, 0},
		Coord3D{2, 0, 1},
		Coord3D{2, 1, 0},
	}
	directions := []Coord3D{
		Coord3D{1, 1, 1},
		Coord3D{1, 1, -1},
		Coord3D{1, -1, 1},
		Coord3D{-1, 1, 1},
		Coord3D{1, -1, -1},
		Coord3D{-1, 1, -1},
		Coord3D{-1, -1, 1},
		Coord3D{-1, -1, -1},
	}
	// rotDir := []PairCoord3D{
	// 	PairCoord3D{Coord3D{0, 1, 2}, Coord3D{1, 1, 1}},
	// 	PairCoord3D{Coord3D{0, 1, 2}, Coord3D{1, -1, -1}},
	// 	PairCoord3D{Coord3D{0, 2, 1}, Coord3D{1, -1, 1}},
	// 	PairCoord3D{Coord3D{0, 2, 1}, Coord3D{1, 1, -1}},

	// 	PairCoord3D{Coord3D{1, 0, 2}, Coord3D{1, 1, 1}},
	// 	PairCoord3D{Coord3D{1, 0, 2}, Coord3D{1, -1, -1}},
	// 	PairCoord3D{Coord3D{1, 2, 0}, Coord3D{1, -1, 1}},
	// 	PairCoord3D{Coord3D{1, 2, 0}, Coord3D{1, 1, -1}},

	// 	PairCoord3D{Coord3D{2, 0, 1}, Coord3D{1, 1, 1}},
	// 	PairCoord3D{Coord3D{2, 0, 1}, Coord3D{1, -1, -1}},
	// 	PairCoord3D{Coord3D{2, 1, 0}, Coord3D{1, -1, 1}},
	// 	PairCoord3D{Coord3D{2, 1, 0}, Coord3D{1, 1, -1}},

	// 	PairCoord3D{Coord3D{0, 1, 2}, Coord3D{-1, -1, 1}},
	// 	PairCoord3D{Coord3D{0, 1, 2}, Coord3D{-1, 1, -1}},
	// 	PairCoord3D{Coord3D{0, 2, 1}, Coord3D{-1, -1, -1}},
	// 	PairCoord3D{Coord3D{0, 2, 1}, Coord3D{-1, 1, 1}},

	// 	PairCoord3D{Coord3D{1, 0, 2}, Coord3D{-1, 1, 1}},
	// 	PairCoord3D{Coord3D{1, 0, 2}, Coord3D{-1, -1, -1}},
	// 	PairCoord3D{Coord3D{1, 2, 0}, Coord3D{-1, -1, 1}},
	// 	PairCoord3D{Coord3D{1, 2, 0}, Coord3D{-1, 1, -1}},

	// 	PairCoord3D{Coord3D{2, 0, 1}, Coord3D{-1, 1, -1}},
	// 	PairCoord3D{Coord3D{2, 0, 1}, Coord3D{-1, -1, 1}},
	// 	PairCoord3D{Coord3D{2, 1, 0}, Coord3D{-1, 1, 1}},
	// 	PairCoord3D{Coord3D{2, 1, 0}, Coord3D{-1, -1, -1}},
	// }
	for _, r := range rotations {
		for _, d := range directions {
			// for _, rd := range rotDir {
			// r, d := rd.a, rd.b
			if checkMask(*(*(*scanData)[0])[mCoord1], *(*(*scanData)[matchedScan])[mCoord2], r, d, offset) {
				return r, d, offset
			}
		}
	}
	panic("no mask found!")
	return
}

func checkMask(neisA, neisB []Nei, rotation, direction, offset Coord3D) bool {
	i, j := 0, 0
	// fmt.Println("checking: ", rotation, direction, offset)
	for {
		if math.Abs(neisA[i].magnitude-neisB[j].magnitude) < 1e-6 {
			// fmt.Println(neisA[i])
			// fmt.Println(neisB[j])
			// fmt.Println(translate(neisB[j].relativePos, rotation, direction, offset))
			if neisA[i].relativePos != translate(neisB[j].relativePos, rotation, direction, offset) {
				return false
			}
			i++
			j++
		} else if neisA[i].magnitude < neisB[j].magnitude {
			i++
		} else if neisA[i].magnitude > neisB[j].magnitude {
			j++
		}
		if i == len(neisA) || j == len(neisB) {
			break
		}
	}
	return true
}

func translate(c Coord3D, r, d, o Coord3D) Coord3D {
	cc := []int{c.x, c.y, c.z}
	return Coord3D{cc[r.x]*d.x - o.x, cc[r.y]*d.y - o.y, cc[r.z]*d.z - o.z}
}

func main() {
	allScans := readInput19()
	solve19(allScans)
	// fmt.Println(allScans)

}
