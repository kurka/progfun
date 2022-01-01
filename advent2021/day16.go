package main

import (
	"fmt"
)

type Packet struct {
	version    int
	typeId     int
	subpackets []Packet
	literal    int
}

func hex2bin(h rune) (bits []int) {
	switch h {
	case '0':
		bits = []int{0, 0, 0, 0}
	case '1':
		bits = []int{0, 0, 0, 1}
	case '2':
		bits = []int{0, 0, 1, 0}
	case '3':
		bits = []int{0, 0, 1, 1}
	case '4':
		bits = []int{0, 1, 0, 0}
	case '5':
		bits = []int{0, 1, 0, 1}
	case '6':
		bits = []int{0, 1, 1, 0}
	case '7':
		bits = []int{0, 1, 1, 1}
	case '8':
		bits = []int{1, 0, 0, 0}
	case '9':
		bits = []int{1, 0, 0, 1}
	case 'A':
		bits = []int{1, 0, 1, 0}
	case 'B':
		bits = []int{1, 0, 1, 1}
	case 'C':
		bits = []int{1, 1, 0, 0}
	case 'D':
		bits = []int{1, 1, 0, 1}
	case 'E':
		bits = []int{1, 1, 1, 0}
	case 'F':
		bits = []int{1, 1, 1, 1}
	}
	return
}

func bin2int(bits []int) (val int) {
	for i := 0; i < len(bits); i++ {
		val = 2*val + bits[i]
	}
	return
}

func readInput16() []int {
	var hexSequence string
	fmt.Scan(&hexSequence)

	bitstream := make([]int, 0, 4*len(hexSequence))
	for _, h := range hexSequence {
		bitstream = append(bitstream, hex2bin(h)...)
	}
	return bitstream
}

func parsepacket(bitstream []int) (packet Packet, size int) {
	version := bin2int(bitstream[0:3])
	typeId := bin2int(bitstream[3:6])
	subpackets := []Packet{}
	literal := 0

	// switch for data reading
	switch typeId {
	case 4:
		literalBits := make([]int, 0, len(bitstream))
		// accumulate the groups starting with 1
		groupStart := 6
		for ; bitstream[groupStart] == 1; groupStart += 5 {
			literalBits = append(literalBits, bitstream[groupStart+1:groupStart+5]...)
		}
		// also add the group starting with 0
		literalBits = append(literalBits, bitstream[groupStart+1:groupStart+5]...)
		literal = bin2int(literalBits)
		size = groupStart + 5
	default:
		// subpackets
		lengthTypeId := bitstream[6]
		switch lengthTypeId {
		case 0:
			subpacketLen := bin2int(bitstream[7:22])
			o := 22
			for o < subpacketLen+22 {
				subpacket, pSize := parsepacket(bitstream[o:])
				o += pSize
				subpackets = append(subpackets, subpacket)
			}
			size = o

		case 1:
			subpacketNb := bin2int(bitstream[7:18])
			o := 18
			for sp := 0; sp < subpacketNb; sp++ {
				subpacket, pSize := parsepacket(bitstream[o:])
				o += pSize
				subpackets = append(subpackets, subpacket)
			}
			size = o
		}
	}

	// switch for op processing
	switch typeId {
	case 0:
		for i := range subpackets {
			literal += subpackets[i].literal
		}
	case 1:
		literal = 1
		for i := range subpackets {
			literal *= subpackets[i].literal
		}
	case 2:
		min := subpackets[0].literal
		for i := range subpackets {
			if sl := subpackets[i].literal; sl < min {
				min = sl
			}
		}
		literal = min
	case 3:
		max := subpackets[0].literal
		for i := range subpackets {
			if sl := subpackets[i].literal; sl > max {
				max = sl
			}
		}
		literal = max
	case 4:
		_ = literal
	case 5:
		if subpackets[0].literal > subpackets[1].literal {
			literal = 1
		} else {
			literal = 0
		}
	case 6:
		if subpackets[0].literal < subpackets[1].literal {
			literal = 1
		} else {
			literal = 0
		}
	case 7:
		if subpackets[0].literal == subpackets[1].literal {
			literal = 1
		} else {
			literal = 0
		}
	}
	packet = Packet{
		version:    version,
		typeId:     typeId,
		subpackets: subpackets,
		literal:    literal,
	}

	return
}

func sumVersions(p Packet) (sum int) {
	sum += p.version
	for i := range p.subpackets {
		sum += sumVersions(p.subpackets[i])
	}
	return
}

func solve16(bitstream []int) {
	packet, _ := parsepacket(bitstream)
	// sum versions
	fmt.Println(sumVersions(packet))
	// get literal
	fmt.Println(packet.literal)
}

func main() {
	bitstream := readInput16()
	solve16(bitstream)
}
