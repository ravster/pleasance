package main

import (
	"log"
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func process_line(current_price float64, line string) {
	v := strings.Split(line, "\t")
	strike, _ := strconv.ParseFloat(v[2], 32)
	last, _ := strconv.ParseFloat(v[3], 32)
	bid, _ := strconv.ParseFloat(v[4], 32)
	ask, _ := strconv.ParseFloat(v[5], 32)
	expire := last / current_price
	exercise := (last + strike - current_price) / current_price

	if (expire >= 0.01) && (exercise >= 0.01) {
		fmt.Printf(
			"%.2f\t %.2f\t %.2f\t %.2f\t %.2f\t %.2f\n",
			strike, last, bid, ask, expire, exercise)
	}
}

func main() {
	if len(os.Args) != 3 {
		log.Fatalln("Usage: ./options 4.8 file.txt")
	}

	current_price, err := strconv.ParseFloat(os.Args[1], 32)
	if err != nil {
		log.Fatalln("Can't parse float to current-price")
	}

	filename := os.Args[2]
	fmt.Println(current_price, filename)

	file, err := os.Open(filename)
	if err != nil {
		log.Fatalln("can't open file")
	}
	defer file.Close()
	fmt.Println("strike\t last\t bid\t ask\t expire\t exercise")

	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)
	for scanner.Scan() {
		process_line(current_price, scanner.Text())
	}

}
