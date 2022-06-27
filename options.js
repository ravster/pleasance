const fs = require("fs");

function round2Decimals(num) {
    /* If we ever deal with negative numbers, remember to save the Math.sign, 
    and then abs, and then round.
    Because JS is weird about rounding negatives. */
    return Math.round(num * 100) / 100;
}

function processLine(line) {
    if (line.length < 10) { 
        return 
    }
    elements = line.split(/\t/)

    strike = parseFloat(elements[2])
    last = parseFloat(elements[3])
    bid = parseFloat(elements[4])
    ask = parseFloat(elements[5])

    if ((last < bid) || (last > ask)) {
        return;
    }
    expire = round2Decimals(last / current_price)
    assign = round2Decimals((last + strike - current_price) / current_price)
    if ((expire < 0.01) || (assign < 0.01)) {
        return
    }

    console.log(`${strike}\t${last}\t${bid}\t${ask}\t${expire}\t${assign}`)
}   

let current_price = process.argv[2]
let filename = process.argv[3]

console.log("strike\tlast\tbid\task\texpire\texercise")

fs.readFile(filename, (err, data) => {
    if (err) {
        console.error("readFile fail")
        console.error(err)
    }
    lines = data.toString().split("\n")
    lines.forEach((line) => {
        processLine(line)
    })
})
