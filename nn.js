const fs = require('fs');

let fileContents = "";

try {
  fileContents = fs.readFileSync('qyld.csv', 'utf8');
} catch (err) {
  console.error(err);
}

let dates = [];
let opens = [];
let highs = [];
let lows = [];
let closes = [];
fileContents.split("\n").forEach((line) => {
    let fields = line.split(',')
    dates.push(fields[0])
    opens.push(Number(fields[1]))
    highs.push(Number(fields[2]))
    lows.push(Number(fields[3]))
    closes.push(Number(fields[4]))
})

// Remove the header
dates.splice(0, 1)
opens.splice(0, 1)
highs.splice(0, 1)
lows.splice(0, 1)
closes.splice(0, 1)

let numRows = dates.length

function calcMinMax(data, start, stop) {
    return([Math.min(...data.slice(start, stop)),
	    Math.max(...data.slice(start, stop))
	   ])
}
let [min, max] = calcMinMax(closes, 0, numRows)

// Normalize between 0 and 1
function normalize(data, start, stop, min, diff) {
    let out = new Array(start - 1)
    for (let i = start; i < stop; i++) {
	out.push(
	    (data[i] - min) / diff
	);
    }
    return out
}
let normalizedCloses = normalize(closes, 20, numRows, min, max - min)

let trs = new Array(1)
// We start from index 1 instead of index 0 because we can't calc TR for the first datapoint.
for(let i = 1; i < numRows; i++) {
    let previousClose = closes[i-1]
    let h = highs[i]
    let l = lows[i]
    let a = Math.abs(h - l)
    let b = Math.abs(h - previousClose)
    let c = Math.abs(previousClose - l)

    trs.push(Math.max(a, b, c))
}

let atr10s = new Array(11)
// We start from index 10 instead of index 0 because we can't calc TR for the first 10 datapoints.
for(let i = 11; i < numRows; i++) {
    let sum = 0;
    for(let j = -10; j <= 0; j++) {
	sum += trs[i+j];
    }
    atr10s.push(sum / 10);
}

let foo = calcMinMax(atr10s, 11, numRows)
min = foo[0]
max = foo[1]

let normalizedAtr10s = normalize(atr10s, 11, numRows, min, max - min)

let ma20s = new Array(19)
for(let i = 19; i < numRows; i++) {
    let sum = 0;
    for(let j = -19; j <= 0; j++) {
	sum += closes[i+j];
    }
    ma20s.push(sum / 20);
}

foo = calcMinMax(ma20s, 19, numRows)
min = foo[0]
max = foo[1]

let normalizedMa20s = normalize(ma20s, 19, numRows, min, max - min)
console.log(Math.max(...normalizedMa20s.slice(19)))
console.log(Math.min(...normalizedMa20s.slice(19)))
