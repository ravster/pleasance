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

let closePlus15 = new Array()
let last = numRows - 15;
for(let i = 0; i < last; i++) {
    closePlus15.push(closes[i + 15]);
}

foo = calcMinMax(closePlus15, 0, numRows - 15)
min = foo[0]
max = foo[1]

let normalizedClosePlus15s = normalize(closePlus15, 1, numRows - 15, min, max - min)

// DONE building the data arrays.
// BEGIN building the neural network.

numInNodes = 2
numMidNodes = 4
numOutNodes = 1

// This will be num-mid rows and num-in columns.  The first 3 entries will be
// i0m0, i1m0, i2m0
weightsInMid = [];
weightsMidOut = [];
for(int i = 0; i < (numInNodes * numMidNodes); i++) {
    weightsInMid[i] = Math.random();
}
for(int i = 0; i < (numOutNodes * numMidNodes); i++) {
    weightsMidOut[i] = Math.random();
}

// The middle and outer layer
midNodes = new Array(numMidNodes);
outNodes = new Array(numOutNodes);

// TODO should calc total error here, and then again at the end of the training, to see
// what the improvement has been.

for (i = 0; i < (numRows - 15); i++) {
    // Doing the full dataset at the moment.  TODO Split to training and testing datasets.
    inNodes = [
	normalizedAtr10s[i],
	normalizedMa20s[i]
    ]
    targetOutNodes = [ normalizedClosePlus15s[i] ];

    // Calc midNodes for this datum
    // The rows represent midNodes and columns represent inNodes.
    for (j = 0; j < numMidNodes; j++) {
	sum = 0;
	for (k = 0; k < numInNodes; k++) {
	    sum += inNodes[k] * weightsInMid[(j * numInNodes) + k];
	}
	midNodes[j] = Math.tanh(sum);
    }

    // Calc outNodes
    for (j = 0; j < numOutNodes; j++) {
	sum = 0;
	for (k = 0; k < numMidNodes; k++) {
	    sum += midNodes[k] * weightsMidOut[(j * numOutNodes) + k];
	}
	outNodes[j] = Math.tanh(sum);
    }

    // Calc outNodeErrs
    outNodeErrGradients = [];
    // (1 - y^2) is the derivative of the hyperbolic tangent function, and is always
    // between -1 and 1.
    for (j = 0; j < numOutNodes; j++) {
	y = outNodes[j];
	outNodeErrGradients[j] = (1 - y * y) * (targetOutNodes[j] - y);
    }

    // Calc midNodeErrs
    midNodeErrGradients = [];
    for (j = 0; j < numOutNodes; j++) {
	for (k = 0; k < numMidNodes; k++) {
	    midNode = midNodes[k];
	    midNodeErrGradients[(j * numMidNodes) + k] =
		weightsMidOut[(j * numMidNodes) + k] *
		(1 - midNode * midNode) *
		outNodeErrGradients[j];
	}
    }

    // Change weightsInMid
    for(int j = 0; j < num_mid_nodes; j++) {
      for(int k = 0; k < num_in_nodes; k++) {
        weights_in_mid[(j * num_in_nodes) + k] +=
          0.003 * // rate of learning
          in_nodes[k] *
          mid_nodes_error_gradients[j];
      }
    }

}

console.log(Math.max(...normalizedClosePlus15s.slice(0, numRows - 15)))
console.log(Math.min(...normalizedClosePlus15s.slice(0, numRows - 15)))
