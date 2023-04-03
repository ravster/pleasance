// javac nn.java
// java nn qyld.csv
// java nn.java qyld.csv
// from https://beginnersbook.com/2013/05/first-java-program/
// using https://hub.docker.com/_/amazoncorretto

import java.util.Scanner;
import java.nio.file.Paths;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Arrays;
import java.lang.Math;

public class nn {
    static ArrayList<Float> opens = new ArrayList<Float>();
    static ArrayList<Float> highs = new ArrayList<Float>();
    static ArrayList<Float> lows = new ArrayList<Float>();
    static ArrayList<Float> closes = new ArrayList<Float>();
    static int numRows = 0;
    static Float[] normalizedCloses;
    static Float[] trs;
    static Float[] atr10s;
    static Float[] normalizedAtr10s;
    static Float[] ma20s;
    static Float[] normalizedMa20s;

    static void process_line(String[] line) {
	var open = Float.parseFloat(line[1]);
	opens.add(open);
	var high = Float.parseFloat(line[1]);
	highs.add(high);
	var low = Float.parseFloat(line[1]);
	lows.add(low);
	var close = Float.parseFloat(line[1]);
	closes.add(close);
    }

    static Float[] normalize(ArrayList<Float> data, int start, int stop) {
	var min = Collections.min(data.subList(start, stop));
	var max = Collections.max(data.subList(start, stop));
	var diff = max - min;
	var out = new Float[numRows];

	for (var i = start; i < stop; i++) {
	    out[i] = (data.get(i) - min) / diff;
	}
	return out;
    }

    public static void main(String[] args)
	throws IOException {
	var filename = args[0];
	var path = Paths.get(filename);
	var scanner = new Scanner(path);
	scanner.nextLine(); // Skip the headers
	while(scanner.hasNextLine()){
	    var line = scanner.nextLine();
	    var records = line.split(",");
	    process_line(records);
	}
	scanner.close();

	numRows = opens.size();

	normalizedCloses = normalize(closes, 20, numRows);

	trs = new Float[numRows];
	for(var i = 1; i < numRows; i++) {
	    var previousClose = closes.get(i-1);
	    var h = highs.get(i);
	    var l = lows.get(i);
	    var a = Math.abs(h - l);
	    var b = Math.abs(h - previousClose);
	    var c = Math.abs(previousClose - l);

	    trs[i] = Collections.max(Arrays.asList(a, b, c));
	}

	atr10s = new Float[numRows];
	// We start from index 11 instead of index 0 because we can't calc TR for
	// the first 10 data points.
	for(var i = 11; i < numRows; i++) {
	    var sum = 0f;
	    for(var j = -10; j <= 0; j++) {
		sum += trs[i+j];
	    }
	    atr10s[i] = sum / 10;
	}

	var atr10sAL = new ArrayList<Float>();
	atr10sAL.addAll(Arrays.asList(atr10s));
	normalizedAtr10s = normalize(atr10sAL, 11, numRows);

	ma20s = new Float[numRows];
	for(var i = 19; i < numRows; i++) {
	    var sum = 0f;
	    for(var j = -19; j <= 0; j++) {
		sum += closes.get(i+j);
	    }
	    ma20s[i] = sum / 20;
	}
	var ma20sAL = new ArrayList<Float>();
	ma20sAL.addAll(Arrays.asList(ma20s));
	normalizedMa20s = normalize(ma20sAL, 19, numRows);

	System.out.println(Arrays.toString(normalizedMa20s));
    }
}
