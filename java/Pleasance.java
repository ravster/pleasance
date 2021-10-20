import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.lang.Math;

class Pleasance {
    static ArrayList<String> dates = new ArrayList<String>(200);
    static ArrayList<Double> opens = new ArrayList<Double>(200);
    static ArrayList<Double> highs = new ArrayList<Double>(200);
    static ArrayList<Double> lows = new ArrayList<Double>(200);
    static ArrayList<Double> closes = new ArrayList<Double>(200);
    static ArrayList<Integer> volumes = new ArrayList<Integer>(200);

    public static void main(String[] args) throws java.io.IOException {
	BufferedReader csvReader = new BufferedReader(new FileReader(args[0]));
	String row;
	boolean first_row = true;
	while ((row = csvReader.readLine()) != null) {
	    if (first_row) {
		first_row = false;
		continue;
	    }

	    String[] data = row.split(",");
	    dates.add(data[0]);
	    opens.add(Double.parseDouble(data[1]));
	    highs.add(Double.parseDouble(data[2]));
	    lows.add(Double.parseDouble(data[3]));
	    closes.add(Double.parseDouble(data[4]));
	    volumes.add(Integer.valueOf(data[6])); // Vol is last column
	}
	csvReader.close();

	System.out.println("Row count=" + volumes.size());

	dollarCostAverage();
    }

    static void dollarCostAverage() {
	System.out.println("Basic dollar-cost-averaging");

	double leftover = 0;
	double tot_bought = 0;
	double tot_spent = 0;
	int count = volumes.size();

	for (int i = 0; i < count; i += 10) {
	    double close = closes.get(i);
	    double spendable = 50 + leftover;
	    double bought = Math.floor(spendable/close);
	    tot_bought += bought;
	    double spent = bought * close;
	    tot_spent += spent;
	    leftover = spendable - spent;
	}

	System.out.println("Leftover=" + leftover + "\n bought=" + tot_bought +
			   "\nSpent = " + tot_spent +
			   "\naverage price bought=" + tot_spent / tot_bought +
			   "\nMedian price = " + median(closes));
	System.out.println("DONE dollar-cost-averaging");
    }

    static Double median(ArrayList<Double> data) {
	ArrayList<Double> d2 = (ArrayList<Double>) data.clone();
	d2.sort(null);
	int index = (int)(data.size() / 2);
	if ((data.size()/2) % 2 != 0) {
	    return data.get(index);
	}
	return (data.get(index -1) + data.get(index)) / 2;
    }
}
