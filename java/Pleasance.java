import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.lang.Math;
import java.util.List;
import java.util.Collections;
import java.util.Arrays;

class Pleasance {
	static ArrayList<String> dates = new ArrayList<String>(200);
	static ArrayList<Double> opens = new ArrayList<Double>(200);
	static ArrayList<Double> highs = new ArrayList<Double>(200);
	static ArrayList<Double> lows = new ArrayList<Double>(200);
	static ArrayList<Double> closes = new ArrayList<Double>(200);
	static ArrayList<Integer> volumes = new ArrayList<Integer>(200);

	static List<Double> hh100 = new ArrayList<Double>(1300);
	static List<Double> ll100 = new ArrayList<Double>(1300);

	public static void main(String[] args) throws java.io.IOException {
		BufferedReader csvReader = new BufferedReader(new FileReader(args[0]));
		var row = "";
		var first_row = true;
		while ((row = csvReader.readLine()) != null) {
			if (first_row) {
				first_row = false;
				continue;
			}

			var data = row.split(",");
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
		calc_100s();
		breakout_100();
	}

	static void breakout_100() {
		System.out.println("T2 Breakout from highest-high or lowest-low of last 100d.");

		var profit = 0.0;
		var max = volumes.size();
		var buys = 0;
		var sells = 0;
		for(int i = 102; i < max - 6; i++) {
			if (highs.get(i-1) > hh100.get(i-2)) {
				profit += closes.get(i+5) - opens.get(i);
				buys++;
			} else if (lows.get(i-1) < ll100.get(i-2)) {
				profit += opens.get(i) - closes.get(i+5);
				sells++;
			}
		}

		System.out.println("Buys: " + buys);
		System.out.println("Sells: " + sells);
		System.out.println("Profit: " + profit);
	}

	static void calc_100s() {
		var max_highs = new Double[volumes.size()];
		var min_lows = new Double[volumes.size()];

		for (int i = 100; i < volumes.size(); i++) {
			List<Double> subList = highs.subList(i - 100, i - 1);
			max_highs[i] = Collections.max(subList);
		}

		for (int i = 100; i < volumes.size(); i++) {
			List<Double> subList = lows.subList(i - 100, i - 1);
			min_lows[i] = Collections.min(subList);
		}

		hh100 = Arrays.asList(max_highs);
		ll100 = Arrays.asList(min_lows);
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
			double bought = Math.floor(spendable / close);
			tot_bought += bought;
			double spent = bought * close;
			tot_spent += spent;
			leftover = spendable - spent;
		}

		System.out.println("Leftover=" + leftover + "\n bought=" + tot_bought + "\nSpent = " + tot_spent
				+ "\naverage price bought=" + tot_spent / tot_bought + "\nMedian price = " + median(closes));
		System.out.println("DONE dollar-cost-averaging");
	}

	static Double median(ArrayList<Double> data) {
		ArrayList<Double> d2 = (ArrayList<Double>) data.clone();
		d2.sort(null);
		int index = (int) (data.size() / 2);
		if ((data.size() / 2) % 2 != 0) {
			return data.get(index);
		}
		return (data.get(index - 1) + data.get(index)) / 2;
	}
}
