import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;

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

	System.out.println(volumes);
    }
}
