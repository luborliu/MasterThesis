public class GravityVectorExtraction {
	/**
	 * extract the GVs from the moving cluster
	 * @param cluster: input a cluster for extracting the GVs
	 * @return an arraylist of GVs
	 */
	public ArrayList<GravityVector> extractGravityVector(Cluster cluster) {
		//Step(1): Calculate the average COG of the whole cluster
		double avgCOG = cluster.calculateAverageDirection();
		//Step(2) Map all the points to the axis and partition the points based on 0.01 grid width
		ArrayList<MappingPoint> mpLst = new ArrayList<MappingPoint>();
		for(int i=0; i<cluster.getCluster().size(); i++) {	
			MappingPoint mp = MappingPoint.convertPointToMappingPoint(cluster.getCluster() .get(i), avgCOG);
			mpLst.add(mp);
		}
		insertionSort(mpLst);
		ArrayList<GravityVector> ppL = new ArrayList<GravityVector>();
		int count = 0;
		int k = 0;
		double sum_x = 0;
		double sum_y = 0;
		double sum_SOG = 0;
		double sum_COG = 0;
		ArrayList<MappingPoint> traPointsTMP = new ArrayList<MappingPoint>();
		double medianDistance = 0;
		//partition the points and calculate each GV for each cell
		while(count<=mpLst.size()) {
			if(count < mpLst.size() && (mpLst.get(count) .getMappingtude() - mpLst.get(k). getMappingtude() < 0.01)) {//0.01 as the pre-defined grid width
				sum_x = sum_x+mpLst.get(count).getLongitude();
				sum_y = sum_y+mpLst.get(count).getLatitude();				
				sum_SOG = sum_SOG+mpLst.get(count).getSOG();
				sum_COG = sum_COG+mpLst.get(count).getCOG();
				traPointsTMP.add(mpLst.get(count));
				count++;
			} else {
				double x = 0;
				double y = 0;				
				double sog = 0;
				double cog = 0;
				x = sum_x/(double)(count-k);
				y = sum_y/(double)(count-k);					
				sog = sum_SOG/(double)(count-k);
				cog = sum_COG/(double)(count-k);
				//insert median distance calculation					
				double[] distances = new double[traPointsTMP.size()];
				for(int i=0; i<traPointsTMP.size();i++) {
					double lon=traPointsTMP.get(i).getLongitude();
					double lat=traPointsTMP.get(i).getLatitude();
					double dist = gpsDistance(lat, lon, y, x);
					distances[i]=dist;
				}
				//medianDistance
				medianDistance = quartile(distances, 50);
                //for each cell of the grid, calculate its GV
				GravityVector gv = new GravityVector(x,y,cog,sog,medianDistance);
				ppL.add(gv);				
				sum_x = 0;
				sum_y = 0;
				sum_COG = 0;
				sum_SOG = 0;
				k = count;
				traPointsTMP.clear();
				if(count==mpLst.size()) break;
			}
		}
		return ppL;
	}
    /**
     * insertion sorting
     */
	public void insertionSort(ArrayList<MappingPoint> mpl) {
		for(int i=1; i<mpl.size(); i++) {
			int k = i;
			MappingPoint mp = mpl.get(i);
			boolean insertAlready = false;
			while(mpl.get(i).getMappingtude()<mpl.get(k-1).getMappingtude()) {
				if(k==1) {
					mpl.remove(i);
					mpl.add(0, mp);
					insertAlready = true;
					break;
				}
				k--;				
			}
			if(!insertAlready) {
				mpl.remove(i);
				mpl.add(k,mp);
			}
		}
	}
	 /**
     * Retrive the quartile value from an array, used for generating relative distance.
     * @param values The array of data
     * @param lowerPercent The percent cut off. For the lower quartile use 25, for the upper-quartile use 75
     * @return the quartile value
     */
	public double quartile(double[] values, double lowerPercent) {
        if (values == null || values.length == 0) {
            throw new IllegalArgumentException("The data array either is null or does not contain any data.");
        }
        // order the values
        double[] v = new double[values.length];
        Arrays.sort(v);
        int n=0;
        if(v.length==1) {
        	n = 0;
        }
        else n = (int) Math.round(v.length * lowerPercent / 100);
        return v[n];
    }
}
