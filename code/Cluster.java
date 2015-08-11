public class Cluster {
	private ArrayList<TrajectoryPoint> cluster ; 
	private double avgCOG;	//the average direction (COG) of the whole cluster
    public Cluster() {}    
    public double calculateAverageDirection() {
        double sum = 0;
		for(int i=0; i<this.cluster.size();i++) {
			sum = sum+this.cluster.get(i).getCOG();
		}		
		double avg = sum/(double)(this.cluster.size());
		return avg;		
	}
    //generate getter and setter functions ...
}
