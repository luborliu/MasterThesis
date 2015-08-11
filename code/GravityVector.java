public class GravityVector extends TrajectoryPoint{
	private double medianDistance;      //the median distance feature
	public GravityVector(double longitude, double latitude, double COG, double SOG, double medianDistance) {
		this.latitude = latitude;
		this.longitude = longitude;
		this.COG = COG;
		this.SOG = SOG;
		this.medianDistance = medianDistance;
	}
    //generate getter and setter functions ...
}
