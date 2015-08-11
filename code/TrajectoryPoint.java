public class TrajectoryPoint {
	private String mmsi;				//mmsi, the id of the vessel
	private long timestamp;				//second of UTC time stamp
	//the following protected variables are to be inherited by GV
	protected double longitude;			//longitude
	protected double latitude;			//latitude
	protected double SOG;				//speed over ground
	protected double COG;				//course over ground
	//the following two are useful during the clustering process
	private boolean isVisited;			//whether it has been visited
	private boolean isCorePoint;		//whether it is a core point
	public TrajectoryPoint() {		
		this.isVisited = false; 		//initialize the point unvisited
	}
    //generate getter and setter functions ...
}
