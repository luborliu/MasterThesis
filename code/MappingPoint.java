public class MappingPoint extends TrajectoryPoint{
	private double mappingtude; //mapping point's coordinate on the exact axis
    /**
     * Mapping Point is the point that is mapped onto the axis (average direction of the whole cluster)
     */
	public MappingPoint(double longitude, double latitude, double mappingtude, double COG, double SOG) {
		this.longitude = longitude;
		this.latitude = latitude;
		this.mappingtude = mappingtude;
		this.COG = COG;
		this.SOG = SOG;
	}
    /**
     * map the trajectory point to the particular axis and return the mapping point
     */
	public MappingPoint convertPointToMappingPoint(TrajectoryPoint p, double avgCOG) {
		double mappingtude = 0;
		double angle = (avgCOG/(double)180) * Math.PI;
		if((avgCOG>=0&&avgCOG<90)) {
			mappingtude = (p.getLongitude() + (1.0/Math.tan(angle)) * p.getLatitude()) * Math.sin(angle);
		} else if((avgCOG >= 270&&avgCOG<360)) {
			mappingtude = (p.getLatitude() - (Math.tan(Math.PI*2-angle))* p.getLongitude()) * Math.cos(Math.PI*2 - angle);
		} else if(avgCOG >= 90&&avgCOG<180) {
			mappingtude = ((Math.tan(Math.PI-angle))*p.getLongitude() - p.getLatitude()) * Math.cos(Math.PI - angle);
		} else if(avgCOG >= 180&&avgCOG<270) {
			mappingtude = -(((double)1/Math.tan(angle - Math.PI)) * p.getLatitude() + p.getLongitude()) * Math.sin(angle - Math.PI);
		}
		MappingPoint mp = new MappingPoint(p.getLongitude(), p.getLatitude(), mappingtude, p.getCOG(), p.getSOG());
		return mp;
	}
	// generate setter and getter functions ...
}
