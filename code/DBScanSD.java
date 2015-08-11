public class DBScanSD {
    //final clustering results, a global variable
    private ArrayList<Cluster> resultClusters = new ArrayList<Cluster>();
    /**
     * Apply DBSCANSD on the data set. If the data is stoppoing points, set isStopPoint as true and it will execute the original DBSCAN directly. If the data is moving points, set isStopPoint as false and it will execute DBSCANSD considering speed and direction.
     */
    public ArrayList<Cluster> applyDBScanSD(ArrayList<TrajectoryPoint> pointsList, double eps, int minPoints, double maxSpd, double maxDir, boolean isStopPoint) {
	    for(int index=0;index<pointsList.size();index++) {	    	
	    	//we should mark the point as visited, it has no problem because here we use a for loop, it can stop
	        ArrayList<TrajectoryPoint> tmpLst = new ArrayList<TrajectoryPoint>();
	        TrajectoryPoint p = pointsList.get(index);
	        if(p.isVisited()&&index!=(pointsList.size()-1)&&index%4096!=0) continue;
	        tmpLst = isCorePoint(pointsList, p, eps, minPoints, maxSpd, maxDir, isStopPoint);
	        if(tmpLst!=null||index==(pointsList.size()-1)||index%4096==0){
	        	Cluster c = new Cluster();
	        	c.setCluster(tmpLst);
	        	if(tmpLst!=null)  resultClusters.add(c);
	        	int length=resultClusters.size();
				boolean flag = true;
				if((index%4096==0)||(index==(pointsList.size()-1)))  { 					
					while(flag) {
						flag = false;
					    for(int i=0;i<length;i++){
                            for(int j=0;j<length;j++){
                                if(i!=j){
                                    if(i == length) {
                                        flag = true;
                                        continue;
                                    }
                                    if(mergeClusters(resultClusters.get(i), resultClusters.get(j))) {
                                        resultClusters.remove(j);
                                        j--;
                                        length--;
                                    }
                                }
                            }
                        }
					}
				}
	        }
        }
        return resultClusters;
	}
	/**
     * Merge two clusters into one cluster
     */
	public boolean mergeClusters(Cluster clusterA, Cluster clusterB) {
		boolean merge = false;
		if(clusterA.getCluster() == null || clusterB.getCluster() == null) {
			return merge;
		}
		for(int index = 0; index < clusterB.getCluster().size(); index++) {
			TrajectoryPoint p = clusterB.getCluster().get(index);
			if(p.isCorePoint() && clusterA.getCluster().contains(p)) {
				merge = true;
				break;
			}
		}
		if(merge) {
			for(int index=0; index<clusterB.getCluster().size();index++) {
				if(!clusterA.getCluster().contains(clusterB.getCluster() .get(index))) {
					clusterA.getCluster().add(clusterB.getCluster().get(index));
				}
			}			
		}
		return merge;
	}
	/**
     * Decide if the point p is core point, if yes, return the list with p and its neighbors, if no, return null.
     */
	public ArrayList<TrajectoryPoint> isCorePoint(ArrayList<TrajectoryPoint> lst, TrajectoryPoint p, double eps, int minPoints, double maxSpd, double maxDir, boolean isStopPoint) {
		int count = 0;
		ArrayList<TrajectoryPoint> tmpList = new ArrayList<TrajectoryPoint>();
		for(Iterator<TrajectoryPoint> it = lst.iterator(); it.hasNext();) {
			TrajectoryPoint q = it.next();
			if(isDensityReachable(p, q, eps, minPoints, maxSpd, maxDir, isStopPoint)) {
				count++;
				if(!tmpList.contains(q)) {
					tmpList.add(q);
				}				
			}		
		}
		if(count>=minPoints) {
			p.setCorePoint(true);
			p.setVisited(true);
			return tmpList;
		}
		return null;
	}
    /**
     * Decide if the two points are density reachable.
     */
    public boolean isDensityReachable(TrajectoryPoint p1, TrajectoryPoint p2, double eps, int minPts, double maxSpd, double maxDir, boolean isStopPoint) {
        boolean result = false;
        if(gpsDistance(p1.getLatitude(), p1.getLongitude(), p2.getLatitude(), p2.getLongitude()) <=eps) {
            //if they are stopping points, we can directly use original DBSCAN algorithm without considering speed or direction
            if(isStopPoint) return true;
            if(Math.abs(p1.getCOG()-p2.getCOG())<maxDir) {
                if(Math.abs(p1.getSOG()-p2.getSOG())<maxSpd) {
                    result = true;
                }
            }			
        }		
        return result;		
    }
    /**
     * calcualte the gps distance between two trajectory points considering the curve of the earth.
     */
    public static double gpsDistance(double lat1, double lng1, double lat2, double lng2) {
        double earthRadius = 3958.75;
        double dLat = Math.toRadians(lat2-lat1);
        double dLng = Math.toRadians(lng2-lng1);
        double a = Math.sin(dLat/2) * Math.sin(dLat/2) +
        Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) *
        Math.sin(dLng/2) * Math.sin(dLng/2);
        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
        double dist = earthRadius * c;
        int meterConversion = 1609;
        return (double) (dist * meterConversion);
    }
}