## label the input data with normal moving clusters(GVs) and normal stopping clusters(SSPs)
labelAnomalyPointsWithStopMoveclusters<-function(inputData, normalPointsMove, normalPointsStop) {
  labeledData<-inputData;
  labeledData[,"label"]<-labeledData[,1];
  labeledData[,"label"]<-1;
  #calulate features for the 1st step
  #extract those incomplete data
  end=nrow(labeledData);
  for(i in 1:end) {
    if(!complete.cases(labeledData[i,])) { 
      labeledData[i,"label"]<-9;
    }
  }
  print("Incomplete records labeled finished")
  featuresOne <- generatefeaturesRelativeDistance(inputData, normalpointsMove = normalPointsMove, normalpointsStop = normalPointsStop);
  #below thresholds are for Juan De Fuca strait in paper of "Knowledge-based clustering of ship trajectories using density-based approach"
  rel_dis_threshold = 5.765;
  abs_dis_threshold = 94.096
  #get indices for far points
  index_far_relative <- getIndexOfFarPointsRELATIVE(featuresOne, rel_dis_threshold,abs_dis_threshold = abs_dis_threshold);
  if(nrow(index_far_relative)>0) {
    for(m in 1:nrow(index_far_relative)) {
      if(labeledData[index_far_relative[m,1],"label"]!=9) {
        labeledData[index_far_relative[m,1],"label"] <-0; 
      }
    }   
  }
  print("First step finished.")
  #2nd step
  index_near_relative <- getIndexOfNearPointsRELATIVE(featuresOne, rel_dis_threshold, abs_dis_threshold);
  if(nrow(index_near_relative)>0) {
    featureSecond<-generatefeatures4(inputData [index_near_relative [,1],], normalPoints, rel_dis_threshold);
    cosineThreshold=0.588
    end2 = nrow(featureSecond);
    for(j in 1:end2) {
      if(featureSecond[j,]$cosSimilarity<cosineThreshold) {
        labeledData[index_near_relative[featureSecond[j,]$INDEX,1], "label"] <- (-1);
      }
    }
  }  
  print("Data label process finished!")
  return(labeledData);
}
## generate features considering relative distance
## realpoints: the point to be labeled
## normalpointsMove: moving clusters (GVs)
## normalpointsStop: stopping clusters (SSPs)
generatefeaturesRelativeDistance <- function(realpoints, normalpointsMove, normalpointsStop) {
    end = nrow(realpoints);
    features = data.frame (distance=numeric(), relative_distance=numeric(), SOGRatio=numeric(), COG=numeric(), isStopPoint=integer());
    for(i in 1:end) {
        features = rbind(features,calculateDistancesRelative(realpoints[i,], normalpointsMove, normalpointsStop));
    }
    colnames(features) <- c("Absolute_Distance", "Relative_Distance", "SOGratio", "COG", "isStopPoint");
    return(features);
}
## calculate the relative distances between the point and the clusters
## realpoints: the point to be labeled
## normalpointsMove: moving clusters (GVs)
## normalpointsStop: stopping clusters (SSPs)
calculateDistancesRelative<- function(realpoint, normalpoints, normalpointsStop) {
    index = -1;
    min_d = 1000000000000000;
    relative_distance = 1000000000000000;
    end = nrow(normalpoints);
    end2=nrow(normalpointsStop);
    isStopPoint = 0;
    #use 0.5 as stopping point condition, we can change it to another speed threshold
    if(realpoint[1,"SOG"]<=0.5 | !complete.cases(realpoint[1,])) {
        isStopPoint = 1;
        for( i in 1:end2) {
            #actually here I should use absolute distance instead of relative_distance
            #I am too lazy to do this and this will not influence the result, just a name
            distance_tmp = gpsdist(realpoint[1,"Latitude"], realpoint[1,"Longitude"], normalpointsStop[i,"Latitude"], normalpointsStop[i,"Longitude"]);
            if(distance_tmp < min_d) {
                relative_distance<-999999999;#stands for NA
                min_d <-distance_tmp;
                index <- i;
            }
        }
    } else {
        for(i in 1:end) {
            distance = gpsdist(realpoint[1, "Latitude"], realpoint[1, "Longitude"], normalpoints[i, "Latitude"], normalpoints[i, "Longitude"]);
            quartileDistance=normalpoints[i, "QuartileDistance"];
            if(quartileDistance==0) {
                quartileDistance=1; #to avoid the x/0 error
            }
            relative_dis_tmp = distance/quartileDistance;
            if(relative_dis_tmp < relative_distance) {
                relative_distance<-relative_dis_tmp
                min_d <-distance;
                index <- i;
            }
        }
    }
    speedratio <- (abs(realpoint[1,"SOG"] - normalpoints[index,"SOG"]))/ normalpoints[index,"SOG"];
    direction_dif<- abs(realpoint[1,"COG"] - normalpoints[index,"COG"]);
    
    return (c(min_d, relative_distance,speedratio, direction_dif, isStopPoint ));
}
## used for calculating the geographical distance of two positions
## lat1: latitude of point 1
## lon1: longitude of point 1
## lat2: latitude of point 2
## lon2: longitude of point 2
gpsdist<-function(lat1,lon1,lat2,lon2) {
    R = 6367000;
    dlat = (lat2-lat1)*pi/180;
    dlon = (lon2-lon1)*pi/180;
    radlat1 = lat1*pi/180;
    radlat2 = lat2*pi/180;
    a = sin(dlat/2) * sin(dlat/2) + (sin(dlon/2) * sin(dlon/2) * cos(radlat1) * cos(radlat2));
    c = 2 * atan2(sqrt(a), sqrt(1-a));
    d = R * c;
    d;
}
## get the indices of those points which are too far away
## test_set: the data set to be labeled
## dis is the threshold for the distance
getIndexOfFarPoints <- function(test_set, dis) {
    end = nrow(test_set)
    index = data.frame(index=numeric());
    for(i in 1:end) {
        if(test_set[i,]$distance>dis) {
            index <- rbind(index,i);
        }
    }
    return(index);
}
## get the indices of the points that are near the clusters
## they can be used for training or predicting the speed&direction abnormal thing
getIndexOfNearPoints <- function(test_set, dis) {
    end = nrow(test_set)
    index = data.frame(index=numeric());
    for(i in 1:end) {
        if(test_set[i,]$distance<=dis) {
            index <- rbind(index,i);
        }
    }
    return(index);
}
## get the indices of the points that are near the clusters relatively
## they can be used for training or predicting the speed&direction abnormal thing
#this is based on relative distance
getIndexOfNearPointsRELATIVE <- function(test_set, relative_dis_threshold, abs_dis_threshold) {
    end = nrow(test_set)
    index = data.frame(index=numeric());
    for(i in 1:end) {
        if(test_set[i,"isStopPoint"]==1) {
        } else {
            if(test_set[i,]$Relative_Distance <= relative_dis_threshold) {
                index <- rbind(index,i);
            }
        }
    }
    return(index);
}
## points far away from normal points
## this is based on relative distance
## relative_dis_threshold: the threshold for moving points
## abs_distance: the threshold for stopping points
getIndexOfFarPointsRELATIVE <- function(test_set, relative_dis_threshold, abs_dis_threshold) {
    end = nrow(test_set)
    index = data.frame(index=numeric());
    for(i in 1:end) {
        if(test_set[i,"isStopPoint"]==1) {
            if(test_set[i,]$Absolute_Distance > abs_dis_threshold) {
                index<-rbind(index,i);
            }
        } else {
            if(test_set[i,]$Relative_Distance > relative_dis_threshold) {
                index <- rbind(index,i);
            }
        }
    }
    return(index);
}
## generate the features for the second labelling step: cosine division distance
generatefeatures4 <- function(realpoints, normalpoints, threshold_relative) {
    end = nrow(realpoints);
    features = data.frame(relativeDistance=numeric(), distanceSD=numeric(), distance=numeric(),cosSimilarity=numeric(), distanceSimilarity=numeric(), SOG=numeric(), COG=numeric(), INDEX=numeric());
    print("Start to label cosine division distances")
    for(i in 1:end) {
        index = i;
        if(complete.cases(realpoints[i,])) {
            tmp<-calculateDistances4(realpoints[i,], index, normalpoints, threshold_relative);
            features <- rbind(features,tmp);
        }
    }
    colnames(features) <- c("relativeDistance", "distanceSD", "distance", "cosSimilarity", "distanceSimilarity", "SOGdif", "COGdif", "INDEX");
    #distanceSD minimum distance considering speed and direction
    return(features);
}
## subroutine in function of "generatefeatures4"
calculateDistances4<- function(realpoint, index, normalpoints, threshold) {
    min_relativeDistance = 1000000000; # used for storing the relative distance considering speed&direction
    min_d = 100000000000;       #used for storing the distance to the nearest points considering speed&direction
    cosSimilarity = -1000000;   # cosineSimilarity
    direction_dif<-100000000;  # direction difference
    speed <- 100000000;        # speed difference
    min_distance <- 10000000000;#used for storing the nearest points without considering speed&direction
    distanceSimilarity <- 0;
    end = nrow(normalpoints);
    for(i in 1:end) {
        distance = gpsdist(realpoint[1, "Latitude"], realpoint[1, "Longitude"], normalpoints[i, "Latitude"], normalpoints[i, "Longitude"]);
        relative_distance = distance/(normalpoints[i, "QuartileDistance"] + 0.00000001);
        # below is to record the nearest point in the space without considering speed & direction
        if(distance<min_distance) {
            min_distance = distance;
        }
        if(relative_distance < threshold) {
            min_relativeDistance=relative_distance;
            # consider those points with small speed in the stop area as normal, that is, no need to consider direction parameter(set its difference to 0)
            if(realpoint[1,"SOG"]<=0.3& normalpoints[i,"SOG"]<=0.3) {
                if(distance<min_d) {
                    min_d <- distance;
                }
                speed <- abs(realpoint[1,"SOG"]-normalpoints[i,"SOG"]);
                direction_dif <- 0;
                
                cosSimilarity = 1;
            }
            else {
                if(realpoint[1,"SOG"]>normalpoints[i,"SOG"]) {
                    speedRatio <- normalpoints[i,"SOG"]/realpoint[1,"SOG"];
                    cosSimilarityTmp <- cos(((abs(realpoint[1,"COG"] - normalpoints[i,"COG"]))/180)*pi) * speedRatio;
                } else {
                    speedRatio <- realpoint[1,"SOG"]/normalpoints[i,"SOG"];
                    cosSimilarityTmp <- cos(((abs(realpoint[1,"COG"] - normalpoints[i,"COG"]))/180)*pi) * speedRatio;
                }
                if(cosSimilarityTmp>cosSimilarity) {
                    min_d <-distance;
                    direction_dif<- abs(realpoint[1,"COG"]-normalpoints[i,"COG"]);
                    speed<-abs(normalpoints[i,"SOG"]-realpoint[1,"SOG"]);
                    cosSimilarity<- cosSimilarityTmp;
                }
            }
        }
        distanceSimilarity <- (min_distance+1)/(min_d+1)
    }  
    return (c(min_relativeDistance,min_d, min_distance, cosSimilarity, distanceSimilarity, speed, direction_dif,index));
}