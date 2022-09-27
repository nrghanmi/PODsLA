# PODsLA: Points of Dispensing Location-Allocation Approach during Public Health Emergencies
This repository contains a code for the PODsLA approach for points of dispensing location-allocation based on the R language. We present this study in the paper “PODsLA: Points of Dispensing Location-allocation Approach During Public Health Emergencies.”

#### **Repository content**
------------------
Data: contains the data of demand points (locations, population size and land area) as  areas.csv.

PODsLA: contains PODsLA.R for the proposed approach and weighted_distance.R to calculate the weighted distance.

Results: contains Excel files for demand points and population coverage over different distances. Also, it includes Figures to plot the distribution of demand points and PODs and to show the comparison among approaches in the ablation study.

Ablation study: contains data representation and using weights folders. Data representation has S-PODsLA.R and comparison-approaches.R while using weights has PODsLA_Avg.R, PODsLA_Max.R and comparison-weights.R. 

#### **Data**
------------------
The code for the approach is general and can be used with the data that consists of demand points, location, and population size. 
