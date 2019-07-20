*Check_the_dataset*;
Title "Original_dataset";
proc import datafile="C:\Users\cshi4\Downloads\final\energy-usage-2010(Steve).csv" out=myd replace;
delimiter=',';
getnames=yes;
run;
*Preposscessing & data_cleaning;
data myd_prep;
set myd;
*recording the subtype from 6 to 4;
r_sub_type = .;
IF (BUILDING_SUBTYPE = 'Single Family') THEN r_sub_type = 1;
IF (BUILDING_SUBTYPE = 'Commercial') or (BUILDING_SUBTYPE = 'Industrial') THEN r_sub_type = 2;
IF (BUILDING_SUBTYPE = 'Multi < 7') or (BUILDING_SUBTYPE = 'Multi 7+') THEN r_sub_type = 3;
IF (BUILDING_SUBTYPE = 'Municipal') THEN r_sub_type = 4;
*recording the community area from 77 to 4: central(1), North Side(2), West Side(3), South Side(4);
r_com_area = .;
IF COMMUNITY_AREA_NAME in('Near North Side', 'Loop', 'Near South Side') THEN r_com_area = 1;
IF COMMUNITY_AREA_NAME in('North Center', 'Lakeview', 'Lincoln Park', 'Avondale', 'Logan Square', 'Rogers Park', 
'West Ridge', 'Uptown', 'Lincoln Square', 'Edison Park', 'Norwood Park', 'Jefferson Park', 'Forest Glen', 'North Park', 'Albany Park', 
'O_Hare', 'Edgewater', 'Portage Park', 'Irving Park', 'Dunning', 'Montclare', 'Belmont Cragin', 'Hermosa') THEN r_com_area = 2;
IF COMMUNITY_AREA_NAME in('Humboldt Park', 'West Town', 'Austin', 'West Garfield P', 'East Garfield P', 'Near West Side', 
'North Lawndale', 'South Lawndale', 'Lower West Side') THEN r_com_area = 3;
IF COMMUNITY_AREA_NAME in('Armour Square', 'Douglas', 'Oakland', 'Fuller Park', 'Grand Boulevard', 'Kenwood', 'Washington Park', 'Hyde Park', 'Woodlawn', 
'South Shore', 'Bridgeport', 'Greater Grand C', 'Garfield Ridge', 'Archer Heights', 'Brighton Park', 'McKinley Park', 'New City', 'West Elsdon', 'Gage Park',
'Clearing', 'West Lawn', 'Chicago Lawn', 'West Englewood', 'Englewood', 'Chatham', 'Avalon Park', 'South Chicago', 'Burnside', 'Calumet Heights', 'Roseland',
'Pullman', 'South Deering', 'East Side', 'West Pullman', 'Riverdale', 'Hegewisch', 'Ashburn', 'Auburn Gresham', 'Beverly', 'Washington Heig', 'Mount Greenwood', 
'Morgan Park') THEN r_com_area = 4;
*creat new data as Y var;
TOTAL_ENERGY_USAGE = TOTAL_KWH + TOTAL_THERMS*29.3;
*create dummy variables;
d_Build_Resi =(BUILDING_TYPE = 'Residential');
*create dummy variables for buildType;
d1_sub_type = (r_sub_type = 1);
d2_sub_type = (r_sub_type = 2);
d3_sub_type = (r_sub_type = 4);
*create dummy variables for areas;
d1_com_area = (r_com_area = 2);
d2_com_area = (r_com_area = 3);
d3_com_area = (r_com_area = 4);
*create an interaction term;
TOTAL_SQFT = KWH_TOTAL_SQFT*THERMS_TOTAL_SQFT;
*Drop since the Y-variablle made by TOTAL_KWH & TOTAL_THERMS;
drop TOTAL_KWH;
drop TOTAL_THERMS;
run;

*ALL preprocessing and data cleaning done with above code: recording, data-cleaing, dummy variables, missing values;
*Missing value;

*SGSCATTER;
title "matrix plot";
proc SGSCATTER;
matrix TOTAL_ENERGY_USAGE ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS KWH_TOTAL_SQFT THERMS_TOTAL_SQFT TOTAL_POPULATION TOTAL_UNITS 
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE OCCUPIED_UNITS RENTER_OCCUPIED_HOUSING_UNITS d_Build_Resi 
		d1_sub_type d2_sub_type d3_sub_type d1_com_area d2_com_area d3_com_area TOTAL_SQFT;
run;
*corr;
Title "Corr value";
proc corr;
var TOTAL_ENERGY_USAGE ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS KWH_TOTAL_SQFT THERMS_TOTAL_SQFT TOTAL_POPULATION TOTAL_UNITS 
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE OCCUPIED_UNITS RENTER_OCCUPIED_HOUSING_UNITS d_Build_Resi 
		d1_sub_type d2_sub_type d3_sub_type d1_com_area d2_com_area d3_com_area TOTAL_SQFT;
run;
*Histogram;
Title "Histogram of TOTAL_ENERGY_USAGE";
proc univariate normal;
var TOTAL_ENERGY_USAGE;
histogram / normal (mu=est sigma=est);
run;
*descriptives;
Title "descriptives for TOTAL_ENERGY_USAGE";
proc means mean stderr std range clm min p25 p50 p75 max;
var TOTAL_ENERGY_USAGE;
run;
*Transform opt1;
data myd_prep;
set myd_prep;
Title "Transform Y variable by log";
lnTOTAL_ENERGY_USAGE = log(TOTAL_ENERGY_USAGE);
run;
*recheack for log Histogram;
Title "Histogram of lnTOTAL_ENERGY_USAGE";
proc univariate normal;
var lnTOTAL_ENERGY_USAGE;
histogram / normal (mu=est sigma=est);
run;
*recheack for log Histogram;
Title "descriptives for lnTOTAL_ENERGY_USAGE";
proc means mean stderr std range clm min p25 p50 p75 max;
var lnTOTAL_ENERGY_USAGE;
run;

*Boxplot for BUILDING_TYPE;
Title "Residential vs Commercial";
ods graphics off;
proc sort;
by d_Build_Resi;
run;
proc boxplot;
plot lnTOTAL_ENERGY_USAGE*d_Build_Resi;
inset min mean max stddev/
		header = 'Overall statistics'
		pos = tm;
		insetgroup min mean Q1 Q2 Q3 max Range stddev/
		header = 'Statistics by building_type';
run;
*Second Stage;
*Start to fit the full model with interaction term;
Title "full_model";
proc reg data=myd_prep noprint;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS KWH_TOTAL_SQFT THERMS_TOTAL_SQFT TOTAL_POPULATION TOTAL_UNITS 
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE OCCUPIED_UNITS RENTER_OCCUPIED_HOUSING_UNITS d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area TOTAL_SQFT/ stb tol vif influence r;

	output out=testdataset r=Residual cookd=Influential;
plot residual.*predicted.;
plot npp.*residual.;
run;
Title "removing outliers";
data testdataset;
set testdataset;
if (lnTOTAL_ENERGY_USAGE < 8) then delete;
if (lnTOTAL_ENERGY_USAGE > 20) then delete;
run;

title "rerun model without outliers";
proc reg data = testdataset;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS KWH_TOTAL_SQFT THERMS_TOTAL_SQFT TOTAL_POPULATION TOTAL_UNITS 
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE OCCUPIED_UNITS RENTER_OCCUPIED_HOUSING_UNITS d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area TOTAL_SQFT/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*recheack for discriptive Histogram;
Title "descriptives for lnTOTAL_ENERGY_USAGE";
proc means mean stderr std range clm min p25 p50 p75 max;
var lnTOTAL_ENERGY_USAGE;
run;
*Muticollinearity stage: total 6 variables have muticollinearity issue: TOTAL_UNITS, OCCUPIED_UNITS, 
*RENTER_OCCUPIED_HOUSING_UNITS, TOTAL_POPULATION, KWH_TOTAL_SQFT, and THERMS_TOTAL_SQFT;
*Firstrecheck the corr;
Title "Corr value";
proc corr;
var lnTOTAL_ENERGY_USAGE ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS KWH_TOTAL_SQFT THERMS_TOTAL_SQFT TOTAL_POPULATION TOTAL_UNITS 
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE OCCUPIED_UNITS RENTER_OCCUPIED_HOUSING_UNITS d_Build_Resi 
		d1_sub_type d2_sub_type d3_sub_type d1_com_area d2_com_area d3_com_area TOTAL_SQFT;
run;
*Second centered the multicollinearty variable;
Title "central the vars";
data testdataset;
set testdataset;
TOTAL_UNITS_C = TOTAL_UNITS - 47.34072;
OCCUPIED_UNITS_C = OCCUPIED_UNITS - 41.53317;
RENTER_OCCUPIED_HOUSING_UNITS_C = RENTER_OCCUPIED_HOUSING_UNITS - 24.79391;
TOTAL_POPULATION_C = TOTAL_POPULATION - 103.98434;
KWH_TOTAL_SQFT_C = KWH_TOTAL_SQFT - 23455;
THERMS_TOTAL_SQFT_C = THERMS_TOTAL_SQFT - 22999;
run;
*Then Go back to the full model to check multicollinearty problem;
Title "Full model with centered vaiables";
proc reg data = testdataset;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area TOTAL_SQFT 
		TOTAL_UNITS_C OCCUPIED_UNITS_C RENTER_OCCUPIED_HOUSING_UNITS_C TOTAL_POPULATION_C KWH_TOTAL_SQFT_C
		THERMS_TOTAL_SQFT_C/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*Since the centered method doesn't work in my case,tring to drop them directly;
*"Drop the collinearity var TOTAL_UNITS";
data afterDrop;
set testdataset;
drop TOTAL_UNITS_C;
run;
title "rerun model after drop TOTAL_UNITS";
proc reg data = testdataset;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area TOTAL_SQFT 
		OCCUPIED_UNITS_C RENTER_OCCUPIED_HOUSING_UNITS_C TOTAL_POPULATION_C KWH_TOTAL_SQFT_C
		THERMS_TOTAL_SQFT_C/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*does not hurt adjR2, keep going;

*"Drop the collinearity var OCCUPIED_UNITS";
data afterDrop;
set testdataset;
drop OCCUPIED_UNITS_C;
run;
title "rerun model after drop TOTAL_UNITS_C, OCCUPIED_UNITS_C";
proc reg data = afterDrop;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area TOTAL_SQFT 
		RENTER_OCCUPIED_HOUSING_UNITS_C TOTAL_POPULATION_C KWH_TOTAL_SQFT_C
		THERMS_TOTAL_SQFT_C/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*Decide to keep the OCCUPIED_UNITS_C, since the R2 decrease signifcantly;
*then return the model and data set with afer dropping the TOTAL_UNITS;

*Drop more collinearty varaible: RENTER_OCCUPIED_HOUSING_UNITS;
data afterDrop;
set testdataset;
drop RENTER_OCCUPIED_HOUSING_UNITS_C;
run;
Title "rerun model without outliers and dropped TOTAL_UNITS and ENTER_OCCUPIED_HOUSING_UNITS_C";
proc reg data = testdataset;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area TOTAL_SQFT 
		OCCUPIED_UNITS_C TOTAL_POPULATION_C KWH_TOTAL_SQFT_C
		THERMS_TOTAL_SQFT_C/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*Great, does not hurt adjR2, keep going;

*"Drop the collinearity var TOTAL_POPULATION_C";
data afterDrop;
set testdataset;
drop TOTAL_POPULATION_C;
run;
*Double Check the model after dropped RENTER_OCCUPIED_HOUSING_UNITS, TOTAL_UNITS;
Title "rerun model without outliers and dropped TOTAL_UNITS,ENTER_OCCUPIED_HOUSING_UNITS_C,TOTAL_POPULATION_C";
proc reg data = afterDrop;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area TOTAL_SQFT 
		OCCUPIED_UNITS_C KWH_TOTAL_SQFT_C
		THERMS_TOTAL_SQFT_C/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*R2 just decreases 0.001, after droopping TOTAL_POPULATION, it is acceptable ;

*Then try to drop KWH_TOTAL_SQFT since it insignifncant and has collinearity;
data afterDrop;
set testdataset;
drop KWH_TOTAL_SQFT_C;
run;
*Double Check the model after dropped RENTER_OCCUPIED_HOUSING_UNITS, TOTAL_UNITS, TOTAL_POPULATION, KWH_TOTAL_SQFT;
Title "rerun model without outliers and dropped TOTAL_UNITS,RENTER_OCCUPIED_HOUSING_UNITS,TOTAL_POPULATION,KWH_TOTAL_SQFT_C";
proc reg data = afterDrop;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area TOTAL_SQFT 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;

**Brilliant! adj-R2 didn't change and remain on the 0.5099 and without muticollinearty issues;
*Recheck the scatterPlot about residuals of each variables, if they have been violated, do the polynomial;

*SGSCATTER;
title "matrix plot for clean mdoel";
proc SGSCATTER;
matrix lnTOTAL_ENERGY_USAGE ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area TOTAL_SQFT 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C;
run;
*the residuals of X variables have been violated, doing the polynomial;
*create polynomial term;
data afterDrop;
set afterDrop;
ELECTRICITY_ACCOUNTS2 = ELECTRICITY_ACCOUNTS**2;
ZERO_KWH_ACCOUNTS2 = ZERO_KWH_ACCOUNTS**2;
GAS_ACCOUNTS2= GAS_ACCOUNTS**2;
AVERAGE_STORIES2 = AVERAGE_STORIES**2;
AVERAGE_BUILDING_AGE2 = AVERAGE_BUILDING_AGE**2;
AVERAGE_HOUSESIZE2 = AVERAGE_HOUSESIZE**2;
TOTAL_SQFT2 = TOTAL_SQFT**2;
OCCUPIED_UNITS_C2 = OCCUPIED_UNITS_C**2;
THERMS_TOTAL_SQFT_C2 = THERMS_TOTAL_SQFT_C**2;
run;
*MODEL WITH POLYNOMIAL TERM;
TITLE "MODEL WITH POLYNOMIAL TERM";
proc reg;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area TOTAL_SQFT 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*adjR2 been approved from 0.5099 to 0.6026. polynomial is necessary, but cause the multicollinearty issue, center them again;
*Second time to centered the multicollinearty variable;
**Muticollinearity: total 14 variables have muticollinearity issue: TOTAL_SQFT, THERMS_TOTAL_SQFT_C2,OCCUPIED_UNITS_C2,OCCUPIED_UNITS_C, ELECTRICITY_ACCOUNTS2, AVERAGE_HOUSESIZE, 
					TOTAL_SQFT2, AVERAGE_HOUSESIZE2, THERMS_TOTAL_SQFT_C, ELECTRICITY_ACCOUNTS, ZERO_KWH_ACCOUNTS2, AVERAGE_BUILDING_AGE, AVERAGE_BUILDING_AGE2, GAS_ACCOUNTS2;

*get the mean value from each multicollinearty variable;
Title "mean for center";
proc means mean stderr std range clm min p25 p50 p75 max;
var TOTAL_SQFT THERMS_TOTAL_SQFT_C2 OCCUPIED_UNITS_C2 OCCUPIED_UNITS_C ELECTRICITY_ACCOUNTS2 AVERAGE_HOUSESIZE 
					TOTAL_SQFT2 AVERAGE_HOUSESIZE2 THERMS_TOTAL_SQFT_C ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS2 AVERAGE_BUILDING_AGE AVERAGE_BUILDING_AGE2 GAS_ACCOUNTS2;
run;

Title "central the vars";
data afterDrop;
set afterDrop;
TOTAL_SQFT_C = TOTAL_SQFT - 23176790416;
THERMS_TOTAL_SQFT_C3 = THERMS_TOTAL_SQFT_C2 - 22268922375;
OCCUPIED_UNITS_C3 = OCCUPIED_UNITS_C2 - 109506.04;
OCCUPIED_UNITS_C1 = OCCUPIED_UNITS_C + (4.1786);
ELECTRICITY_ACCOUNTS3 = ELECTRICITY_ACCOUNTS2 - 2029.68;
AVERAGE_HOUSESIZE3 = AVERAGE_HOUSESIZE - 3.4642484;
TOTAL_SQFT3 = TOTAL_SQFT2 - 1.3338644;
AVERAGE_HOUSESIZE31 = AVERAGE_HOUSESIZE2 - 703.7381068;
THERMS_TOTAL_SQFT_C31 = THERMS_TOTAL_SQFT_C - 0.0140151;
ELECTRICITY_ACCOUNTS32 = ELECTRICITY_ACCOUNTS - 13.6985265;
ZERO_KWH_ACCOUNTS3 = ZERO_KWH_ACCOUNTS2 - 379.1599618;
AVERAGE_BUILDING_AGE3 = AVERAGE_BUILDING_AGE - 72.0718504;
AVERAGE_BUILDING_AGE23 = AVERAGE_BUILDING_AGE2 - 6326.25;
GAS_ACCOUNTS3 = GAS_ACCOUNTS2 - 352.4064398;
run;
*RERUN THE MODEL FOR CHECK MUTIL;
Title "rerun model without outliers WITH polynomial with SECOND CENTER";
proc reg;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area TOTAL_SQFT 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2
		TOTAL_SQFT_C THERMS_TOTAL_SQFT_C3 OCCUPIED_UNITS_C3 OCCUPIED_UNITS_C1 ELECTRICITY_ACCOUNTS3 AVERAGE_HOUSESIZE3 TOTAL_SQFT3 AVERAGE_HOUSESIZE31 THERMS_TOTAL_SQFT_C31
		ELECTRICITY_ACCOUNTS32 ZERO_KWH_ACCOUNTS3 AVERAGE_BUILDING_AGE3 AVERAGE_BUILDING_AGE23 GAS_ACCOUNTS3/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*The center would not help eliminaing multi issues, start to drop;
*change the dataset name from original afterDrop to afterDrop2;
data afterDrop2;
set afterDrop;
drop TOTAL_SQFT;
run;
Title "rerun model without outliers WITH polynomial drop TOTAL_SQFT ";
proc reg;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*great, keep drop OCCUPIED_UNITS_C2;
data afterDrop3;
set afterDrop2;
drop OCCUPIED_UNITS_C2;
run;
Title "rerun model without outliers WITH polynomial drop TOTAL_SQFT OCCUPIED_UNITS_C2";
proc reg;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 THERMS_TOTAL_SQFT_C2/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*Hurt R2, keep OCCUPIED_UNITS_C2, back to model with OCCUPIED_UNITS_C2;
data afterDrop3;
set afterDrop2;
drop OCCUPIED_UNITS_C;
run;
Title "rerun model without outliers WITH polynomial drop TOTAL_SQFT OCCUPIED_UNITS_C";
proc reg;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*Hurt R2, keep OCCUPIED_UNITS_C2 OCCUPIED_UNITS_C, back to model without TOTAL_SQFT and drop ELECTRICITY_ACCOUNTS2 ;
data afterDrop3;
set afterDrop2;
drop ELECTRICITY_ACCOUNTS2;
run;
Title "rerun model without outliers WITH polynomial drop TOTAL_SQFT,ELECTRICITY_ACCOUNTS2";
proc reg;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*Hurt R2, keep OCCUPIED_UNITS_C2 OCCUPIED_UNITS_C,ELECTRICITY_ACCOUNTS2, back to model without TOTAL_SQFT and start drop THERMS_TOTAL_SQFT_C2;
data afterDrop3;
set afterDrop2;
drop THERMS_TOTAL_SQFT_C2;
run;
Title "rerun model without outliers WITH polynomial drop TOTAL_SQFT,THERMS_TOTAL_SQFT_C2";
proc reg;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 OCCUPIED_UNITS_C2/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*Hurt R2, keep OCCUPIED_UNITS_C2 OCCUPIED_UNITS_C,ELECTRICITY_ACCOUNTS2, THERMS_TOTAL_SQFT_C2, back to model without TOTAL_SQFT and start drop AVERAGE_HOUSESIZE;
data afterDrop3;
set afterDrop2;
drop AVERAGE_HOUSESIZE;
run;
Title "rerun model without outliers WITH polynomial drop TOTAL_SQFT,AVERAGE_HOUSESIZE";
proc reg;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*Since drop multicollinearity var will hurt adjR2, so we decide to keep them in our model;
*doublecheck the matrix plot;
*So we decide to use model only dropped TOTAL_SQFT as our best full model with dataset afterDrop2;
Title "Best full model";
proc reg data = afterDrop2;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2/ stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
Title "matrix for Best full model";
proc SGSCATTER data = afterDrop2;
matrix lnTOTAL_ENERGY_USAGE ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2;
run;
*After addressing outliers & influential point,multicollinearity. 
we are going to next stage: start to fit the final model by splite dataset;
*Create train set & test set;
Title "split dataset as train & test";
proc surveyselect data = afterDrop2 out = myd_prep seed = 8368585 
samprate = 0.75 outall;
run;
data myd_prep;
set myd_prep;
if Selected then new_y_total = lnTOTAL_ENERGY_USAGE;
run;
*Using trainset as dependent variable;
*Final model stage:Start to fit final model with dataset finalset;
data finalset;
set myd_prep;
run;
title "model selection";
proc reg data = finalset;
*model_1;
model new_y_total = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 
		TOTAL_SQFT2 OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2/ selection = backward sle=0.05 stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;

proc reg data = finalset;		
*model_2;
model new_y_total = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 
		TOTAL_SQFT2 OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2/ selection = stepwise sle=0.05 stb tol vif;

plot residual.*predicted.;
plot npp.*residual.;
run;
*EXACTLY SAME RESULY! PERFECT;
*Start tp test proformance of two methods, computing regression for train and predicted for test;
title "Val_testSet";
proc reg data = finalset;
*model;
model new_y_total = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 
		TOTAL_SQFT2 OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2/ sle=0.05;

		output out=outm1(where=(new_y_total=.)) p=yhat;
*mode2;
model new_y_total = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 
		TOTAL_SQFT2 OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2/ sle=0.05;
		
		output out=outm2(where=(new_y_total=.)) p=yhat;
run;
*check RMSE, MAE, ADJR2, CVR2 between 2 tesdset;
*the result exactly same;
*computing performance state;
title "Diff bet obs & pred in test for model1";
data outm1_sum;
set outm1;
d=lnTOTAL_ENERGY_USAGE-yhat;
absd = abs(d);
run;
*Computing predictive statistics: RMSE AND MAE;
proc summary data=outm1_sum;
var d absd;
output out=outm1_stats std(d)=rmse mean(absd)=mae;
run;
title "corr for test m1";
proc corr data=outm1;
var lnTOTAL_ENERGY_USAGE yhat;
run;
title "Diff bet obs & pred in test for model2";
data outm2_sum;
set outm2;
d=lnTOTAL_ENERGY_USAGE-yhat;
absd = abs(d);
run;
*Computing predictive statistics: RMSE AND MAE;
proc summary data=outm2_sum;
var d absd;
output out=outm2_stats std(d)=rmse mean(absd)=mae;
run;
title "corr for test m2";
proc corr data=outm2;
var lnTOTAL_ENERGY_USAGE yhat;
run;
*From predictive coomputing, 2 models performance are exactly same!;
*Start the Cross-Validation;
*first use the dataset best fullmodel;
Title "Cross-Validation M1";
ods graphics on;
proc glmselect data=afterDrop2
plots=(asePlot Criteria);
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 
		OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2/selection = backward(stop=cv) cvMethod=split(5) cvDetails=all; 
run;
*apply 5-fold crossvalidation with stepwise selection and 25% of data removed for testing;
title "5-fold crossvalidation + 25% testing set";
ods graphics on;
proc glmselect data=afterDrop2 plots=(asePlot Criteria);
* partition defines a test set (25% of data) to validate model on
* new data;
partition fraction(test=0.25);
* selection=stepwise uses stepwise selection method;
* stop=cv: minimizes prediction residual sum of squares for 
* variable selection;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 
		OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2/selection = backward(stop=cv) cvMethod=split(5) cvDetails=all; 
run;

Title "Cross-Validation M2";
ods graphics on;
proc glmselect data=afterDrop2
plots=(asePlot Criteria);
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 
		OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2/selection = stepwise(stop=cv) cvMethod=split(5) cvDetails=all; 
run;
*apply 5-fold crossvalidation with stepwise selection and 25% of data removed for testing;
title "5-fold crossvalidation + 25% testing set";
ods graphics on;
proc glmselect data=afterDrop2 plots=(asePlot Criteria);
* partition defines a test set (25% of data) to validate model on
* new data;
partition fraction(test=0.25);
* selection=stepwise uses stepwise selection method;
* stop=cv: minimizes prediction residual sum of squares for 
* variable selection;
model lnTOTAL_ENERGY_USAGE = ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS
		AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi d1_sub_type d2_sub_type d3_sub_type 
		d1_com_area d2_com_area d3_com_area 
		OCCUPIED_UNITS_C
		THERMS_TOTAL_SQFT_C
		ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 
		OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2/selection = stepwise(stop=cv) cvMethod=split(5) cvDetails=all; 
run;
*Conclusion: the final should be the backward or stepwise in model-validation and dropped d1_com_area.
*Total in 23 var;
*Last step to compute prediction;
*Assume the AVERAGE_BUILDING_AGE is 65;
data pred;
input lnTOTAL_ENERGY_USAGE ELECTRICITY_ACCOUNTS ZERO_KWH_ACCOUNTS GAS_ACCOUNTS AVERAGE_STORIES AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE d_Build_Resi 
		d1_sub_type d2_sub_type d3_sub_type d2_com_area d3_com_area OCCUPIED_UNITS_C THERMS_TOTAL_SQFT_C ELECTRICITY_ACCOUNTS2 ZERO_KWH_ACCOUNTS2 GAS_ACCOUNTS2 
		AVERAGE_STORIES2 AVERAGE_BUILDING_AGE2 AVERAGE_HOUSESIZE2 TOTAL_SQFT2 OCCUPIED_UNITS_C2 THERMS_TOTAL_SQFT_C2;
datalines;
. . . . . 65 3.5 . . . . . . . . . . . . . . . . .
;
run;
proc print;
run;
*join the dataset;
data prediction;
set pred afterDrop2;
proc reg data = prediction noprint;
model lnTOTAL_ENERGY_USAGE = AVERAGE_BUILDING_AGE AVERAGE_HOUSESIZE/ p clm cli alpha=0.05;
run;
