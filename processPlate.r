#Read raw data file (csvFileName) and return a matrix with 96 row (96 well measurement) X number of time point
processPlateData = function(csvFileName){
   numDataRow = 8;
   numDataCol = 12;  
   leftMostDataCol = 3; # nothing special.. just how csv file organized
   rightMostDataCol = leftMostDataCol + numDataCol - 1;
   storeIndex = 0;

   #read data from csv file the output is (somehow) a list of integer
   rawPlateData = read.csv(csvFileName)
   
   #we will look at the first column which is where the csv cells  labelled "Plate:" are. The location of these cells 
   #will point us to where plate measurement from individual data point is stored on the csv file. 
   col1 = as.character(rawPlateData[[1]]); #get the first column, [[]] allows use to go from list to array
   indexArray = 1:length(col1);
   readIndex = indexArray[col1 == "Plate:"]; #find the row number of cell on the first column with a word "Plate:"
   numTimePoint = length(readIndex); # the number of cell with word "Plate:" is basically the number of measurement data point
   
   #Here we create an output matrix, allPlateData, to store the plate reader results we pulled out from rawPlateData
   numAllDataPoint = numDataRow*numDataCol*numTimePoint;
   allPlateData = array(rep(-10, numAllDataPoint), c(numDataRow*numDataCol, numTimePoint));

   #Looping through each element in readIndex to get to where measurement from each time point is and get that data out to output matrix.
   for (n in readIndex){
        topDataRow = n + 2;
		bottomDataRow = topDataRow + numDataRow - 1;
		dataLocations = c(leftMostDataCol, rightMostDataCol, topDataRow, bottomDataRow); #location where the plate data is stored in 'rawPlateData'
		storeIndex = storeIndex + 1;
		#this function collectdata read the 'rawPlateData' at location specified by 'dataLocations', store that in 'allPlateData' at column 'storeIndex'
		allPlateData = collectData(rawPlateData, dataLocations, allPlateData, storeIndex);		
   }  
   return(allPlateData)
}


# collect data from a raw data list (dataVa) at location (dataLocation) and store in a column of a storage matrix (storeVar) indicated by storeInd
collectData = function(dataVar, dataLocation, storeVar, storeInd){
    n = 0; # data from each different well of dataVar goes to different rows of storeVar
    for (i in (dataLocation[1]:dataLocation[2])){
	   for (j in (dataLocation[3]:dataLocation[4])){
	      n = n+1;
          col_i = dataVar[[i]];	
          storeVar[n,storeInd] = as.numeric(as.character(col_i[j]))
	   }
	}
	return(storeVar)	
}

#Read raw data file (csvFileName) and return a matrix with 96 row (96 well experiment) X number of related experimental parameter
loadPlateSetup = function(setupName){
   numDataRow = 8;
   numDataCol = 12;
   leftMostDataCol = 2; # nothing special.. just how csv file organized
   rightMostDataCol = leftMostDataCol + numDataCol - 1;
   storeIndex = 0;

   #read setup from csv file the output is (somehow) a list of integer
   plateSetupData = read.csv(setupName) 
   #we will look at the first column which is where the csv cells  labelled each parameters (like "Strain:, Arabinose:, Atc:, and Device: are.
   #The location of these cells will point us to where parameters of each well is labelled. 
   #plate measurement from individual data point is stored on the csv file. 
   col1 = as.character(plateSetupData[[1]]); #get the first column, [[]] allows use to go from list to array
   indexArray = 1:length(col1);
   readIndex = indexArray[(as.character(col1) != "")]; #find the row number of cell on the first column with a word "Plate:"
   numParam = length(readIndex); # the number of cell with a word is basically the number of parameters in the set up
   paramList = as.character(col1[readIndex]); #list of parameters of our interest
	  
   # create output matrix of size 96 x numParam to store setup information
   numAllDataPoint = numDataRow*numDataCol*numParam;
   allSetupData = array(rep(-10, numAllDataPoint), c(numDataRow*numDataCol, numParam));
   
   #Looping through each element in readIndex to get to where the setup of each parameter are.
   for (n in readIndex)
   {
        topDataRow = n + 1;
		bottomDataRow = topDataRow + numDataRow - 1;
		dataLocations = c(leftMostDataCol, rightMostDataCol, topDataRow, bottomDataRow);
		storeIndex = storeIndex + 1;
		allSetupData = collectSetupData(plateSetupData, dataLocations, allSetupData, storeIndex);
   }
   
   colnames(allSetupData) <- paramList # label column of output matrix
   return(allSetupData)
}

# similar to collectData but for setup information
collectSetupData = function(dataVar, dataLocation, storeVar, storeInd){
    n = 0; # data from each different well of dataVar goes to different rows of storeVar
    for (i in (dataLocation[1]:dataLocation[2])){
	   for (j in (dataLocation[3]:dataLocation[4])){
	      n = n+1;
          col_i = dataVar[[i]];	
          storeVar[n,storeInd] = as.character(col_i[j])
	   }
	}
	return(storeVar)	
}

#select the data from the whole matrix of data (allData) that matches with given specification (selectParam)
#selectParam look like: c("DHaZ1", "-", "200", "Toxin A")
selectData = function(allData, allSetup, selectParam){
    selectionMark = rep(TRUE, length(allSetup[,1]))
	for (i in 1:length(selectParam)){ # looping through each entry in selectParam
		if (selectParam[i] == '-'){
		   next # if specified as '-', skip this parameter, i.e., getting all rows
		}
		selectionMark = selectionMark & (allSetup[,i] == selectParam[i]) # mark row that does not match with given param value as false. 
	}
	selectedData = allData[selectionMark,] #select the row of selectionMark that match selectionMark
	return(selectedData)
}

#similar to selectData but apply to setup matrix instead
selectSetup = function(allSetup, selectParam){
    selectionMark = rep(TRUE, length(allSetup[,1]))
	for (i in 1:length(selectParam)){
		if (selectParam[i] == '-'){
		   next 
		}
		selectionMark = selectionMark & (allSetup[,i] == selectParam[i])
	}
	selectedSetup = allSetup[selectionMark,]
	return(selectedSetup)
}

#extract time data from the csv data file and output as an array in minute of measurement time 
timeData = function(csvFileName){
   rawPlateData = read.csv(csvFileName)
   print(length(rawPlateData))
   col1 = as.character(rawPlateData[[1]]);
   indexArray = 1:length(col1);
   timeRow = indexArray[col1 == "Time:"]+1
   timeCol = 1
   timeArray = c()
   while(TRUE) {
       timeCol = timeCol + 1
	   print(timeCol)
	   print(length(rawPlateData))
	   print("...")
	   
	   #this if is to prevent out of bound error in case that time data row length exceed other row length on rawPlateData
	   if (timeCol > length(rawPlateData)){
	       break;
	   }
	   #this if is to stop reading once we reach the end of time data row. 
	   col_timeCol = as.character(rawPlateData[[timeCol]])
	   col_timeCol_row = col_timeCol[timeRow]
	   if (is.na(col_timeCol_row)){
	       break;
	   }
	   
	   timeArray = c(timeArray, as.numeric(col_timeCol_row ))
   }
   timeArray = 60*(timeArray - min(timeArray))# convert time unit to minutes
   return(timeArray)
}


#This function add error bar to a plot with x as free variable and y as output variable , 'sdxy' is standard deviation.
addErrorbar = function(x, y, sdxy){
	epsilon = 5 #size of horizontal bar on error bar
	for(i in 1:length(x)) {
		up = y[i] + sdxy[i]
		low = y[i] - sdxy[i]
		segments(x[i],low , x[i], up)
		segments(x[i]-epsilon, up , x[i]+epsilon, up)
		segments(x[i]-epsilon, low , x[i]+epsilon, low)
	}
}

#For a given matrix return a 2 x n matrix when n is the number of input matrix column. 
#Top row is column mean. Bottom row is column's SD
colMeanSD = function(data2D){
     numCol = length(data2D[1, ])
	 numRow = length(data2D[,1])
     colMeanSDOut = array(rep(-1, numCol*numRow), c(2, numCol)) 
	 for (i in 1:numCol){
	    colMeanSDOut[1, i] = mean(data2D[,i])
		colMeanSDOut[2, i] = sd(data2D[,i])
	 }
	 rownames(colMeanSDOut) <- c('mean', 'SD')
	 return(colMeanSDOut)
	 
}

