source("processPlate.r");


#import data and set up information 
allData = processPlateData("cellContactExperiment - 2014_10_28 data.csv"); 
timeArrayO = timeData("cellContactExperiment - 2014_10_28 data.csv") ;
setupData = loadPlateSetup("cellContactExperiment - 2014_10_28 setup.csv");

#set display range
selectTime = 3:7  #for this example, we have 7 time point but the first two is bad so we start from 3 and go up to 7

#specify color of each plot line
colList = c("blue", "red",  "black","green", "cyan4", "coral4")		

##plot 1:
titleName = "No induction" # plot title
legendTitle = "Constructs" # legend title
freeVarInd = 4;   #this number specify the index of free variable. For this example, 1-4 are Strain, Arabinose, Atc and Device.
         #we will later use 'freeVarInd' for labelling the legend, By picking freeVarInd = 4 here, our legend label is Negative control, ToxinA, ToxinB

#selectParams tell the script rows of 'allData are to be plotted. In this example, this list with three entries specifies that there will be three plots.
# (on the same axis). The first plot is specified by c('DH5aZ1', '0', '0', 'Negative control'), i.e., using all row with DH5aZ1 strain, Arabinose 0 %,
# ATc 0 ng/ml and Negative control device. 		 
selectParams = list(c('DH5aZ1', '0', '0', 'Negative control'), # these four entries are strain, Ara level, Atc level and construct 
                     c('DH5aZ1', '0', '0', 'Toxin A'),
					 c('DH5aZ1', '0', '0', 'Toxin B')
					 )
					
# # plot 2:					
# titleName = "Induced toxin (0.0001% ara)"
# legendTitle = "Constructs"
# freeVarInd = 4;
# selectParams = list(c('DH5aZ1', '0.0001', '0', 'Negative control'), # these for entries are strain, Ara level, Atc level and construct 
                    # c('DH5aZ1', '0.0001', '0', 'Toxin A'),
					# c('DH5aZ1', '0.0001', '0', 'Toxin B')
					# )			
					
# # plot 3:					
# titleName = "Induced toxin (0.1% ara)"
# legendTitle = "Constructs"
# freeVarInd = 4;
# selectParams = list(c('DH5aZ1', '0.1', '0', 'Negative control'), # these for entries are strain, Ara level, Atc level and construct 
                    # c('DH5aZ1', '0.1', '0', 'Toxin A'),
					# c('DH5aZ1', '0.1', '0', 'Toxin B')
					# )								

# ##plot 4:
# titleName = "Induced antitoxin (200 ng/ml ATc)"
# legendTitle = "Constructs"
# freeVarInd = 4;
# selectParams = list(c('DH5aZ1', '0', '200', 'Negative control'), # these for entries are strain, Ara level, Atc level and construct 
                     # c('DH5aZ1', '0', '200', 'Toxin A'),
					 # c('DH5aZ1', '0', '200', 'Toxin B')
					 # )

# ##plot 5:
# titleName = "Induced toxin/antitoxin (0.0001% ara; 200 ng/ml ATc)"
# legendTitle = "Constructs"
# freeVarInd = 4;
# selectParams = list(c('DH5aZ1', '0.0001', '200', 'Negative control'), # these for entries are strain, Ara level, Atc level and construct 
                     # c('DH5aZ1', '0.0001', '200', 'Toxin A'),
					 # c('DH5aZ1', '0.0001', '200', 'Toxin B')
					 # )
					 
# ##plot 6:
# titleName = "Induced toxin/antitoxin (0.1% ara; 200 ng/ml ATc)"
# legendTitle = "Constructs"
# freeVarInd = 4;
# selectParams = list(c('DH5aZ1', '0.1', '200', 'Negative control'), # these for entries are strain, Ara level, Atc level and construct 
                     # c('DH5aZ1', '0.1', '200', 'Toxin A'),
					 # c('DH5aZ1', '0.1', '200', 'Toxin B')
					 # )
					 

#..........................script below is for plotting..should not be changed...
freeVar = c();
#Here we setup the axis for the plot. Note that -1. -1 is just a dummy dot plot which won't appear on the final display. 
plot(-1,-1, xlab = 'time (min)', ylab = 'OD', pch = 21, bg = "red",
     xlim=c(0, max(timeArrayO)), ylim=c(0, 1) )
title(main = titleName)	 
# This loop generate plot as specified by each entry in selectParams on by one. 	 
for (i in 1: length(selectParams)){

	selectParam = selectParams[[i]]; 
    sel_setup = selectSetup(setupData, selectParam); #select set up
    sel_dat = selectData(allData, setupData, selectParam); #select data to be plotted
	timeArray = timeArrayO[selectTime] # select time range
	print(timeArray)
    sel_dat = sel_dat[, selectTime]
	sel_dat_meanSD = colMeanSD(sel_dat) #find mean and variance of selected set of data
	
	print(sel_setup)
	print(sel_dat_meanSD)
	
	sel_dat_mean = sel_dat_meanSD[1,];
	sel_dat_SD = sel_dat_meanSD[2,];
	
	#generate the plot
	lines(timeArray,  sel_dat_mean, type = "o", pch = 16, col = colList[i], bg = colList[i], lwd = 2)
	freeVar = c(freeVar, selectParam[freeVarInd]) 
	addErrorbar(timeArray, sel_dat_mean, sel_dat_SD) #add error bar
}
	 
   legend("topleft", inset=.05, title=legendTitle,
  	freeVar, fill = colList[1:length(selectParams)], horiz=FALSE)
	

