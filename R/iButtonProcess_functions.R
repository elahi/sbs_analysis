#################################################
# Author: Robin Elahi
# Date: 151005

# Script to compile iButton temperature files
# into one csv file
#################################################

# Raw iButton files are placed in a directory called "iButtonFiles", 
# within 'data' directory

# This processing file is in the "R" directory: where is this?
getwd()

# This call gets you the files within the present working directory
dir() # OR
dir(path = ".")

# What if I want the directory above?
dir(path = "..")

# What if I want a different directory one level above?
dir(path = "../data")

# Now choose the appropriate folder, in this case "iButtonFiles"
dir(path = "../data/iButtonFiles")

######################################################
# Need a list of file names from the iButtonFiles
fileNames <- dir(path = "../data/iButtonFiles", recursive = TRUE, 
               pattern = ".csv") 

######################################################
# Goal is to create a single data file with 4 columns:
# date/time
# temperature (Value)
# csv file name
# iButton registration number 
# (in this case, the latter two should be the same)

iButtonTempF <- function(fls) {
	# get names of all files
	fls <- fileNames
	emptyMat <- as.data.frame(matrix(nrow = 0, ncol = 4))
	names(emptyMat) <- c("iButtonID", "spreadsheetID", "dateR", 
	                     "tempC")
	
	for (f in 1:length(fls)) {
		
		# extract iButtonID
	  fls[1]
	  paste("../data/iButtonFiles/", fls[1], sep = "")
	  
	  read.csv("../data/iButtonFiles/030000002E2C9421.csv")
	  
	  
		ibutton <- read.csv(file = paste("../data/iButtonFiles/", fls[3], sep = ""), 
		                    skip = 1, nrow = 1, header = TRUE, stringsAsFactors = FALSE)
		ibutton
		
		# at which value do the 6 0s start?
		startZeros <- regexpr("000000", ibutton)[1] # position 39
		iButtonIDstart <- startZeros + 6 # position where iButtonID starts 
		iButtonID <- substr(ibutton, start = iButtonIDstart, stop = iButtonIDstart + 5) 
		# iButton ID is 6 digits long, so end 5 digits after start
		
		# assign treatmentID
		treatmentIDstart <- regexpr("/", fls[f]) + 1
		treatmentIDend <- regexpr(".csv", fls[f]) - 1
		treatmentID <- substr(fls[f], treatmentIDstart, treatmentIDend)

		# extract temperature Data
		tempDat <- read.csv(fls[f], skip = 14)
		# convert time stamps to POSIX object
		dateTimeR <- as.POSIXct(strptime(substr(tempDat$Date.Time, 1, 20), 
			format = '%d/%m/%y %I:%M:%S %p'))
		tempDat$dateTimeR <- dateTimeR

		# set minimum and maximum date.time for data inclusion
		startTime <- as.POSIXct(strptime("28/02/15 01:00:00 PM", 
			format = '%d/%m/%y %I:%M:%S %p'))
		endTime <- as.POSIXct(strptime("05/03/15 02:00:00 PM", 
			format = '%d/%m/%y %I:%M:%S %p'))
		# subset appropriate data
		tempDat2 <- tempDat[tempDat$dateTimeR > startTime & 
			tempDat$dateTimeR < endTime, ]

		# output data
		tempDat3 <- data.frame(iButtonID, treatmentID, 
			tempDat2$dateTimeR, tempDat2$Value)
		names(tempDat3)[3:4] <- c("dateR", "tempC")

		# return data
		emptyMat <- rbind(emptyMat, tempDat3)
	}
	return(emptyMat)
}