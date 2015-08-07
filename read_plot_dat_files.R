####
#This script was written to read in data that was originally downloaded using a telnet protocol and saved as .dat files.
#Data files are accumulated in two folders, one for each circuit board.
#The data originate from an experimental soil warming experiment in Barrow, Alaska. There are two plots: one heated and one control.
#Three temperature probes per plot, each probe samples 5 different depths.
#Two circuit boards control these probes.
####

barrowData <- function(path, path2) {
files <- list.files(path, full.names=TRUE) #list files in folder
files2 <- list.files(path2, full.names=TRUE)

##read in files, cut non-data rows, then concatenate
readIn <- function(file){ 
  for(i in 1: length(file)) {
  data <- read.table(file[i], header = FALSE, sep="\t", quote = "") #load files
  cols <- as.character(data[,1]) #default data type is factor, change to character (i.e., string) to manipulate in next line
  data <- as.data.frame(data[grep("^<\\|.*\\|>$", cols),]) #select only rows that satisfy the condition: "<|" at SOL & "|>" at EOL
      # if the merged dataset does exist, append to it
      if (exists("dataset")){
        temp_dataset <- data
        dataset<-rbind(dataset, temp_dataset)
        rm(temp_dataset)  
      }
      # if the merged dataset doesn't exist, create it
      if (!exists("dataset")){
        dataset <- data
      }
  }
 return(dataset)
}

firstFolder <- readIn(files) #read in first folder
secondFolder <- readIn(files2) #adds second folder to first
dataset <- rbind(firstFolder, secondFolder)

#remove whitespace from data strings
whitespace <- function(x) {
  ch <- gsub(" ", "", x)
}
ch <- lapply(dataset, whitespace)

##split strings at | to assign columns
newdata <- data.frame(matrix(0, nrow = nrow(dataset) , ncol = as.numeric(37))) #create empty matrix
parsefx <- function(ch) {
  x <- strsplit(ch, "\\|")
  newdata <- do.call(rbind, lapply(x, `[`, c(1L:37L)))
}
newdata <- lapply(ch, parsefx)

#enter column headers below
newdata <- as.data.frame(newdata)
colnames(newdata) <- c("start", "date", "time", "id", "plot", "H.1.1", "H.1.2", "H.1.3", "H.1.4", "H.1.5", "H.2.1", 
                       "H.2.2", "H.2.3", "H.2.4", "H.2.5", "H.3.1", "H.3.2", "H.3.3", "H.3.4", "H.3.5", "H.4.1", "H.4.2",
                       "H.4.3", "H.4.4", "H.4.5", "attribute1", "attribute2", "attribute3", "attribute4", "attribute5", "end")

#redefine column types
convert.magic <- function(obj,types){
  wide <- lapply(1:length(obj),FUN = function(i){FUN1 <- switch(types[i],character = as.character,numeric = as.numeric, factor = as.factor); FUN1(obj[,i])})
  names(wide) <- colnames(obj)
  as.data.frame(wide,stringsAsFactors = FALSE)
}
widech <- convert.magic(newdata, c('character', 'factor', 'character', 'factor', 'factor', rep('character', 32)))
wide <- convert.magic(widech, c('character', 'factor', 'character', 'factor', 'factor', rep('numeric', 32)))
wide$date <- as.Date(wide$date, "%d-%m-%Y")
wide$time <- with(widech, as.POSIXct(paste(date,time), format= "%d-%m-%Y %H:%M:%S")) #time zone is still PDT
wide <- subset(wide, select = -c(start, end, NA., NA..1, NA..2, NA..3, NA..4, NA..5))

#change from wide data format to long
#install.packages("reshape") #only need to do this once, then comment out
library(reshape)
long <- melt(wide, id = c("date", "time", "id", "plot", "attribute1", "attribute2", "attribute3", "attribute4", "attribute5"))

#split header into three columns
test <- strsplit(as.character(long$variable), "\\.")

long$circuit <- unlist(lapply(test, '[[', 1)) #set header attribute 1
long$loc <- unlist(lapply(test, '[[', 2)) #set header attribute 2
long$depth <- unlist(lapply(test, '[[', 3)) #set header attribute 3
long <- subset(long, select = -c(variable))

#uncomment to write data to a csv file, change path to desired destination
#write.csv(long, file = "/Users/rzabramoff/Desktop/myfile.csv")

#uncomment to sort data
#sorted1 <- long[order(long$value),] #sort by value
#sorted2 <- long[order(long$value, long$depth),] #sort by value and depth
#sorted3 <- long[order(long$value, rev(long$depth)),] #sort by value (ascending) and depth (descending)

par(mfrow = c(1,2))
#sample plot with wide data format
plot(wide$time, wide$H.1.1, cex = 0.5, ylab = expression("Temperature (" ~ degree*C ~ ")"), xlab = "Time")
points(wide$time, wide$H.1.2, cex = 0.5, col = 2)
points(wide$time, wide$H.1.3, cex = 0.5, col = 4)
legend("topright",c("H.1.1","H.1.2", "H.1.3"),
       pch = c(1,1,1),
       col=c(1,2,4),cex=1.3
)

#sample plot with long data format (only plotting subset for speed)
plot(long$time[1:1000], long$value[1:1000], ylab = expression("Temperature (" ~ degree*C ~ ")"), xlab = "Time")
}

##enter path to the folder that contains data from circuit board M
path = "/Users/rzabramoff/Desktop/mstdat" #read in folder
path2 = "/Users/rzabramoff/Desktop/otherCB"

ptm <- proc.time()
barrowData(path, path2)
proc.time() - ptm


