# Clustering with the cluster package
# Adapted from Matt Peeples: http://www.mattpeeples.net/

# First, clear all previous stuff out of the workspace...
rm(list = ls())

# VARIABLES - THIS SECTION HAS THE STUFF YOU SHOULD CHANGE.
# ** With a few exceptions, everything you'd modify is in this section. **
# ** Everything else (unless specified), LEAVE ALONE!!					**
# INPUT_FILENAME     The name of the file that contains the data (CSV format)
# PLOT_FILENAME      The name of the pdf file, generated by R, that contains the clustering plots
# OUTPUT_FILENAME    The name of the txt file, generated by R, that contains the output from the clustering algorithm
# CLUSTERS_FILENAME  The name of the csv file, generated by R, that contains the cluster contents
# NORMALIZE          Whether to normalize the data (1=yes, 0=no)
# RM_OUTLIER         Whether to remove outliers (1=yes, 0=no)
# MAX_CLUSTER        The maximum number of clusters to generate in SSE plot
# NUM_CLUSTER        The number of clusters to generate for solution
# MAX_ITERATION      The maximum number of iterations the algorithm will attempt before stopping (more = better clusters)
# VAR_LIST           The names of variables to be used for clustering (must be spelled the same as in the input file)
INPUT_FILENAME    <- "Census2000.csv"
PLOT_FILENAME     <- "ClusteringPlots.pdf"
OUTPUT_FILENAME   <- "ClusteringOutput.txt"
CLUSTERS_FILENAME <- "ClusterContents.csv"
NORMALIZE         <- 1
RM_OUTLIER        <- 1 
MAX_CLUSTER       <- 25
NUM_CLUSTER       <- 7
MAX_ITERATION     <- 500
VAR_LIST          <- c("RegionDensityPercentile","MedianHouseholdIncome","AverageHouseholdSize")

# We need two packages, cluster and psych. 
# If the packages haven't been installed already, they will be installed first, and then loaded.
if (!require("cluster")) {        
  install.packages("cluster")
  require("cluster")
}

if (!require("psych")) {
  install.packages("psych")
  require("psych")
}

# Read the comma-delimited data file. Make sure:
#     1) The first row contain the data labels
#     2) Numeric data fields only contain numbers
inputFile     <- read.csv(INPUT_FILENAME)
inputMatrix   <- as.matrix(inputFile)

# Turn on output to a file (in addition to the screen). 
# This way we've got a record of what we did.
#   append   = FALSE means overwrite the file if it already exists
#   split    = TRUE  means send the output to the console too!
sink(OUTPUT_FILENAME, append=FALSE, split=TRUE)

# Summarize the data
cat("\n###### Summary Statistics for each variable (raw data): ######\n")
describe(inputFile[,VAR_LIST])

# And now turn on PDF output to send plots to a PDF file
# The pdf file will be saved to the working directory.
pdf(PLOT_FILENAME)

# Plot the histogram of each variable
par(mfrow=c(2,length(VAR_LIST))) #This line allows us to put multiple charts on the same page
for (i in 1:length(VAR_LIST)){
  hist(inputFile[,VAR_LIST[i]], xlab = VAR_LIST[i], main="") }


#######################################################################
######                 Preprocessing the data                    ######

# Create a new dataset without missing values, call it kData
kData <- na.omit(inputMatrix[,VAR_LIST])
numNoMissing <- nrow(kData)

#### Normalize the data so that it is all on the same scale.
# The scale() function will first "center" each column
# by subtracting the column average from the corresponding column, 
# And then "rescale" 
# by dividing each (centered) column by its standard deviation
if (NORMALIZE == 1) {
  kData <- scale(kData)}
  
# After normalizing the data,
# the mean of each variable become 0
cat("\n###### Summary Statistics after normalization: ######\n")
describe(kData)

####  Remove outliers
# We are defining outliers as values that are more than 
# three standard deviations away from the mean. Again, this keeps data with
# very large values from skewing the results.
if (RM_OUTLIER == 1) { outlierIndex <- which(apply(abs(kData)<=3,1,prod)==0)
  kData <- kData[-outlierIndex,] }

# Display some quick stats about the cleaning process
cat("\nTotal number of original cases:")
nrow(inputMatrix);

cat("\nTotal number of cases with no missing data:")
numNoMissing

cat("\nTotal number of cases without missing data and without outliers:")
nrow(kData)

#######################################################################
######                    Start Clustering!                      ######

# This just rigs the random number generator so it always comes out the same.
# It keeps out results consistent over multiple runs.
set.seed(1234) 

############################################################################
###### First, comparing different solutions with numbers of clusters  ######
# Calculate the within-cluster SSE (Cohesion) for different numbers of cluster solutions
wss <- rnorm(10)
while (prod(wss==sort(wss,decreasing=T))==0) {
  wss <- (nrow(kData)-1)*sum(apply(kData,2,var))
  for (i in 2:MAX_CLUSTER) wss[i] <- sum(kmeans(kData, algorithm="Lloyd", centers=i, iter.max = MAX_ITERATION)$withinss)}

# Plot the cluster solutions against within-cluster SSE
par(mfrow=c(1,1))
plot(1:MAX_CLUSTER, wss, type="b", xlab="Number of Clusters",
     ylab="Within-Cluster SSE", , main="Cluster Solutions against Within-Cluster SSE")

# There is no hard and fast rule for selecting the appropriate number of clusters
# but the plots above designed to help you evaluate your data set.

#############################################################################
######   Second, run cluster analysis with a given number of clusters  ######
# Create a kMeans clustering model called MyKMeans.
# We run the clustering algorithm using the kmeans() function. 
# The first term is the input dataset that has been cleaned up
# The second term algorithm
# The third term is the number of clusters we just chose
# The fourth term is the number of iterations we want to run in the algorithm 
#     -- you don't need to worry about it.
MyKMeans <- kmeans(kData, algorithm="Lloyd", NUM_CLUSTER, iter.max = MAX_ITERATION)

################################################
######   Third, Interpreting the Results  ######
# Display the cluster sizes
cat("\nCluster size:")
MyKMeans$size

# Plot the cluster sizes on a nice looking pie chart!
lbls <- paste("Cluster #",1:NUM_CLUSTER,":\n",MyKMeans$size)
pie(MyKMeans$size, main = "Pie Chart of Cluster Sizes", col = 1:NUM_CLUSTER, labels = lbls)

# Display the cluster means (means for each input variable)
cat("\nCluster Means (centroids):")
MyKMeans$centers

# Display the summary statistics for each cluster 
cat("\nSummary Statistics by Cluster (normalized data):")
describeBy(kData, MyKMeans$cluster)

# Plot the each variable by cluster
# Each plot will be about one variable, e.g, RegionDensityPercentile
# The x-axis represents clusters
# The red line represents the cluster average (centroid)
# The dash line represents population average (which is always 0)
for (i in 1:length(VAR_LIST)){
  meansbycluster = tapply(kData[,VAR_LIST[i]],MyKMeans$cluster,mean)
  plot(kData[,VAR_LIST[i]]~MyKMeans$cluster,
          xlab = "Cluster",
          ylab = VAR_LIST[i],
          ylim = c(min(kData[,VAR_LIST[i]])-1.5,max(kData[,VAR_LIST[i]])+0.5),
          main=paste(VAR_LIST[i],"by Cluster"))
  lines(1:NUM_CLUSTER,meansbycluster, col="red") #add a line to represent centroid
  abline(h=0,lty=2,xlim=c(1,NUM_CLUSTER)) #add a horizontal line at zero
  legend("bottom",legend=c("Population Average", "Cluster Average"),
          col=c("black","red"), lty=c(2,1),horiz=TRUE)
}

# Display withinss (i.e. the within-cluster SSE for each cluster)
cat("\nWithin cluster SSE for each cluster (Cohesion):")
MyKMeans$withinss

# Display betweenss (i.e. the SSE between clusters)
cat("\nTotal between-cluster SSE (Seperation):")
MyKMeans$betweenss

# Compute average separation: more clusters = less separation
cat("\nAverage between-cluster SSE:") 
MyKMeans$betweenss/NUM_CLUSTER

dev.off()

# Turn off output to the text file.
sink()

# output data
outFile <- data.frame(kData,cluster = MyKMeans$cluster)  #append clusters to csv file
write.csv(outFile,file=CLUSTERS_FILENAME)

