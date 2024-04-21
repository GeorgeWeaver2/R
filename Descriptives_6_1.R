# DESCRIPTIVES EXAMPLE
# Adapted from David Schuff, MIS Department, Fox School of Business, Temple University
# Really, the stats is all stuff you know already. 
# This is just a demonstration of what you can do using R, and to get you used to the wacky syntax.
# In most cases, you'll just modify parameters in these scripts, but sometimes you'll need to change 
#   the statements themselves to get it to work with your own datasets.

# First, clear all previous stuff out of the workspace...
rm(list = ls())

# VARIABLES - THIS SECTION HAS THE STUFF YOU SHOULD CHANGE.
# ** With a few exceptions, everything you'd modify is in this section. **
# ** Everything else (unless specified), LEAVE ALONE!!  				        **
# INPUT_FILENAME    The name of the file that contains the data (CSV format)
# OUTPUT_FILENAME   The name of the file that contains the text output (txt format)
# OUTPUT_HISTNAME   The name of the file that contains the histogram graphic (PDF format)
# NUM_BREAKS        The number of "buckets" of data in the histogram
# HISTLABEL         The label of the x-axis in the histogram
# HIST_BARCOLOR     The color of the shading for the bars in the histogram
# HIST_TITLE        The title of the histogram
INPUT_FILENAME    <- "OnTimeAirport2017Dec.csv"
OUTPUT_FILENAME   <- "descriptivesOutput.txt"
OUTPUT_HISTNAME   <- "histogram.pdf"
NUM_BREAKS        <- 50
HISTLABEL         <- "ArrivalDelays"
HIST_BARCOLOR     <- "lightgrey"
HIST_TITLE        <- "Histogram of Arrival delays per Airport"

# Install required packages as needed
if (!require("psych")) { install.packages("psych") #if you cant run psych, install it
  require("psych") }

# turn off scientific notation
options(scipen = 999)

# Read the comma-delimited data file. Make sure:
#     1) The first row contain the data labels
#     2) Numeric data fields only contain numbers
#     3) The order of the data match the order of the labels
dataSet <- read.csv(INPUT_FILENAME)

# Turn on output to a file (in addition to the screen). This way we've got a record of
# what we did.
#   append=FALSE means overwrite the file if it already exists
#   split=TRUE   means send the output to the console too!
sink(OUTPUT_FILENAME, append=FALSE, split=TRUE)

# Write out some text so that the output file makes sense...
#   cat() sends text to the output stream (console and the file)
#   \n means go to the next line. So \n\n means skip a line before whatever comes next.
cat("\n###### Let's get started with some fun statistics in RStudio! ######\n")

# Provide summary statisics of the Salary variable in dataSet (mean, median, etc.).
#    dataSet is my clever name for the dataset!
#    So, the thing before the $ is the name of your dataset.
#    The thing after the $ is the name of the variable you want to examine (watch CAPS!).
#
# IF YOU WERE USING YOUR OWN DATASET HERE (OR WANTED TO ANALYZE A DIFFERENT VARIABLE)
# YOU'D NEED TO CHANGE THE VARIABLE NAME. SAME GOES FOR THE REST OF THIS SCRIPT!
cat("\n###### Summary statistics for dataset: ", INPUT_FILENAME, "using the summary() function: ######\n")
summary(dataSet$Dest)

# You may want to find out more
# describe() is another useful function provided by the psych
cat("\n###### Summary statistics using the describe() function: ######\n")
describe(dataSet$ArrDelay)

# Provide summary statistics, this time grouped by player position.
#    So the first parameter is the variable your summarizing, 
#    and the second parameter is the grouping variable. The grouping variable should be categorical
#    or you may get a crazy mess!
cat("\n###### Summary statistics split by variable 'position' using the describeBy() function: ######\n")
describeBy(dataSet$Distance,dataSet$UniqueCarrier)
describeBy(dataSet$ArrDelay,dataSet$UniqueCarrier)
describeBy(dataSet$TaxiOut,dataSet$Origin)

cat("\n###### Do point guards make more than small forwards? ######\n")

# To compare the salaries of point guards to small forwards, we'll do a t-test.
#    So we need to first create a new dataset with only those two groups.
#    The line below does that - give me the rows where Position is PG **OR** SF.
#       Use | for OR, and use & for AND.
# 
# AGAIN, BE CAREFUL HERE. FOR YOUR OWN DATA, YOU'D CHANGE THE VARIABLE NAME AND THE
#    CATEGORIES TO REFLECT WHAT'S IN YOUR OWN DATA SET. IF THERE WERE ONLY TWO LEVELS 
#    IN YOUR DATA TO START WITH, YOU COULD JUST DO THIS:
#    subset <- dataSet;


cat("\n###### Create a subset with only two positions: PG and SF... ######\n")

sub1 <- dataSet[ which(dataSet$UniqueCarrier=='UA' |  dataSet$UniqueCarrier=='AA'), ] ;
t.test(sub1$ArrDelay~sub1$UniqueCarrier)

sub2 <- dataSet[which(dataSet$Origin=='EWR' | dataSet$Origin=='PHL'),];
t.test(sub2$TaxiOut~sub2$Origin)

describeBy(sub1$ArrDelay,sub1$CarrierName);
# Now run a standard t-test. Use Salary as your dependent variable
#     and Position as your independent (grouping) variable.
# FOR YOUR OWN DATASET, ADJUST YOUR VARIABLES ACCORDINGLY!
cat("\n###### Performing a t-test: ######\n")




describeBy(dataSet$Distance,dataSet$UniqueCarrier);

describeBy(dataSet$TaxiOut,dataSet$Origin);

# This stops R from writing any more to the text output file.
sink();

# We can verify the data is (or is not) normally distributed through a histogram.
#    use hist() to do this.
#    the first parameter  is the variable of interest
#    the second parameter is the number of bars to generate (more bars = more detail)
#                         (R takes this parameter as a suggestion but ultimately does what it wants!!)
#    the third parameter  is the color of the shading of the bars
#    the fourth parameter is the label for the x-axis of the histogram
# FOR YOUR OWN DATASET, ADJUST YOUR VARIABLES ACCORDINGLY!!
hist(dataSet$ArrDelay, breaks=NUM_BREAKS, col=HIST_BARCOLOR, xlab=HISTLABEL, main=HIST_TITLE)

# And now we run it again, but this time we send the output to a nice PDF file so you can look at it
# later, stick it up on your refrigerator, etc.
# FOR YOUR OWN DATASET, ADJUST YOUR VARIABLES ACCORDINGLY!!
pdf(OUTPUT_HISTNAME)
hist(dataSet$ArrDelay, breaks=NUM_BREAKS, col=HIST_BARCOLOR, xlab=HISTLABEL, main=HIST_TITLE)
dev.off()