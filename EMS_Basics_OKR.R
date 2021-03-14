#######################################################
#### Basic steps for EMS, by Omar KR ##### 02/06/2020 #
#######################################################
# uploaded to https://github.com/omarkr8/EMS_tutorial #
#######################################################

##\0\ Setting up your work space
# It is good practice to start a project with a clean environment
rm(list=ls())

# If using RStudio, this line sets the working directory to the location of the script being worked on
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# If not using Rstudio, remove the `#` and use the following instead:
#setwd(getSrcDirectory()[1])

##\1\ loading and checking the PA matrix
# the arguments: `header` makes the first row as column names; `check.names` imports those headers "as is";
# and `row.names` specifies that the column '1' should be row names and not table entries

datamat<- read.csv("Bats_and_Sites.csv", header=TRUE, check.names = FALSE, row.names = 1)

# for EMS, we want our matrix to be a rows = sites(sometimes called species ranges), columns = species.
# the matrix cells have species abundances, as in how many species are found in that range
# if your matrix is not in the right format, try the transpose function " t() "

head(datamat)

##\2\ obtaining EMS values
# The package `metacom` has the tools to calculate the Elements of Metacommunity Structure

library(metacom)

# the 'Metacommunity' function will string together the relevant functions from the package, 
# and output values in tables
ems1<-Metacommunity(datamat,scores = 1, method = "r1", sims = 1000, order = TRUE, binary = TRUE)

# I suggest reading the help files on this function to understand what these arguments mean;
# other than score, the other arguments affect the simulated null matrices used for generating p-values

# calling the object made prints 4 `tables`. 
ems1
# we will be exploring each table below. in particular we are interested in `coherence`, `turnover`, and `boundary clumping` of our dataset in order to determine metacommunity structure

##\3\ interpreting EMS
# there are a few things to unpack here, but essentially null matrices (simulated 1000 times) were created by the function,
# and our matrix was rearranged to maximise 'coherence' so lets look at that first. 
# we will be visualising this `most coherent` matrix later in a nicer way than what is shown in `ems1`

ems1$Coherence

# embAbs is the number of embedded absences in our data, we compare this number to the simulated mean (simMean)
# we have fewer absences, and this is also reflected in the negative z value
# and the p-value shows this is significant
# a matrix having fewer absences by chance is considered 'positively coherent'

ems1$Turnover

# the same concept as above, we compare the numbers of our data to the simulated mean
# there is a larger turnover than expected by chance
# this data had positive turnover

ems1$Boundary

# for boundary clumping, we just need to look at the index and p-value
# if index >1 , there is boundary clumping, index <1 boundaries are hyperdispersed

# we can conclude that this matrix is coherent, has positive turnover, and has clumped boundaries;
# which is characteristic of a Clementsian structure.

# if you havent come across a figure to interpret structures from these elements,
# have a look at `Elements_visual_guide.pdf` in the GitHub repository
# or for a published version, try  https://doi.org/10.1002/ece3.1767

##\4\ generating figures for the ordinated matrix
# metacom has a function to visualise our matrix
# but its not very optimised, and does not look great on this dataset

Imagine(datamat,order = TRUE, scores = 1, fill = TRUE,xlab = "Species", ylab = "Site", yline = 2, xline = 2,sitenames = rownames(datamat), speciesnames = colnames(datamat),binary = TRUE)

# There is probably an easier way to make this figure work, 
# but I just tweaked the code to move the margin limits and limited some arguments, 
# see the very end of this script for my edited functions
# ImagineOKR (margins to fit long species names) and ImagineOKR2 (for a slimmer version)

ImagineOKR(datamat,order = TRUE, scores = 1, fill = TRUE,xlab = NULL, ylab = NULL, yline = 2, xline = 2,binary = TRUE)


##### end of script ####

#this is an edit of the Imagine function from metacom, i just adjusted the figure margins in the par(mar) argument, here at line 91.
ImagineOKR<-function (comm, col = c(0, 1), order = TRUE, scores = 1, fill = TRUE, 
                      xlab = "Species", ylab = "Site", yline = 2, xline = 2, 
                      sitenames = rownames(comm), speciesnames = colnames(comm), 
                      binary = TRUE) 
{
  require(metacom)
  if (order == TRUE) {
    comm <- OrderMatrix(comm, binary = binary, scores = scores)
  }
  if (fill == TRUE) {
    for (i in 1:dim(comm)[2]) {
      temp = comm[, i]
      if (sum(temp) < 2) {
        comm[, i] = temp
      }
      else {
        first <- min(which(temp > 0))
        last <- max(which(temp > 0))
        comm[first:last, i] <- max(temp)
      }
    }
  }
  reverse <- nrow(comm):1
  comm <- comm[reverse, ]
  par(mar = c(2, 8, 8, 1))
  image(1:dim(comm)[2], 1:dim(comm)[1], t(comm), col = col, 
        xlab = "", ylab = "", axes = FALSE)
  box()
  if (length(sitenames) > 1) {
    axis(2, at = 1:dim(comm)[1], labels = sitenames, las = 1, 
         cex.axis = 1, lwd.ticks = 0)
  }
  if (length(speciesnames) > 1) {
    axis(3, at = 1:dim(comm)[2], labels = speciesnames, las = 2, 
         cex.axis = 1, lwd.ticks = 0)
  }
  mtext(xlab, 3, cex = 1.5, line = xline)
  mtext(ylab, 2, cex = 1.5, line = yline)
}


###this is for smaller Imagine
ImagineOKR2<-function (comm, col = c(0, 1), order = TRUE, scores = 1, fill = TRUE, 
                       xlab = "Species", ylab = "Site", yline = 2, xline = 2, 
                       sitenames = rownames(comm), speciesnames = colnames(comm), 
                       binary = TRUE) 
{
  require(metacom)
  if (order == TRUE) {
    comm <- OrderMatrix(comm, binary = binary, scores = scores)
  }
  if (fill == TRUE) {
    for (i in 1:dim(comm)[2]) {
      temp = comm[, i]
      if (sum(temp) < 2) {
        comm[, i] = temp
      }
      else {
        first <- min(which(temp > 0))
        last <- max(which(temp > 0))
        comm[first:last, i] <- max(temp)
      }
    }
  }
  reverse <- nrow(comm):1
  comm <- comm[reverse, ]
  par(mar = c(2, 30, 5, 1))
  image(1:dim(comm)[2], 1:dim(comm)[1], t(comm), col = col, 
        xlab = "", ylab = "", axes = FALSE)
  box()
  if (length(sitenames) > 1) {
    axis(2, at = 1:dim(comm)[1], labels = sitenames, las = 1, 
         cex.axis = 1, lwd.ticks = 0)
  }
  if (length(speciesnames) > 1) {
    axis(3, at = 1:dim(comm)[2], labels = speciesnames, las = 2, 
         cex.axis = 1, lwd.ticks = 0)
  }
  mtext(xlab, 3, cex = 1.5, line = xline)
  mtext(ylab, 2, cex = 1.5, line = yline)
}


#### end of script ####