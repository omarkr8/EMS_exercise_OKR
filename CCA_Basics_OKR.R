#######################################################
#### Basic steps for CCA, by Omar KR ##### 02/06/2020 #
#######################################################
# uploaded to https://github.com/omarkr8/EMS_tutorial #
#######################################################
# This script was created to complement 'Basic Steps for EMS', but otherwise is an independent analysis
# Canonical Correspondence Analysis (CCA) will need a matrix, and a list of variables to 'correlate' with.
# for the matrix, we will use the ordinated metacommunity matrix from the EMS basics script.


##\1\ fetching an ordinated matrix from metacom::Metacommunity 
# i'll repeat the ordinated matrix generation here, but you can just use the `ems1` created from the Basic  # EMS script
datamat<- read.csv("Bats_and_Sites.csv", header=TRUE, check.names = FALSE, row.names = 1)

# We are looking for the object created by the Metacommunity function from the metacom package
library(metacom)
ems1<-Metacommunity(datamat,scores = 1, method = "r1", sims = 1000, order = TRUE,binary = TRUE)
ems1

# we specifically want the  ordinated community matrix from the object we created. We can use '$' to pick it out 
OComm<-ems1$Comm

##\2\ CCA models, general tips
# cca is used similar to linear modeling. To see which independent variables corresponds to the pattern in  # dependent variable; only our dependent variable is a matrix rather than a vector.
# import the environmental variable data.its in format rows (metric) and col (fragment), same as the
# metacom$comm
# in this example, we are using landscape variables that measure forest fragmentation
landmet<- (read.csv("Landscape_var.csv", row.names = 1))
landmet

# the cca function from vegan has many arguments, but for basics we just need a formula and a matrix with 
# the environmental variables
# for context, the variables here are logArea (area of habitat), isolation (shortest distance between 
# habitat), and PCA of vegetation configurations
S1<-cca(formula= OComm ~ logArea + Isolation + PCAhab1 + PCAland1 + PCAhab2 + PCAland2, data=landmet)

# similar to variables in linear modeling, we may want to check for and limit correlated variables.
library(car)
vif(S1)

# As a general rule, a Variance Inflation Factor of >5 indicates some problematic pairs of variables (the 
# importance of both variables, their coefficients, get inflated). Here we find logArea, PCAhab1, and 
# PCAland1 to have our attention. Some people would remove all three variables, but in our case we have an a # priori reason to retain 'logArea' (it is known to be an important feature in habitat studies)
# instead, i will remove the variable with the highest VIF, and repeat until i am satisfied with the values.
S2<-cca(formula= OComm ~ logArea + Isolation + PCAland1 + PCAhab2 + PCAland2, data=landmet)
vif(S2)
S3<-cca(formula= OComm ~ logArea + Isolation + PCAhab2 + PCAland2, data=landmet)
vif(S3)

# So now i have some confidence that these variables are independent enough from each other.
# let's see what this CCA object looks like
S3

##\3\ CCA interpretation: inertia, eigenvalues
# From here on there are a few terms we need to understand. Inertia and Eigenvalues. I will try to explain 
# these in casual terms.

# Inertia is the 'variation in our metacommunity data'.Important not to use the term 'variance' here, since 
# it is an already existing concept in statistics. 
# We have the total inertia 'available to be allocated' from our data, constrained inertia (variation 
# attributed to the variables we provided), and unconstrained inertia (variation yet to be attributed).

# Eigenvalues are 'contributions of explanatory power' by each Constrained Canonical Axis (CCA1, CCA2, etc).
# Yes I am aware that having Constrained Canonical Axes (CCAs) in Canonical Correspondence Analyses (CCAs) 
# is unhelpful for anagram purposes, but we can try being unambiguous when assigning them.
# The number of constrained axes should equal to the number of variables we provided, in this case 4.
# if you added all the constrained eigenvalues, it should be the same as value of constrained inertia, and 
# same for unconstrained eigenvalues.

# Like EMS, CCA also uses reciprocal averaging. In both EMS and CCA, we can look at multiple axes of 
# ordination to gain insight in our data. The typical practice is to look at the first two (primary and 
# secondary) axes. This is because the axes are ranked, primary axis being the strongest (this is true for 
# both EMS and CCA).

# You should be able to already make some inference from this output alone as to how much variation is 
# covered by the variables provided. But we should also look at the summary.cca()
Sum3<-summary(S3)
Sum3

# it's a big table, but again we can dissect it with $, i'll highlight some sections soon.

##\4\ Biplot interpretation
# we can create a biplot, which is a kind of scatterplot with added variables as vectors
plot(S3)

# Sites are in black, species in red, and variables are blue vectors.
# the species and sites section are coordinates used in a biplot. 
Sum3$species
Sum3$sites

# At the default cca scale setting, the further something is from the origin, the more 'discerning' it is, 
# as in, points close to origin tend to be 'inert' and not easily influenced by anything in our data. 

# You can further discuss relationships between sites and species based on their within-group distance from 
# one another. so close species are found in similar sites, close sites have similar species. but remember, 
# if this clump is close to the origin, this no longer applies, as it becomes an 'uninformative/inert' clump

# this distance inference cannot be used cross group. so for example, site REGUA2 and V. pussila being close # should not be used to say that the two are related (although in this case they are, V. pusilla is in fact # only found in REGUA2)...
# A better example to state my point would be site F4 and H.velatus. This bat is only found in F4, but based # on the sites around it, you'd think it was related to F6 and maybe F5 too, this is not the case. So dont 
# use cross-group distances to make conclusions.

# biplot scores are coordinates for your provided variables
Sum3$biplot

# (Orientation): variables in the similar direction indicate a positive correlation between those variables.
# (Length) : variables that follow a direction to an axis, is strongly related to that axis. The arbitrary 
# threshold used here is 0.5 and greater, for a variable to be considered significant to that axis.
# (Angles) : small angles indicate strong correlation. this is true for vector to vector, and vector to axis
# 90 degrees represent no correlation.
# for example, both logArea and Isolation have strong direction in terms of the first axis, but not the 
# second. They are also negatively correlated with one another
# also, the two PCA variables are positively correlated with one another, has a strong effect in the second 
# axis, but not the first.


# we can take a look at the eigenvalue proportions for the axes
Sum3$concont # for just the constrained axes
Sum3$cont # for both constrained and unconstrained axes

##\5\ reviewing what we learned
# Let's try to put the pieces together now.
# the four variables we included in the model accounts for less than 50% of inertia
S3

# the primary constrained axis (CCA1) only accounts for 30% of the constrained inertia, and 16% of overall inertia
Sum3$concont
Sum3$cont

# and we see that CCA1 is strongly related to habitat area and isolation
Sum3$biplot

# putting this together, we can say that both of these variables are the most important in our set of 
# variables, but there is alot of inertia still unexplained.
# We can compare CCA1 and CA1, and see that they are similar, so there are variables just as important as 
# area and isolation we had not included.
# important to remember that changing the list of variables will change a lot of these values, everything is
# relative to one another in this analysis.

#### end of script ####
