
# Elements of Metacommunity Structure tutorial
#### This repository is used to give an example of how to perform analysis of the elements of metacommunity structure, followed by constrained correspondence analysis. I used this method to infer how metacommunities form along environmental gradients. Main R packages showcased here include `metacom`, and various `vegan` tools.
#### The R scripts were created for tutoring colleagues. It is currently not reproducible as the datasets are not included in this repository. 

The 2 datasets used were provided by [Tiago Teixeira](https://scholar.google.com/citations?user=Ed7bSzQAAAAJ&hl=en), which consists of frequencies of bat species across 13 sites in the Atlantic Forest in the form of a presence/absence matrix, and data on landscape properties of the 13 sites for the subsequent CCA analysis.

This tutorial has 2 parts: *"EMS_Basics_OKR.R"*, and *"CCA_Basics_OKR.R"* intended to be viewed in that order. 

##### __The R script *"EMS_Basics_OKR.R"* includes a series of commands to:__

* Prepare the environment
* Load and prepare the data set *"Bats_and_Sites.csv"* (not included)
* Perform the EMS calculation with the R function `metacom::Metacommunity`
* Generate a visualisation of the ordinated matrix using `metacom::Imagine` (available in plot folder)

##### __You may then follow-up with R script *"CCA_Basics_OKR.R"* to:__

* Fetch the ordinated matrix
* Select environmental variables from *"Landscape_var.csv"* (not included), create CCA model 
* Interpret CCA: Inertia, eigenvalues
* Interpret CCA: biplot (available in plot folder)

###### Archiving this repo, OKR
