# Title: test relocate.R

# Notes:
    #* Description
        #** Unit testing to see if the functions in the relocate.R script work.
    #* Updated
        #** 2023-06-14
        #** dcr

# Setup
    #* Set working directory
# setwd("../src/")
setwd("./src/")
    #* Load relevant funcitons
box::use(
    testthat[...]
    , ./R/relocate[...]
)

    #* Example
pavlovia_folder = "../data/original/study_1/data/"
prolific_file = "../data/clean/prolific_demographic_data.csv"

