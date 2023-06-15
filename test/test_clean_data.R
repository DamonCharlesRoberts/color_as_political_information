# Title: test clean_data function

# Notes:
    #* Description
        #** testing of the clean_data funciton
    #* Updated
        #** 2023-06-12
        #** dcr

# Setup
    #* set working directory
#setwd("../src/")
setwd("./src/")
    #* load relevat packages
box::use(
    ./R/clean_data[...]
    , data.table[...]
)
    #* examples
        #** load single file
df_temp <- read.csv(
    file = "/Users/dcr/Dropbox/current_projects/dissertation/chapter_2/data/original/study_1/data/diss_ch_1_pre-reg_PARTICIPANT_SESSION_2023-06-12_12h07.31.739.csv"
)

df_test <- extract_file(df_temp)


df_test <- clean_data(
    folder = "../data/original/study_1/data"
)

# check to see if it is not null
list_test_name <- names(list_multiple_files)
