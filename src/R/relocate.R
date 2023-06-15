#' @title relocate non-completed
#'
#' @description
#' This is a function that identifies files from pavlovia that
#' reflect incomplete participation (left the study early)
#' and moves them to a separate folder.
#' 
#' @details
#' The files that this reflect do not account for people that didn't answer
#' particular questions. It reflects people who left the study early
#' or faced a technical issue and were not marked as "Complete"
#' on prolific.
#' 
#' @param pavlovia_folder A string of a folder path telling it where the pavlovia csv files are
#' @param prolific_file A string of a file containing a file with a list of participant_id's
#' for those who completed the study.
#' @param new_folder A string of a folder path to place the incomplete participant data into.
#' 
#' @returns
#' 
#' @examples
#' 
organize_pavlovia_files <- function(
    pavlovia_folder = NULL
    , prolific_file = NULL
    , new_folder = NULL
) {
    # Load in prolific file
    df_prolific <- read.csv(file = prolific_file)
    # Create a list of valid participant id's and submission ids
    list_unique_id <- paste(df_prolific$Participant.id, df_prolific$Submission.id, sep="")
    # Load in all of the csv's for the pavlovia data
    list_pavlovia_filenames <- list.files(
        path = pavlovia_folder
        , full.names = TRUE
    ) |>
    as.list()
    list_pavlovia_data <- base::lapply(
        list_pavlovia_filenames,
        , FUN = read.csv
    )
    # Make a list of participant participant and session ids
    list_pavlovia_names <- base::lapply(
        list_pavlovia_data
        , function(x) {
            name <- paste(x$subject_id[1], x$session_id[1], sep="")
        }
    )
    # Define the index names for the list of pavlovia data.frames by unique names
    names(list_pavlovia_data) <- list_pavlovia_names
    # Filter index names of pavlovia data.frames by list of unique prolific names
    list_filtered <- list_pavlovia_data[list_unique_id]
}

