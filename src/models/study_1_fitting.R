# Title: Preliminary analyses of pavlovia data

# Notes
    #* Description
        #** This is a R file that performs some preliminary analyses of
        #** the pavlovia data for study 1
    #* Updated
        #** 2023-06-16
        #** dcr

# Setup
    #* set working directory
setwd("./src")
    #* execute cleaning script
source("./eda/study_1/cleaning.R")
    #* load helpful functions
box::use(
    data.table[...]
    , brms[...]
)
    #* make an empty model list object
list_fitted <- list(
    h_1 = list()
    , h_2 = list()
    , h_3 = list()
    , h_4 = list()
    , h_5 = list()
)
list_ppc <- list(
    h_1 = list()
    , h_2 = list()
    , h_3 = list()
    , h_4 = list()
    , h_5 = list()
)

# Hypothesis 1

# Hypothesis 2
    #* Fit models
list_fitted[["h_2"]][["red"]] <- brm(
    formula = trial_1_party ~ trial_1_red_stimuli
    , data = list_df[["cleaned"]]
    , family = cumulative(link = "logit")
    , prior = set_prior("normal(0, 1)", class = "b")
    , backend = "cmdstanr"
)
list_fitted[["h_2"]][["blue"]] <- brm(
    formula = trial_1_party ~ trial_1_blue_stimuli
    , data = list_df[["cleaned"]]
    , family = cumulative(link = "logit")
    , prior = set_prior("normal(0, 1)", class = "b")
    , backend = "cmdstanr"
)
list_fitted[["h_2"]][["red_interact"]] <- brm(
    formula = trial_1_party ~ trial_1_red_stimuli + age + trial_1_red_stimuli * age
    , data = list_df[["cleaned"]]
    , family = cumulative(link = "logit")
    , prior = set_prior("normal(0, 1)", class = "b")
    , backend = "cmdstanr"
)
list_fitted[["h_2"]][["blue_interact"]] <- brm(
    formula = trial_1_party ~ trial_1_blue_stimuli + age + trial_1_blue_stimuli * age
    , data = list_df[["cleaned"]]
    , family = cumulative(link = "logit")
    , prior = set_prior("normal(0 , 1)", class = "b")
    , backend = "cmdstanr"
)
    #* Posterior predictive checks
list_ppc[["h_2"]][["red"]] <- pp_check(
    list_fitted[["h_2"]][["red"]]
)
list_ppc[["h_2"]][["blue"]] <- pp_check(
    list_fitted[["h_2"]][["blue"]]
)
list_ppc[["h_2"]][["red_interact"]] <- pp_check(
    list_fitted[["h_2"]][["red_interact"]]
)
list_ppc[["h_2"]][["blue_interact"]] <- pp_check(
    list_fitted[["h_2"]][["blue_interact"]]
)
# Hypothesis 3

# Hypothesis 4

# Hypothesis 5

# Store model results
save(
    list_fitted
    , list_ppc
    , file = "../data/temp/models/study_1_fitted.RData"
)