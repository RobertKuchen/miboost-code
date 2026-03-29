
# Create Functions -------------------------------------------------------------

source("functions/replicate_datasets.R")

source("functions/cv_boost_imputed_wrap.R")

source("functions/create_lambda_sequence.R")

source("functions/SaLASSO_functions.R")

source("functions/SaENET_functions.R")


# Run R-Scripts ----------------------------------------------------------------

## Simulate and Impute Data ----------------------------------------------------

source("scripts/MI_Simulation.R")

source("scripts/EA_Boosting_Wrapper.R")

source("scripts/MIBoost_Wrapper.R")

source("scripts/SaLASSO_wrapper.R")

source("scripts/SaENET_wrapper.R")

source("scripts/Summarize_Results.R")







