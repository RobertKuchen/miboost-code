library(miselect)
library(tidyverse)

load(file.path("output", "FULL_SIM_LIST_50.RData"))

n_sim <- length(FULL_SIM_LIST_50$Y_LIST_TEST)

from_to <- seq_len(n_sim)

n_lambda <- 100

# p = 50 -----------------------------------------------------------------------

# First determine the lambda sequence 

lasso_sim_results <- lasso_sim(X_LIST_TRAIN = FULL_SIM_LIST_50$X_LIST_TRAIN[from_to],
                               Y_LIST_TRAIN = FULL_SIM_LIST_50$Y_LIST_TRAIN[from_to],
                               X_LIST_TEST = FULL_SIM_LIST_50$X_LIST_TEST[from_to],
                               Y_LIST_TEST = FULL_SIM_LIST_50$Y_LIST_TEST[from_to],
                               X_LIST_CV = FULL_SIM_LIST_50$X_LIST_CV[from_to],
                               Y_LIST_CV = FULL_SIM_LIST_50$Y_LIST_CV[from_to],
                               X_LIST_VAL_CV = FULL_SIM_LIST_50$X_LIST_VAL_CV[from_to],
                               Y_LIST_VAL_CV = FULL_SIM_LIST_50$Y_LIST_VAL_CV[from_to],
                               n_lambda = n_lambda,
                               use_adweights = TRUE,
                               use_adweights_lm = TRUE,
                               use_cv_saenet = FALSE)

salasso_seq_50 <-

create_lambda_sequence(selected_lambdas = lasso_sim_results$Best_Lambda,
                     n_lambda = n_lambda)


# Run SaLASSO with that lambda sequence


salasso_results_50 <- lasso_sim(X_LIST_TRAIN = FULL_SIM_LIST_50$X_LIST_TRAIN[from_to],
                                 Y_LIST_TRAIN = FULL_SIM_LIST_50$Y_LIST_TRAIN[from_to],
                                 X_LIST_TEST = FULL_SIM_LIST_50$X_LIST_TEST[from_to],
                                 Y_LIST_TEST = FULL_SIM_LIST_50$Y_LIST_TEST[from_to],
                                 X_LIST_CV = FULL_SIM_LIST_50$X_LIST_CV[from_to],
                                 Y_LIST_CV = FULL_SIM_LIST_50$Y_LIST_CV[from_to],
                                 X_LIST_VAL_CV = FULL_SIM_LIST_50$X_LIST_VAL_CV[from_to],
                                 Y_LIST_VAL_CV = FULL_SIM_LIST_50$Y_LIST_VAL_CV[from_to],
                                 lambda_seq = salasso_seq_50,
                                 use_adweights = TRUE,
                                 use_adweights_lm = TRUE,
                                 use_cv_saenet = FALSE)

save(
  salasso_results_50,
  file = file.path("output", "salasso_results_50.RData")
)

# p = 100 -----------------------------------------------------------------------

load(file.path("output", "FULL_SIM_LIST_100.RData"))

# First determine the lambda sequence 

lasso_sim_results <- lasso_sim(X_LIST_TRAIN = FULL_SIM_LIST_100$X_LIST_TRAIN[from_to],
                               Y_LIST_TRAIN = FULL_SIM_LIST_100$Y_LIST_TRAIN[from_to],
                               X_LIST_TEST = FULL_SIM_LIST_100$X_LIST_TEST[from_to],
                               Y_LIST_TEST = FULL_SIM_LIST_100$Y_LIST_TEST[from_to],
                               X_LIST_CV = FULL_SIM_LIST_100$X_LIST_CV[from_to],
                               Y_LIST_CV = FULL_SIM_LIST_100$Y_LIST_CV[from_to],
                               X_LIST_VAL_CV = FULL_SIM_LIST_100$X_LIST_VAL_CV[from_to],
                               Y_LIST_VAL_CV = FULL_SIM_LIST_100$Y_LIST_VAL_CV[from_to],
                               n_lambda = n_lambda,
                               use_adweights = TRUE,
                               use_adweights_lm = TRUE,
                               use_cv_saenet = FALSE)

salasso_seq_100 <-
  
  create_lambda_sequence(selected_lambdas = lasso_sim_results$Best_Lambda,
                         n_lambda = n_lambda)


# Run SaLASSO with that lambda sequence


salasso_results_100 <- lasso_sim(X_LIST_TRAIN = FULL_SIM_LIST_100$X_LIST_TRAIN[from_to],
                                Y_LIST_TRAIN = FULL_SIM_LIST_100$Y_LIST_TRAIN[from_to],
                                X_LIST_TEST = FULL_SIM_LIST_100$X_LIST_TEST[from_to],
                                Y_LIST_TEST = FULL_SIM_LIST_100$Y_LIST_TEST[from_to],
                                X_LIST_CV = FULL_SIM_LIST_100$X_LIST_CV[from_to],
                                Y_LIST_CV = FULL_SIM_LIST_100$Y_LIST_CV[from_to],
                                X_LIST_VAL_CV = FULL_SIM_LIST_100$X_LIST_VAL_CV[from_to],
                                Y_LIST_VAL_CV = FULL_SIM_LIST_100$Y_LIST_VAL_CV[from_to],
                                lambda_seq = salasso_seq_100,
                                use_adweights = TRUE,
                                use_adweights_lm = TRUE,
                                use_cv_saenet = FALSE)


save(
  salasso_results_100,
  file = file.path("output", "salasso_results_100_II.RData")
)


load("output/salasso_results_100.Rdata")


salasso_results_100_II <- salasso_results_100



salasso_results_100$Best_Lambda |> mean()

salasso_results_100_II$Best_Lambda |> mean()

























