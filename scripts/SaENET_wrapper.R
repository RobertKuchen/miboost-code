library(miselect)
library(tidyverse)

load(file.path("output", "FULL_SIM_LIST_50.RData"))

n_sim <- length(FULL_SIM_LIST_50$Y_LIST_TEST)

from_to <- seq_len(n_sim)

n_alpha <- 21

n_lambda <- 100

# p = 50 -----------------------------------------------------------------------

# First determine the lambda sequence 

saenet_sim_results <- saenet_sim(X_LIST_TRAIN = FULL_SIM_LIST_50$X_LIST_TRAIN[from_to],
                               Y_LIST_TRAIN = FULL_SIM_LIST_50$Y_LIST_TRAIN[from_to],
                               X_LIST_TEST = FULL_SIM_LIST_50$X_LIST_TEST[from_to],
                               Y_LIST_TEST = FULL_SIM_LIST_50$Y_LIST_TEST[from_to],
                               X_LIST_CV = FULL_SIM_LIST_50$X_LIST_CV[from_to],
                               Y_LIST_CV = FULL_SIM_LIST_50$Y_LIST_CV[from_to],
                               X_LIST_VAL_CV = FULL_SIM_LIST_50$X_LIST_VAL_CV[from_to],
                               Y_LIST_VAL_CV = FULL_SIM_LIST_50$Y_LIST_VAL_CV[from_to],
                               n_alpha = n_alpha,
                               n_lambda = n_lambda,
                               use_adweights = TRUE,
                               use_adweights_lm = TRUE,
                               use_cv_saenet = FALSE,
                               alpha_seq = NULL,
                               lambda_seq = NULL)

saenet_seq_50 <-

  create_lambda_sequence(selected_lambdas = saenet_sim_results$Best_Lambda,
                       n_lambda = n_lambda)

save(saenet_seq_50, file = "saenet_seq_50.Rdata")


# Run SaLASSO with that lambda sequence

saenet_results_50 <- saenet_sim(X_LIST_TRAIN = FULL_SIM_LIST_50$X_LIST_TRAIN[from_to],
                                Y_LIST_TRAIN = FULL_SIM_LIST_50$Y_LIST_TRAIN[from_to],
                                X_LIST_TEST = FULL_SIM_LIST_50$X_LIST_TEST[from_to],
                                Y_LIST_TEST = FULL_SIM_LIST_50$Y_LIST_TEST[from_to],
                                X_LIST_CV = FULL_SIM_LIST_50$X_LIST_CV[from_to],
                                Y_LIST_CV = FULL_SIM_LIST_50$Y_LIST_CV[from_to],
                                X_LIST_VAL_CV = FULL_SIM_LIST_50$X_LIST_VAL_CV[from_to],
                                Y_LIST_VAL_CV = FULL_SIM_LIST_50$Y_LIST_VAL_CV[from_to],
                                n_alpha = n_alpha,
                                use_adweights = TRUE,
                                use_adweights_lm = TRUE,
                                use_cv_saenet = FALSE,
                                alpha_seq = NULL,
                                lambda_seq = saenet_seq_50)

save(
  saenet_results_50,
  file = file.path("output", "saenet_results_50.RData")
)


# p = 100 -----------------------------------------------------------------------

load(file.path("output", "FULL_SIM_LIST_100.RData"))

# First determine the lambda sequence 

saenet_sim_results <- saenet_sim(X_LIST_TRAIN = FULL_SIM_LIST_100$X_LIST_TRAIN[from_to],
                                 Y_LIST_TRAIN = FULL_SIM_LIST_100$Y_LIST_TRAIN[from_to],
                                 X_LIST_TEST = FULL_SIM_LIST_100$X_LIST_TEST[from_to],
                                 Y_LIST_TEST = FULL_SIM_LIST_100$Y_LIST_TEST[from_to],
                                 X_LIST_CV = FULL_SIM_LIST_100$X_LIST_CV[from_to],
                                 Y_LIST_CV = FULL_SIM_LIST_100$Y_LIST_CV[from_to],
                                 X_LIST_VAL_CV = FULL_SIM_LIST_100$X_LIST_VAL_CV[from_to],
                                 Y_LIST_VAL_CV = FULL_SIM_LIST_100$Y_LIST_VAL_CV[from_to],
                                 n_alpha = n_alpha,
                                 n_lambda = n_lambda,
                                 use_adweights = TRUE,
                                 use_adweights_lm = TRUE,
                                 use_cv_saenet = FALSE,
                                 alpha_seq = NULL,
                                 lambda_seq = NULL)

saenet_seq_100 <-
  
  create_lambda_sequence(selected_lambdas = saenet_sim_results$Best_Lambda,
                         n_lambda = n_lambda)

save(saenet_seq_100, file = "saenet_seq_100.Rdata")


# Run SaLASSO with that lambda sequence

saenet_results_100 <- saenet_sim(X_LIST_TRAIN = FULL_SIM_LIST_100$X_LIST_TRAIN[from_to],
                                Y_LIST_TRAIN = FULL_SIM_LIST_100$Y_LIST_TRAIN[from_to],
                                X_LIST_TEST = FULL_SIM_LIST_100$X_LIST_TEST[from_to],
                                Y_LIST_TEST = FULL_SIM_LIST_100$Y_LIST_TEST[from_to],
                                X_LIST_CV = FULL_SIM_LIST_100$X_LIST_CV[from_to],
                                Y_LIST_CV = FULL_SIM_LIST_100$Y_LIST_CV[from_to],
                                X_LIST_VAL_CV = FULL_SIM_LIST_100$X_LIST_VAL_CV[from_to],
                                Y_LIST_VAL_CV = FULL_SIM_LIST_100$Y_LIST_VAL_CV[from_to],
                                n_alpha = n_alpha,
                                use_adweights = TRUE,
                                use_adweights_lm = TRUE,
                                use_cv_saenet = FALSE,
                                alpha_seq = NULL,
                                lambda_seq = saenet_seq_100)

save(
  saenet_results_100,
  file = file.path("output", "saenet_results_100.RData")
)


