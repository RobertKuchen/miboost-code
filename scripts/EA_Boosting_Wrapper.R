library(booami)

# p = 50 -----------------------------------------------------------------------

load(file.path("output", "FULL_SIM_LIST_50.RData"))

from_to <- seq_len(FULL_SIM_LIST_50$X_LIST_TRAIN |> length())

EA_Boosting_50 <-

  cv_boost_imputed_wrap(X_TRAIN_LIST = FULL_SIM_LIST_50$X_LIST_CV[from_to],
                        Y_TRAIN_LIST = FULL_SIM_LIST_50$Y_LIST_CV[from_to],
                        X_VAL_LIST = FULL_SIM_LIST_50$X_LIST_VAL_CV[from_to],
                        Y_VAL_LIST = FULL_SIM_LIST_50$Y_LIST_VAL_CV[from_to],
                        X_FULL_LIST = FULL_SIM_LIST_50$X_LIST_TRAIN[from_to],
                        Y_FULL_LIST = FULL_SIM_LIST_50$Y_LIST_TRAIN[from_to],
                        X_TEST_LIST = FULL_SIM_LIST_50$X_LIST_TEST[from_to],
                        Y_TEST_LIST = FULL_SIM_LIST_50$Y_LIST_TEST[from_to],
                        pool = TRUE, MIBoost = FALSE)


save(
  EA_Boosting_50,
  file = file.path("output", "EA_Boosting_50.RData")
)


# p = 100 ----------------------------------------------------------------------

load(file.path("output", "FULL_SIM_LIST_100.RData"))

EA_Boosting_100 <-

  cv_boost_imputed_wrap(X_TRAIN_LIST = FULL_SIM_LIST_100$X_LIST_CV[from_to],
                        Y_TRAIN_LIST = FULL_SIM_LIST_100$Y_LIST_CV[from_to],
                        X_VAL_LIST = FULL_SIM_LIST_100$X_LIST_VAL_CV[from_to],
                        Y_VAL_LIST = FULL_SIM_LIST_100$Y_LIST_VAL_CV[from_to],
                        X_FULL_LIST = FULL_SIM_LIST_100$X_LIST_TRAIN[from_to],
                        Y_FULL_LIST = FULL_SIM_LIST_100$Y_LIST_TRAIN[from_to],
                        X_TEST_LIST = FULL_SIM_LIST_100$X_LIST_TEST[from_to],
                        Y_TEST_LIST = FULL_SIM_LIST_100$Y_LIST_TEST[from_to],
                        pool = TRUE, MIBoost = FALSE)


save(
  EA_Boosting_100,
  file = file.path("output", "EA_Boosting_100.RData")
)

