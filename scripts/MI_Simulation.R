library(mice)
library(tidyverse)
library(miselect)
library(MASS)
library(stats)
library(booami)

n_total <- 500   

n_sim <- 50

M <- 10

k <- 5

n_train <- 0.8 * n_total

folds <- cut(seq_len(n_train), breaks = k, labels = FALSE)


# Simulate Data with missing values ---------------------------------------

p_vector <- c(50,100)


for (i_p in seq_along(p_vector)) {


p <- p_vector[i_p]


set.seed(123)

data_list_temp <- replicate_datasets(n_sims = n_sim, p = p)

data_list <- list()

for(sim_iter in seq_len(n_sim)){

  data_list[[sim_iter]] <- data_list_temp[[sim_iter]]$data


}

# Training-Test Partitioning -------------------------------------------------------


X_LIST_TRAIN <- Y_LIST_TRAIN <- X_LIST_TEST <- Y_LIST_TEST <- list()

X_list_train <- y_list_train <- X_list_test <- y_list_test <- list()


data_train_list <- data_test_list <- list()


n_total <- 500

sim_iter <- 1


for (sim_iter in seq_len(n_sim)) {

  # --- Split data into training and test sets (80/20) ---
  data <- data_list[[sim_iter]]
  train_idx <- sort(sample(seq_len(n_total), size = floor(0.8 * n_total)))

  data_train <- data[train_idx, ]
  data_test  <- data[-train_idx, ]

  data_train_list[[sim_iter]] <- data_train
  data_test_list[[sim_iter]]  <- data_test

  # --- Build predictor matrix from TRAIN only (no leakage) ---
  quickpred_train <- quickpred(
    data    = data_train,
    method  = "spearman",
    exclude = "y",
    mincor  = 0.1
  )

  # --- Impute training data; then impute test using the same imputation models ---
  imp_train <- mice(data_train, m = M, predictorMatrix = quickpred_train)
  imp_test  <- mice.mids(imp_train, newdata = data_test)

  # --- Prepare storage for centered (X-only) datasets ---
  completed_train_list <- vector("list", M)
  completed_test_list  <- vector("list", M)

  # Indices for predictors and response (assumes X=1:p, y=p+1)
  x_cols <- seq_len(p)
  y_col  <- p + 1

  # ============================================================
  # GRAND-MEAN CENTERING (TRAIN only, pooled across imputations)
  # Step 1: compute mean in each imputed TRAIN dataset
  # Step 2: grand mean = average of those means
  # Step 3: center each imputed dataset using the same grand mean
  # ============================================================

  # Identify numeric X columns once (leave factors/characters alone)
  num_x <- vapply(data_train[, x_cols, drop = FALSE], is.numeric, logical(1L))
  x_num_cols <- x_cols[num_x]

  grand_means <- NULL
  if (length(x_num_cols) > 0) {

    means_mat <- matrix(NA_real_, nrow = M, ncol = length(x_num_cols))
    colnames(means_mat) <- names(data_train)[x_num_cols]

    for (m_i in seq_len(M)) {
      train_m_tmp <- complete(imp_train, m_i)
      means_mat[m_i, ] <- colMeans(train_m_tmp[, x_num_cols, drop = FALSE])
    }

    grand_means <- colMeans(means_mat)
  }

  # --- Create centered completed datasets for each imputation ---
  for (m_i in seq_len(M)) {

    # Extract completed datasets for imputation m_i
    train_m <- complete(imp_train, m_i)
    test_m  <- complete(imp_test,  m_i)

    # --- Center ONLY the numeric predictor columns using GRAND means (leave y alone) ---
    if (length(x_num_cols) > 0) {
      train_m[, x_num_cols] <- sweep(train_m[, x_num_cols, drop = FALSE], 2, grand_means, "-")
      test_m[,  x_num_cols] <- sweep(test_m[,  x_num_cols, drop = FALSE], 2, grand_means, "-")
    }

    # Store centered datasets
    completed_train_list[[m_i]] <- train_m
    completed_test_list[[m_i]]  <- test_m
  }

  # --- Split into X / y lists of matrices/vectors for each imputation ---
  X_list_test  <- vector("list", M)
  y_list_test  <- vector("list", M)
  X_list_train <- vector("list", M)
  y_list_train <- vector("list", M)

  for (m_i in seq_len(M)) {
    X_list_test[[m_i]]   <- as.matrix(completed_test_list[[m_i]][, x_cols, drop = FALSE])
    y_list_test[[m_i]]   <- completed_test_list[[m_i]][, y_col]

    X_list_train[[m_i]]  <- as.matrix(completed_train_list[[m_i]][, x_cols, drop = FALSE])
    y_list_train[[m_i]]  <- completed_train_list[[m_i]][, y_col]
  }

  X_LIST_TEST[[sim_iter]]  <- X_list_test
  Y_LIST_TEST[[sim_iter]]  <- y_list_test
  X_LIST_TRAIN[[sim_iter]] <- X_list_train
  Y_LIST_TRAIN[[sim_iter]] <- y_list_train
}


# Cross Validation Settings -----------------------------------------------

X_LIST_CV <- Y_LIST_CV <- X_LIST_VAL_CV <- Y_LIST_VAL_CV <- list()

X_list_cv <- y_list_cv <- X_list_val_cv <- y_list_val_cv <- list()

X_list <- y_list <- X_list_val <- y_list_val <- list()

data_train_II_list <- data_val_list <- list()

for (sim_iter in seq_len(n_sim)) {
  data_train_II_list[[sim_iter]] <- vector("list", k)
  data_val_list[[sim_iter]]      <- vector("list", k)
}

for (sim_iter in seq_len(n_sim)) {

  data_train <- data_train_list[[sim_iter]]

  for (cv_iter in seq_len(k)) {

    data_train_II <- data_train[folds != cv_iter, ]
    data_val      <- data_train[folds == cv_iter, ]

    # store raw splits (optional)
    data_train_II_list[[sim_iter]][[cv_iter]] <- data_train_II
    data_val_list[[sim_iter]][[cv_iter]]      <- data_val

    # predictor matrix from TRAIN only (no leakage)
    quickpred_train_II <- quickpred(
      data    = data_train_II,
      method  = "spearman",
      exclude = "y",
      mincor  = 0.10
    )

    # impute: fit on train_II, apply models to val
    imp_train_II <- mice(data_train_II, m = M, predictorMatrix = quickpred_train_II)
    imp_val      <- mice.mids(imp_train_II, newdata = data_val)

    # storage for centered (X-only) datasets
    completed_train_II_list <- vector("list", M)
    completed_val_list      <- vector("list", M)

    # indices
    x_cols <- seq_len(p)     # predictors
    y_col  <- p + 1          # response (left as-is)

    # ============================================================
    # GRAND-MEAN CENTERING (TRAIN_II only, pooled across imputations)
    # Step 1: compute mean in each imputed TRAIN_II dataset
    # Step 2: grand mean = average of those means
    # Step 3: center each imputed dataset using the same grand mean
    # ============================================================

    # Identify numeric X columns once (leave factors/characters alone)
    num_x <- vapply(data_train_II[, x_cols, drop = FALSE], is.numeric, logical(1L))
    x_num_cols <- x_cols[num_x]

    grand_means <- NULL
    if (length(x_num_cols) > 0) {

      means_mat <- matrix(NA_real_, nrow = M, ncol = length(x_num_cols))
      colnames(means_mat) <- names(data_train_II)[x_num_cols]

      for (m_i in seq_len(M)) {
        train_II_m_tmp <- complete(imp_train_II, m_i)
        means_mat[m_i, ] <- colMeans(train_II_m_tmp[, x_num_cols, drop = FALSE])
      }

      grand_means <- colMeans(means_mat)
    }

    for (m_i in seq_len(M)) {
      # completed datasets
      train_II_m <- complete(imp_train_II, m_i)
      val_m      <- complete(imp_val,      m_i)

      # Center ONLY numeric predictor columns using GRAND means (no scaling; y untouched)
      if (length(x_num_cols) > 0) {
        train_II_m[, x_num_cols] <- sweep(train_II_m[, x_num_cols, drop = FALSE], 2, grand_means, "-")
        val_m[,      x_num_cols] <- sweep(val_m[,      x_num_cols, drop = FALSE], 2, grand_means, "-")
      }

      completed_train_II_list[[m_i]] <- train_II_m
      completed_val_list[[m_i]]      <- val_m
    }

    # split into X/y lists for this CV fold
    X_list      <- vector("list", M)
    y_list      <- vector("list", M)
    X_list_val  <- vector("list", M)
    y_list_val  <- vector("list", M)

    for (m_i in seq_len(M)) {
      X_list[[m_i]]     <- as.matrix(completed_train_II_list[[m_i]][, x_cols, drop = FALSE])
      y_list[[m_i]]     <- completed_train_II_list[[m_i]][, y_col]
      X_list_val[[m_i]] <- as.matrix(completed_val_list[[m_i]][,   x_cols, drop = FALSE])
      y_list_val[[m_i]] <- completed_val_list[[m_i]][,   y_col]
    }

    X_list_cv[[cv_iter]]      <- X_list
    y_list_cv[[cv_iter]]      <- y_list
    X_list_val_cv[[cv_iter]]  <- X_list_val
    y_list_val_cv[[cv_iter]]  <- y_list_val
  }

  X_LIST_CV[[sim_iter]]      <- X_list_cv
  Y_LIST_CV[[sim_iter]]      <- y_list_cv
  X_LIST_VAL_CV[[sim_iter]]  <- X_list_val_cv
  Y_LIST_VAL_CV[[sim_iter]]  <- y_list_val_cv
}


assign(
  paste0("FULL_SIM_LIST_", p_vector[i_p]),
  setNames(list(X_LIST_TRAIN, Y_LIST_TRAIN,
                X_LIST_TEST, Y_LIST_TEST,
                X_LIST_CV, Y_LIST_CV,
                X_LIST_VAL_CV, Y_LIST_VAL_CV), 
           c("X_LIST_TRAIN", "Y_LIST_TRAIN",
             "X_LIST_TEST", "Y_LIST_TEST",
             "X_LIST_CV", "Y_LIST_CV",
             "X_LIST_VAL_CV", "Y_LIST_VAL_CV"))
)

dir.create("output", showWarnings = FALSE, recursive = TRUE)


obj_name <- paste0("FULL_SIM_LIST_", p_vector[i_p])


save(
  list = obj_name,
  file = file.path("output", paste0("FULL_SIM_LIST_", p_vector[i_p], ".RData"))
)

}
















