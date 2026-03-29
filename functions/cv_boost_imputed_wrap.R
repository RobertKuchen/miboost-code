cv_boost_imputed_wrap <- function(
    X_TRAIN_LIST, Y_TRAIN_LIST,
    X_VAL_LIST,   Y_VAL_LIST,
    X_FULL_LIST,  Y_FULL_LIST,
    X_TEST_LIST,  Y_TEST_LIST,
    ny = 0.1, mstop = 400, type = c("gaussian","logistic"),
    MIBoost = TRUE, pool = TRUE, pool_threshold = 0,
    center = "auto",
    show_progress = TRUE
){
  type   <- match.arg(type)
  center <- match.arg(center, c("off","auto","force"))
  
  n_sim <- length(X_TRAIN_LIST)
  stopifnot(length(Y_TRAIN_LIST) == n_sim,
            length(X_VAL_LIST)   == n_sim,
            length(Y_VAL_LIST)   == n_sim,
            length(X_FULL_LIST)  == n_sim,
            length(Y_FULL_LIST)  == n_sim,
            length(X_TEST_LIST)  == n_sim,
            length(Y_TEST_LIST)  == n_sim)
  
  # Helpers
  to_matrix <- function(x) { if (!is.matrix(x)) x <- data.matrix(x); storage.mode(x) <- "double"; x }
  binarize01 <- function(y) {
    if (is.numeric(y)) {
      if (all(unique(na.omit(y)) %in% c(0,1))) return(as.numeric(y))
      stop("For logistic, numeric y must be 0/1.")
    }
    if (is.logical(y)) return(as.numeric(y))
    if (is.factor(y) || is.character(y)) {
      yy <- as.factor(y)
      if (nlevels(yy) != 2) stop("For logistic, y must have exactly 2 classes.")
      return(as.numeric(as.integer(yy) - 1L))
    }
    stop("Unsupported y type.")
  }
  to_num <- function(y) if (identical(type, "logistic")) binarize01(y) else as.numeric(y)
  
  # Deep map for CV lists
  map_cv_X <- function(cv_list) lapply(cv_list, function(fold) lapply(fold, to_matrix))
  map_cv_y <- function(cv_list) lapply(cv_list, function(fold) lapply(fold, to_num))
  
  test_losses <- numeric(n_sim)
  best_mstops <- integer(n_sim)
  fits        <- vector("list", n_sim)
  
  if (isTRUE(show_progress)) {
    cat(sprintf("Running %d simulation rounds...\n", n_sim)); flush.console()
  }
  
  s <- 1
  
  for (s in seq_len(n_sim)) {
    if (isTRUE(show_progress)) cat(sprintf("\n[Simulation %d/%d]\n", s, n_sim)); flush.console()
    
    # 1) CV
    Xtr_cv <- map_cv_X(X_TRAIN_LIST[[s]])
    ytr_cv <- map_cv_y(Y_TRAIN_LIST[[s]])
    Xva_cv <- map_cv_X(X_VAL_LIST[[s]])
    yva_cv <- map_cv_y(Y_VAL_LIST[[s]])
    
    X_full_list <- lapply(X_FULL_LIST[[s]], to_matrix)
    y_full_list <- lapply(Y_FULL_LIST[[s]], to_num)
    
    cv_res <- cv_boost_imputed(
      X_train_list = Xtr_cv,
      y_train_list = ytr_cv,
      X_val_list   = Xva_cv,
      y_val_list   = yva_cv,
      X_full       = X_full_list,
      y_full       = y_full_list,
      ny = ny, mstop = mstop, type = type,
      MIBoost = MIBoost, pool = pool, pool_threshold = pool_threshold,
      show_progress = FALSE,
      center = center
    )
    
    best_mstop <- cv_res$best_mstop
    best_mstops[s] <- best_mstop
    if (isTRUE(show_progress)) cat(sprintf("  • Selected mstop = %d\n", best_mstop))
    
    # 2) Fit on full imputations
    if (isTRUE(show_progress)) cat(sprintf("  • Fitting impu_boos on M = %d full imputations...\n", length(X_full_list)))
    fit <- impu_boost(
      X_list = X_full_list,
      y_list = y_full_list,
      ny = ny,
      mstop = best_mstop,
      type = type,
      MIBoost = MIBoost,
      pool = pool,
      pool_threshold = pool_threshold,
      center = center
    )
    fits[[s]] <- fit
    
    # 3) Test
    X_te_list <- lapply(X_TEST_LIST[[s]], to_matrix)
    y_te_list <- lapply(Y_TEST_LIST[[s]], to_num)
    
    im_losses <- numeric(length(X_te_list))
    if (isTRUE(pool)) {
      INT  <- fit$INT
      BETA <- fit$BETA
      for (m_idx in seq_along(X_te_list)) {
        Xte <- X_te_list[[m_idx]]; yte <- y_te_list[[m_idx]]
        if (type == "gaussian") {
          yhat <- as.vector(INT + Xte %*% BETA)
          im_losses[m_idx] <- mean((yte - yhat)^2)
        } else {
          lp <- as.vector(INT + Xte %*% BETA)
          p_hat <- 1 / (1 + exp(-lp))
          p_hat <- pmin(pmax(p_hat, 1e-8), 1 - 1e-8)
          im_losses[m_idx] <- -2 * mean(yte * log(p_hat) + (1 - yte) * log(1 - p_hat))
        }
      }
    } else {
      stopifnot(is.matrix(fit$BETA), length(fit$INT) == nrow(fit$BETA))
      M_fit <- nrow(fit$BETA)
      for (m_idx in seq_along(X_te_list)) {
        j <- ((m_idx - 1) %% M_fit) + 1L
        INTj  <- fit$INT[j]; BETAj <- fit$BETA[j, ]
        Xte <- X_te_list[[m_idx]]; yte <- y_te_list[[m_idx]]
        if (type == "gaussian") {
          yhat <- as.vector(INTj + Xte %*% BETAj)
          im_losses[m_idx] <- mean((yte - yhat)^2)
        } else {
          lp <- as.vector(INTj + Xte %*% BETAj)
          p_hat <- 1 / (1 + exp(-lp))
          p_hat <- pmin(pmax(p_hat, 1e-8), 1 - 1e-8)
          im_losses[m_idx] <- -2 * mean(yte * log(p_hat) + (1 - yte) * log(1 - p_hat))
        }
      }
    }
    
    test_losses[s] <- mean(im_losses)
    if (isTRUE(show_progress)) cat(sprintf("  • Test loss = %.6f (mean over %d imputations)\n",
                                           test_losses[s], length(im_losses)))
  }
  
  list(
    test_loss  = test_losses,
    best_mstop = best_mstops,
    fits       = fits
  )
}
