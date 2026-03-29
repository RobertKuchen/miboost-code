results_by_lambda <- function(lasso_object, X_list_val,
                              y_list_val){
  
  
  p <- length(lasso_object$coef[1,,]) - 1
  
  M <- length(y_list_val)
  
  lambda_list <- lasso_object[["lambda"]]
  
  n_lambda <- length(lambda_list)
  
  all_coefs_sets <- matrix(0, nrow = p + 1, ncol = n_lambda)
  
  i <- 1
  
  OOS_CV <- matrix(0, nrow = M, ncol = n_lambda)
  
  
  for(i in seq_len(n_lambda)){
    
    coef_list <- coef(lasso_object, lambda = lambda_list[i], alpha = 1)
    
    all_coefs_sets[,i] <- coef_list
    
    coef_list[1] <- mean(unlist(y_list_val))
    
    # all_coefs_sets[,i] <- Reduce("+", coef_list) / length(coef_list)
    
  }
  
  
  for(lambda in seq_len(n_lambda)){
    
    for(m in seq_len(M)){
      
      OOS_CV[m,lambda] <-
        
        sum((y_list_val[[m]] -
               
               all_coefs_sets[1,lambda] -
               X_list_val[[m]] %*% all_coefs_sets[-1,lambda])^2)
      
    }
    
  }
  
  return(colMeans(OOS_CV))
  
}

.calc_weights_SaLASSO <- function(X_list, y_list,
                                  X_list_val, y_list_val,
                                  use_adweights,
                                  use_adweights_lm, use_cv_saenet,
                                  lambda_seq){
  
  n_train_II <- length(y_list[[1]])
  
  M <- length(y_list)
  
  p <- ncol(X_list[[1]])
  
  
  if(use_adweights){
    
    if(use_adweights_lm){
      
      init_coef_mat <- matrix(0, ncol = M, nrow = p)
      
      
      for(m in seq_len(M)){
        
        init_coef_mat[,m] <- coef(lm(y_list[[m]] ~ X_list[[m]]))[-1]
        
      }
      
      init_coef_vec <- rowMeans(init_coef_mat)
      
      
    }else{
      
      if(use_cv_saenet){
        
        
        initial_senet <- cv.saenet(x = X_list,
                                   y = y_list,
                                   adWeight = rep(1, p),
                                   pf = pf,
                                   weights = rep(1/M, n_train_II),
                                   lambda = lambda_seq,
                                   alpha = 1)
        
        
        init_coef_vec <- coef(initial_senet)[-1]
        
        
        
      }else{
        
        
        initial_senet <-
          
          saenet(x = X_list,
                 y = y_list,
                 adWeight = rep(1, p),
                 pf = pf,
                 lambda = lambda_seq,
                 weights = rep(1/M, n_train_II),
                 alpha = 1)
        
        initial_losses <- .results_by_lamalph(initial_senet,
                                              X_list_val, y_list_val)
        
        
        
        results_by_lambda(lasso_object = eval(parse(text = "lasso_0 ")),
                          X_list_val = X_list_val_cv[[cv_iter]],
                          y_list_val = y_list_val_cv[[cv_iter]])
        
        
        
        best_initial_lambda <- lambda_seq[.best_lamalph_no(initial_losses)[1]]
        
        
        init_coef_vec <- coef(initial_senet,
                              lambda = best_initial_lambda,
                              alpha = best_initial_alpha)[-1]
        
      }
      
    }
    
    nu <- log(p)/log(M * n_train_II)
    
    gamma <- (2 * nu)/(1 - nu) + 1
    
    adapt_weights <- (abs(init_coef_vec) + 1/(n_train_II * M))^(-gamma)
    
  }else{
    
    adapt_weights <- rep(1, p)
    
  }
  
  return(adapt_weights)
  
}



lasso_cv <- function(X_list_cv, y_list_cv,
                     X_list_val_cv, y_list_val_cv,
                     use_adweights, use_adweights_lm,
                     use_cv_saenet,
                     lambda_seq, n_lambda = NULL){
  
  k <- length(X_list_cv)
  
  if(is.null(lambda_seq)){
    
    lambda_mat <- matrix(0, nrow = k, ncol = n_lambda)
    
  }else{
    
    lambda_mat <- matrix(0, nrow = k, ncol = length(lambda_seq))
    
  }
  
  n_train_II <- nrow(X_list_cv[[1]][[1]])
  
  p <- ncol(X_list_cv[[1]][[1]])
  
  M <- length(X_list_cv[[1]])
  
  
  if(is.null(n_lambda)){
    
    n_lambda <- length(lambda_seq)
    
  }else{
    
    lambda_seq <- NULL
    
  }
  
  
  OOS_CV <- matrix(0, nrow = M, ncol = n_lambda)
  
  OOS_CV_mat <- matrix(0, nrow = k, ncol = n_lambda)
  
  
  
  for(cv_iter in seq_len(k)){
    
    X_list <- X_list_cv[[cv_iter]]
    
    y_list <- y_list_cv[[cv_iter]]
    
    
    X_list_val <- X_list_val_cv[[cv_iter]]
    
    y_list_val <- y_list_val_cv[[cv_iter]]
    
    
    adapt_weights <- .calc_weights_SaLASSO(X_list = X_list,
                                           y_list = y_list,
                                           X_list_val = X_list_val,
                                           y_list_val = y_list_val,
                                           use_adweights = TRUE,
                                           use_adweights_lm = TRUE,
                                           use_cv_saenet = use_cv_saenet,
                                           lambda_seq = lambda_seq)
    
    lasso_0 <- saenet(x = X_list,
                      y = y_list,
                      adWeight = adapt_weights,
                      pf = rep(1, p),
                      weights = rep(1/M, n_train_II),
                      lambda = lambda_seq,
                      alpha = 1)
    
    
    lambda_mat[cv_iter,] <- lasso_0$lambda
    
    
    # All_LAMBDAS_mat <- rbind(All_LAMBDAS_mat, saenet_0$lambda)
    
    
    OOS_CV_mat[cv_iter,] <- results_by_lambda(eval(parse(text = "lasso_0")),
                                              X_list_val = X_list_val_cv[[cv_iter]],
                                              y_list_val = y_list_val_cv[[cv_iter]])
    
  }
  
  final_lambda <- mean(lambda_mat[,which.min(colMeans(OOS_CV_mat))])
  
  return(list(best_lambda = final_lambda))
  
}


lasso_sim <- function(X_LIST_TRAIN, Y_LIST_TRAIN,
                      X_LIST_TEST, Y_LIST_TEST,
                      X_LIST_CV, Y_LIST_CV,
                      X_LIST_VAL_CV, Y_LIST_VAL_CV,
                      lambda_seq, n_lambda = NULL,
                      use_adweights, use_adweights_lm,
                      use_cv_saenet){
  
  
  if(is.null(n_lambda)){
    
    n_lambda <- length(lambda_seq)
    
  }else{
    
    lambda_seq <- NULL
    
  }
  
  
  n_train <- length(Y_LIST_TRAIN[[1]][[1]])
  
  
  k <- length(Y_LIST_CV[[1]])
  
  n_sim <- length(Y_LIST_TRAIN)
  
  best_lambda_vec <- MSPE_vec <-numeric(n_sim)
  
  M <- length(Y_LIST_TRAIN[[1]])
  
  p <- ncol(X_LIST_TRAIN[[1]][[1]])
  
  BETA_FINAL_mat <- matrix(0, nrow = n_sim, ncol = p + 1)
  
  Included_Covariates <- matrix(0, nrow = n_sim, ncol = p)
  
  MSPE <- numeric(M)
  
  for(sim_iter in seq_len(n_sim)){
    
    X_list_train <- X_LIST_TRAIN[[sim_iter]]
    y_list_train <- Y_LIST_TRAIN[[sim_iter]]
    X_list_test  <- X_LIST_TEST[[sim_iter]]
    y_list_test  <- Y_LIST_TEST[[sim_iter]]
    X_list_cv    <- X_LIST_CV[[sim_iter]]
    y_list_cv    <- Y_LIST_CV[[sim_iter]]
    X_list_val_cv <- X_LIST_VAL_CV[[sim_iter]]
    y_list_val_cv <- Y_LIST_VAL_CV[[sim_iter]]
    
    
    lambda_mat <- matrix(0, nrow = k, ncol = n_lambda)
    
    
    cv_results <- lasso_cv(X_list_cv = X_list_cv,
                           y_list_cv = y_list_cv,
                           X_list_val_cv = X_list_val_cv,
                           y_list_val_cv = y_list_val_cv,
                           use_adweights = use_adweights,
                           use_adweights_lm = use_adweights_lm,
                           use_cv_saenet = use_cv_saenet,
                           lambda_seq = lambda_seq,
                           n_lambda = n_lambda)
    
    
    best_lambda_vec[sim_iter] <- cv_results$best_lambda
    
    
    adapt_weights <- .calc_weights_SaLASSO(X_list = X_list_train,
                                           y_list = y_list_train,
                                           X_list_val = X_list_test,
                                           y_list_val = y_list_test,
                                           use_adweights = use_adweights,
                                           use_adweights_lm = use_adweights_lm,
                                           use_cv_saenet = use_cv_saenet,
                                           lambda_seq = lambda_seq)
    
    
    
    final_lasso <- saenet(x = X_list_train,
                          y = y_list_train,
                          adWeight = adapt_weights,
                          pf = rep(1, p),
                          weights = rep(1, n_train),
                          alpha = 1,
                          lambda = cv_results$best_lambda)
    
    
    beta_final <- coef(final_lasso, lambda = cv_results$best_lambda, alpha = 1)
    
    beta_final[1] <- mean(unlist(y_list_train))
    
    
    BETA_FINAL_mat[sim_iter,] <- beta_final
    
    
    included_cov <- colnames(Included_Covariates) %in%
      names(beta_final[beta_final !=0][-1])
    
    Included_Covariates[sim_iter,included_cov] <- 1
    
    
    for(m in seq_len(M)){
      
      MSPE[[m]] <-
        
        mean((y_list_test[[m]] - beta_final[1] -
                X_list_test[[m]] %*% beta_final[-1])^2)
      
    }
    
    
    MSPE_vec[sim_iter] <- mean(MSPE)
    
    
    print(sim_iter)
    
  }
  
  
  return(list(BETA = BETA_FINAL_mat, MSPE = MSPE_vec,
              Best_Lambda = best_lambda_vec))
  
}

