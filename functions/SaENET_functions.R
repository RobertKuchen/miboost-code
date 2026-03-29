.results_by_lamda_alpha <- function(saenet_object, X_list_val,
                                    y_list_val){
  
  p <- length(saenet_object$coef[1,1,]) - 1
  
  M <- length(y_list_val)
  
  alpha_vec <- saenet_object[["alpha"]]
  
  lambda_vec <-  saenet_object[["lambda"]]
  
  n_alpha <- length(alpha_vec)
  
  n_lambda <- length(lambda_vec)
  
  
  OOS_CV <- list()
  
  for(m in seq_len(M)){
    
    OOS_CV[[m]] <- matrix(0, ncol = n_alpha, nrow = n_lambda)
    
    
  }
  
  
  for(m in seq_len(M)){
    
    for(i in seq_len(n_alpha)){
      
      for(j in seq_len(n_lambda)){
        
        est_coefs <- saenet_object$coef[j, i,]
        
        OOS_CV[[m]][j, i] <-
          
          sum((y_list_val[[m]] -
                 
                 est_coefs[1] - X_list_val[[m]] %*% est_coefs[-1] )^2)
        
        
        
      }
      
      
      
    }
    
  }
  
  OOS_CV_tot <- Reduce("+", OOS_CV) /length(OOS_CV)
  
  return(OOS_CV_tot)
  
  
}

.best_lamda_alpha_no <- function(OOS_CV_tot){
  
  n_lambda <- nrow(OOS_CV_tot)
  
  
  if((which.min(OOS_CV_tot) %% n_lambda) == 0){
    
    lambda_best <- n_lambda
    
  }else{
    
    lambda_best <- which.min(OOS_CV_tot) %% n_lambda
    
  }
  
  
  
  return(c(lambda_best,
           ceiling(which.min(OOS_CV_tot)/n_lambda)))
  
}



.calc_weights_SaENET <- function(X_list, y_list,
                          X_list_val, y_list_val,
                          use_adweights,
                          use_adweights_lm, use_cv_saenet,
                          alpha_seq, lambda_seq){
  
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
                                   alpha = alpha_seq)
        
        
        init_coef_vec <- coef(initial_senet)[-1]
        
        
        
      }else{
        
        
        initial_senet <-
          
          saenet(x = X_list,
                 y = y_list,
                 adWeight = rep(1, p),
                 pf = pf,
                 lambda = lambda_seq,
                 nlambda = n_lambda,
                 weights = rep(1/M, n_train_II),
                 alpha = alpha_seq)
        
        initial_losses <- .results_by_lamda_alpha(initial_senet,
                                                  X_list_val, y_list_val)
        
        
        best_initial_lambda <- lambda_seq[.best_lamda_alpha_no(initial_losses)[1]]
        
        best_initial_alpha <- alpha_seq[u.best_lamda_alpha_no(initial_losses)[2]]
        
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


saenet_cv <- function(X_list_cv, y_list_cv,
                      X_list_val_cv, y_list_val_cv,
                      use_adweights, use_adweights_lm,
                      use_cv_saenet,
                      n_lambda, lambda_seq,
                      n_alpha, alpha_seq){
  
  
  if(is.null(n_lambda)){
    
    n_lambda <- length(lambda_seq)
    
  }else{
    
    lambda_seq <- NULL
    
  }
  
  
  if(is.null(n_alpha)){
    
    n_alpha <- length(alpha_seq)
    
  }else{
    
    alpha_seq <- seq(0, 1, length.out = n_alpha)
    
  }
  
  n_train_II <- nrow(X_list_cv[[1]][[1]])
  
  k <- length(X_list_cv)
  
  p <- ncol(X_list_cv[[1]][[1]])
  
  M <- length(X_list_cv[[1]])
  
  
  lambda_mat <- matrix(0, nrow = k, ncol = n_lambda)
  
  OOS_CV_list <- list()
  
  
  for(cv_iter in seq_len(k)){
    
    X_list <- X_list_cv[[cv_iter]]
    
    y_list <- y_list_cv[[cv_iter]]
    
    
    X_list_val <- X_list_val_cv[[cv_iter]]
    
    y_list_val <- y_list_val_cv[[cv_iter]]
    
    
    adapt_weights <- .calc_weights_SaENET(X_list = X_list,
                                   y_list = y_list,
                                   X_list_val = X_list_val,
                                   y_list_val = y_list_val,
                                   use_adweights = TRUE,
                                   use_adweights_lm = TRUE,
                                   use_cv_saenet = use_cv_saenet,
                                   lambda_seq = lambda_seq,
                                   alpha_seq = alpha_seq)
    
    saenet_0 <- saenet(x = X_list,
                       y = y_list,
                       adWeight = adapt_weights, pf = rep(1, p),
                       weights = rep(1/M, n_train_II),
                       lambda = lambda_seq,
                       nlambda = n_lambda,
                       alpha = alpha_seq)
    
    
    lambda_mat[cv_iter,] <- saenet_0$lambda
    
    
    # All_LAMBDAS_mat <- rbind(All_LAMBDAS_mat, saenet_0$lambda)
    
    
    
    OOS_CV_list[[cv_iter]] <- .results_by_lamda_alpha(saenet_object =
                                                        eval(parse(text = "saenet_0")),
                                                      X_list_val = X_list_val,
                                                      y_list_val = y_list_val)
    
  }
  
  
  OOS_CV_tot <- Reduce("+", OOS_CV_list)
  
  which.min(OOS_CV_tot)
  
  
  
  final_lambda <- mean(lambda_mat[,.best_lamda_alpha_no(OOS_CV_tot = OOS_CV_tot)[1]])
  
  final_alpha <- alpha_seq[unlist(.best_lamda_alpha_no(OOS_CV_tot = OOS_CV_tot)[2])]
  
  
  
  return(list(CV_error = OOS_CV_tot, best_alpha = final_alpha,
              best_lambda = final_lambda))
  
  
}


saenet_sim <- function(X_LIST_TRAIN, Y_LIST_TRAIN,
                       X_LIST_TEST, Y_LIST_TEST,
                       X_LIST_CV, Y_LIST_CV,
                       X_LIST_VAL_CV, Y_LIST_VAL_CV,
                       n_alpha, alpha_seq,
                       lambda_seq = NULL, n_lambda = NULL,
                       use_adweights, use_adweights_lm,
                       use_cv_saenet){
  
  if(is.null(n_lambda)){
    
    n_lambda <- length(lambda_seq)
    
  }else{
    
    lambda_seq <- NULL
    
  }
  
  
  if(is.null(n_alpha)){
    
    n_alpha <- length(alpha_seq)
    
  }else{
    
    alpha_seq <- seq(0, 1, length.out = n_alpha)
    
  }
  
  n_train <- length(Y_LIST_TRAIN[[1]][[1]])
  
  
  k <- length(Y_LIST_CV[[1]])
  
  n_sim <- length(Y_LIST_TRAIN)
  
  ALPHA_vec <- best_lambda_vec <- MSPE_vec <- numeric(n_sim)
  
  M <- length(Y_LIST_TRAIN[[1]])
  
  p <- ncol(X_LIST_TRAIN[[1]][[1]])
  
  BETA_FINAL_mat <- matrix(0, nrow = n_sim, ncol = p + 1)
  
  Included_Covariates <- matrix(0, nrow = n_sim, ncol = p)
  
  MSPE <- numeric(M)
  
  MSPE_vec <- numeric(n_sim)
  
  
  for(sim_iter in seq_len(n_sim)){
    
    X_list_train <- X_LIST_TRAIN[[sim_iter]]
    y_list_train <- Y_LIST_TRAIN[[sim_iter]]
    X_list_test  <- X_LIST_TEST[[sim_iter]]
    y_list_test  <- Y_LIST_TEST[[sim_iter]]
    X_list_cv    <- X_LIST_CV[[sim_iter]]
    y_list_cv    <- Y_LIST_CV[[sim_iter]]
    X_list_val_cv <- X_LIST_VAL_CV[[sim_iter]]
    y_list_val_cv <- Y_LIST_VAL_CV[[sim_iter]]
    
    
    cv_results <- saenet_cv(X_list_cv = X_list_cv,
                            y_list_cv = y_list_cv,
                            X_list_val_cv = X_list_val_cv,
                            y_list_val_cv = y_list_val_cv,
                            use_adweights = use_adweights,
                            use_adweights_lm = use_adweights_lm,
                            use_cv_saenet = use_cv_saenet,
                            lambda_seq = lambda_seq,
                            n_lambda = n_lambda,
                            alpha_seq = alpha_seq,
                            n_alpha = n_alpha)
    
    
    ALPHA_vec[sim_iter] <- cv_results$best_alpha
    
    best_lambda_vec[sim_iter] <- cv_results$best_lambda
    
    
    adapt_weights <- .calc_weights_SaENET(X_list = X_list_train,
                                   y_list = y_list_train,
                                   X_list_val = X_list_test,
                                   y_list_val = y_list_test,
                                   use_adweights = use_adweights,
                                   use_adweights_lm = use_adweights_lm,
                                   use_cv_saenet = use_cv_saenet,
                                   lambda_seq = lambda_seq,
                                   alpha_seq = alpha_seq)
    
    
    finael_saenet <- saenet(x = X_list_train,
                            y = y_list_train,
                            adWeight = adapt_weights,
                            pf = rep(1, p),
                            weights = rep(1, n_train),
                            alpha = cv_results$best_alpha,
                            lambda = cv_results$best_lambda)
    
    
    beta_final <- coef(finael_saenet,
                       lambda = finael_saenet$lambda,
                       alpha = finael_saenet$alpha)
    
    beta_final[1] <- mean(unlist(y_list_train))
    
    
    BETA_FINAL_mat[sim_iter,] <- beta_final
    
    
    
    included_cov <- colnames(Included_Covariates) %in%
      names(beta_final[beta_final !=0][-1])
    
    Included_Covariates[sim_iter,included_cov] <- 1
    
    
    for(m in seq_len(M)){
      
      MSPE[m] <-
        
        mean((y_list_test[[m]] - beta_final[1] -
                X_list_test[[m]] %*% beta_final[-1])^2)
      
    }
    
    
    MSPE_vec[sim_iter] <- mean(MSPE)
    
    
    print(sim_iter)
    
  }
  
  
  return(list(BETA = BETA_FINAL_mat, MSPE = MSPE_vec,
              Best_Alpha = ALPHA_vec,
              Best_Lambda = best_lambda_vec))
  
}