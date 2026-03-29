# EA_Boostinging ------------------------------------------------------------------

load(file.path("output", "EA_Boosting_50.RData"))

load(file.path("output", "EA_Boosting_100.RData"))


n_sim <- EA_Boosting_50$fits |> length()

p_vector <- c(50,100)

P_informative <- 5


## p = 50 ----------------------------------------------------------------------

Included_Covariates <- matrix(0, nrow = n_sim, ncol = p_vector[1])


for(i in seq_len(n_sim)){

  Included_Covariates[i,which(EA_Boosting_50$fits[[i]]$BETA != 0)] <- 1


}


EA_Boosting_50_list <- list(MSPE = mean(EA_Boosting_50$test_loss),

                             Tuning  = mean(EA_Boosting_50$best_mstop),

                             TPP = mean(apply(Included_Covariates, 2,
                                         function(x)sum(x)/length(x))[1:5]),

                             TNP = 1 - mean(apply(Included_Covariates,
                                             2, function(x)sum(x)/length(x))[-(1:5)]),

                             Num_Selected = mean(apply(Included_Covariates, 1, sum)),

                             Var_Selected  = apply(Included_Covariates, 1, sum))


## p = 100 ---------------------------------------------------------------------

Included_Covariates <- matrix(0, nrow = n_sim, ncol = p_vector[2])


for(i in seq_len(n_sim)){

  Included_Covariates[i,which(EA_Boosting_100$fits[[i]]$BETA != 0)] <- 1


}


EA_Boosting_100_list <- list(MSPE = mean(EA_Boosting_100$test_loss),

                              Tuning  = mean(EA_Boosting_100$best_mstop),

                              TPP = mean(apply(Included_Covariates, 2,
                                               function(x)sum(x)/length(x))[1:5]),

                              TNP = 1 - mean(apply(Included_Covariates,
                                                   2, function(x)sum(x)/length(x))[-(1:5)]),

                              Num_Selected = mean(apply(Included_Covariates, 1, sum)),

                              Var_Selected  = apply(Included_Covariates, 1, sum))




# MIBoost ----------------------------------------------------------------

load(file.path("output", "MIBoost_50.RData"))

load(file.path("output", "MIBoost_100.RData"))


## p = 50 ----------------------------------------------------------------------

Included_Covariates <- matrix(0, nrow = n_sim, ncol = p_vector[1])


for(i in seq_len(n_sim)){

Included_Covariates[i,which(MIBoost_50$fits[[i]]$BETA != 0)] <- 1


}



MIBoost_50_list <- list(MSPE = mean(MIBoost_50$test_loss),

                   Tuning  = mean(MIBoost_50$best_mstop),

                   TPP = mean(apply(Included_Covariates, 2,
                                    function(x)sum(x)/length(x))[1:5]),

                   TNP = 1 - mean(apply(Included_Covariates,
                                        2, function(x)sum(x)/length(x))[-(1:5)]),

                   Num_Selected = mean(apply(Included_Covariates, 1, sum)),

                   Var_Selected  = apply(Included_Covariates, 1, sum))


## p = 100 ---------------------------------------------------------------------

Included_Covariates <- matrix(0, nrow = n_sim, ncol = p_vector[2])


for(i in seq_len(n_sim)){

  Included_Covariates[i,which(MIBoost_100$fits[[i]]$BETA != 0)] <- 1


}

MIBoost_100_list <- list(MSPE = mean(MIBoost_100$test_loss),

                             Tuning  = mean(MIBoost_100$best_mstop),

                             TPP = mean(apply(Included_Covariates, 2,
                                              function(x)sum(x)/length(x))[1:5]),

                             TNP = 1 - mean(apply(Included_Covariates,
                                                  2, function(x)sum(x)/length(x))[-(1:5)]),

                             Num_Selected = mean(apply(Included_Covariates, 1, sum)),

                             Var_Selected  = apply(Included_Covariates, 1, sum))


# Salasso -----------------------------------------------------------------

load(file.path("output", "salasso_results_50.RData"))

load(file.path("output", "salasso_results_100.RData"))

## p = 50 ----------------------------------------------------------------------

P_informative <- 5


Salasso_50_list <- list(MSPE = mean(salasso_results_50$MSPE),

                              Tuning  = mean(salasso_results_50$Best_Lambda),

                              TPP = sum(salasso_results_50$BETA[,2:6] != 0)/
                                (n_sim * P_informative),

                              TNP = sum(salasso_results_50$BETA[,-(1:6)] == 0)/
                                (n_sim * (p_vector[1] - P_informative)),


                              Num_Selected = sum(salasso_results_50$BETA[,-1] != 0)/
                                n_sim,

                        Var_Selected  = mean(apply(salasso_results_50$BETA[,-1], 
                                                   1, function(x)sum(x!=0))))

## p = 100 ---------------------------------------------------------------------


Salasso_100_list <- list(MSPE = mean(salasso_results_100$MSPE),

                             Tuning  = mean(salasso_results_100$Best_Lambda),

                             TPP = sum(salasso_results_100$BETA[,2:6] != 0)/
                               (n_sim * P_informative),

                             TNP = sum(salasso_results_100$BETA[,-(1:6)] == 0)/
                               (n_sim * (p_vector[2] - P_informative)),


                             Num_Selected = sum(salasso_results_100$BETA[,-1] != 0)/
                               n_sim,

                         Var_Selected  = mean(apply(salasso_results_100$BETA[,-1], 
                                                    1, function(x)sum(x!=0))))


# Saenet -----------------------------------------------------------------

load(file.path("output", "saenet_results_50.RData"))

load(file.path("output", "saenet_results_100.RData"))

## p = 50 ----------------------------------------------------------------------

Saenet_50_list <- list(MSPE = mean(saenet_results_50$MSPE),

                             Tuning  = c(mean(saenet_results_50$Best_Lambda),
                                         mean(saenet_results_50$Best_Alpha)),

                             TPP = sum(saenet_results_50$BETA[,2:6] != 0)/
                               (n_sim * P_informative),

                             TNP = sum(saenet_results_50$BETA[,-(1:6)] == 0)/
                               (n_sim * (p_vector[1] - P_informative)),


                             Num_Selected = sum(saenet_results_50$BETA[,-1] != 0)/
                               n_sim,

                             Var_Selected  = mean(apply(saenet_results_50$BETA[,-1], 
                                                   1, function(x)sum(x!=0))))


## p = 100 ---------------------------------------------------------------------

Saenet_100_list <- list(MSPE = mean(saenet_results_100$MSPE),

                        Tuning  = c(mean(saenet_results_100$Best_Lambda),
                                    mean(saenet_results_100$Best_Alpha)),

                              TPP = sum(saenet_results_100$BETA[,2:6] != 0)/
                                (n_sim * P_informative),

                              TNP = sum(saenet_results_100$BETA[,-(1:6)] == 0)/
                                (n_sim * (p_vector[2] - P_informative)),


                              Num_Selected = sum(saenet_results_100$BETA[,-1] != 0)/
                                n_sim,

                        Var_Selected  = mean(apply(saenet_results_100$BETA[,-1], 
                                                   1, function(x)sum(x!=0))))

# Result Table ------------------------------------------------------------


result_table <- 

rbind(
c(sprintf("%.2f", EA_Boosting_50_list$MSPE),
  "",
  sprintf("%.1f", EA_Boosting_50_list$Tuning),
  sprintf("%.2f", EA_Boosting_50_list$TPP),
  sprintf("%.2f", EA_Boosting_50_list$TNP),
  sprintf("%.1f", EA_Boosting_50_list$Num_Selected)),


c(sprintf("%.2f", MIBoost_50_list$MSPE),
  "",
  sprintf("%.1f", MIBoost_50_list$Tuning),
  sprintf("%.2f", MIBoost_50_list$TPP),
  sprintf("%.2f", MIBoost_50_list$TNP),
  sprintf("%.1f", MIBoost_50_list$Num_Selected)),


c(sprintf("%.2f", Salasso_50_list$MSPE),
  formatC(Salasso_50_list$Tuning, format = "e", digits = 1),
  "",
  sprintf("%.2f", Salasso_50_list$TPP),
  sprintf("%.2f", Salasso_50_list$TNP),
  sprintf("%.1f", Salasso_50_list$Num_Selected)),


c(sprintf("%.2f", Saenet_50_list$MSPE),
  paste(formatC(Saenet_50_list$Tuning, format = "e", digits = 1),
        collapse = "/"),
  "",
  sprintf("%.2f", Saenet_50_list$TPP),
  sprintf("%.2f", Saenet_50_list$TNP),
  sprintf("%.1f", Saenet_50_list$Num_Selected)),


rep("", 6),


  c(sprintf("%.2f", EA_Boosting_100_list$MSPE),
    "",
    sprintf("%.1f", EA_Boosting_100_list$Tuning),
    sprintf("%.2f", EA_Boosting_100_list$TPP),
    sprintf("%.2f", EA_Boosting_100_list$TNP),
    sprintf("%.1f", EA_Boosting_100_list$Num_Selected)),

  c(sprintf("%.2f", MIBoost_100_list$MSPE),
    "",
    sprintf("%.1f", MIBoost_100_list$Tuning),
    sprintf("%.2f", MIBoost_100_list$TPP),
    sprintf("%.2f", MIBoost_100_list$TNP),
    sprintf("%.1f", MIBoost_100_list$Num_Selected)),


  c(sprintf("%.2f", Salasso_100_list$MSPE),
    formatC(Salasso_100_list$Tuning, format = "e", digits = 1),
    "",
    sprintf("%.2f", Salasso_100_list$TPP),
    sprintf("%.2f", Salasso_100_list$TNP),
    sprintf("%.1f", Salasso_100_list$Num_Selected)),


  c(sprintf("%.2f", Saenet_100_list$MSPE),
    paste(formatC(Saenet_100_list$Tuning, format = "e", digits = 1),
          collapse = "/"),
    "",
    sprintf("%.2f", Saenet_100_list$TPP),
    sprintf("%.2f", Saenet_100_list$TNP),
    sprintf("%.1f", Saenet_100_list$Num_Selected))

)





methods <- c("EA-Boosting", "MIBoost", "SaLASSO", "SaENET")

make_latex_row <- function(row_index, method, p_value = NULL) {
  p_col <- if (is.null(p_value)) {
    "                    "
  } else {
    paste0("\\multirow{4}{*}{", p_value, "}")
  }
  
  paste0(
    p_col, " & ",
    "\\texttt{", method, "} & ",
    result_table[row_index, 1], " & ",
    result_table[row_index, 2], " & ",
    result_table[row_index, 3], " & ",
    result_table[row_index, 4], " & ",
    result_table[row_index, 5], " & ",
    result_table[row_index, 6], " \\\\"
  )
}

latex_lines <- c(
  "\\begin{table}[!ht]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{\\textbf{Simulation results.}}",
  "\\label{tab:results}",
  "\\begin{tabular}{c|l *{6}{c}}",
  "\\toprule",
  "$p$ & Method & \\textbf{MSPE} & $\\lambda^*/\\alpha^*$ & $t_{\\mathrm{stop}}^*$ & \\textbf{TPP} & \\textbf{TNP} & \\textbf{\\# Selec.} \\\\",
  "\\midrule",
  
  make_latex_row(1, methods[1], 50),
  make_latex_row(2, methods[2]),
  make_latex_row(3, methods[3]),
  make_latex_row(4, methods[4]),
  
  "\\cmidrule(lr){1-8}",
  
  make_latex_row(6, methods[1], 100),
  make_latex_row(7, methods[2]),
  make_latex_row(8, methods[3]),
  make_latex_row(9, methods[4]),
  
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\small",
  "\\item \\textit{Legend:} MSPE = Mean Squared Prediction Error; TPP = True Positive Proportion; TNP = True Negative Proportion; \\# Selec. = Number of selected variables; EA = Estimate Averaging.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

latex_table <- paste(latex_lines, collapse = "\n")

cat(latex_table)
writeLines(latex_table, "output/results_table.tex")


