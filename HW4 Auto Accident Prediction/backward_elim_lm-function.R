backward_elimination <- function(lmod){
  #performs backward elimination model selection 
  #removes variables until all remaining ones are stat-sig
  removed_vars <- c()
  removed_pvalues <- c()
  
  #handles category variables
  cat_levels <- unlist(lmod$xlevels)
  cat_vars <- str_sub(names(cat_levels), 1, nchar(names(cat_levels)) - 1)
  cat_var_df <- data.frame(cat_vars, 
                           dummy_vars = str_c(cat_vars, cat_levels),
                           stringsAsFactors = F)
  # checks for p-values > .05 execpt for the intercept
  while (max(summary(lmod)$coefficients[2:length(summary(lmod)$coefficients[, 4]), 4]) > .05){  
    # find insignificant pvalue
    pvalues <- summary(lmod)$coefficients[2:length(summary(lmod)$coefficients[, 4]), 4]
    max_pvalue <- max(pvalues)
    remove <- names(which.max(pvalues))
    
    #if categorical dummy variable, remove the variable
    dummy_var <- dplyr::filter(cat_var_df, dummy_vars == remove)
    print(remove <- ifelse(nrow(dummy_var) > 0, dummy_var[, 1], remove))
    
    removed_vars <- c(removed_vars, remove)
    removed_pvalues <- c(removed_pvalues, max_pvalue)   
    # update model
    lmod <- update(lmod, as.formula(paste0(".~.-`", remove, "`"))) 
  }
  
  print("Removed variables:")
  print(kable(data.frame(removed_vars, removed_pvalues), digits = 3))
  return(lmod)
}
