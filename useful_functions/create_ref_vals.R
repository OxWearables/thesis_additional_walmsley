get_diff <- function(comp_name){
  if (comp_name == "MVPA") {
    diff <- 1 / (24 * 3) # If MVPA, difference is 20 mins
  } else {
    diff <- 1 / 24 # Else, difference is 1 hour
  }
  return(diff)
}

create_ref_vals <- function(comp_name, cm){
  
  # CHECKS -------------------------------------------------------------
  if (!isTRUE(all.equal(sum(cm), 1))){
    stop("Wrong scaling") # Check scaling of compositional mean
  }
  
  
  # DIFFERENCE ON ORIGINAL SCALE DEPENDS ON VARIABLE CONSIDERED---------
  diff <- get_diff(comp_name)
  
  # EXTRACT PIVOT AND OTHER VARIABLES-----------------------------------
  pivvar <- cm[, comp_name]
  othvarname <- colnames(cm)[colnames(cm) != comp_name]
  othvar <- data.frame(cm[, othvarname])
  
 
  # CALCULATE NEW PIVOT VARIABLE----------------------------------------
  pivvarnew <- pivvar + diff
  
  # CALCULATE NEW OTHER VARIABLES BASED ON PROPORTIONAL REALLOCATION----
  othvarnew <- data.frame(matrix(ncol = 0, nrow = 1))
  for (var in othvarname) {
    othvarnew[, var] <-
      othvar[, var] - (diff * othvar[, var] / sum(othvar))
  }
  
  # CALCULATE WHAT THIS MEANS FOR FIRST ILR COORDINATE-------------------
  # note other ilr coordinates in the pivot set should be unchanged, as ratios between othvars are identical
  ilr_old <- sqrt(3 / 4) * log(pivvar / ((othvar[, 1] * othvar[, 2] * othvar[, 3]) ^
                      (1 / 3)))
  ilr_new <- sqrt(3 / 4) * log(pivvarnew / ((othvarnew[, 1] * othvarnew[, 2] * othvarnew[, 3]) ^
                      (1 / 3)))
  # ADDITIONAL CHECKS----------------------------------------------------
  for (i in 1:2){
    for (j in ((i+1):3)){
      if (!isTRUE(all.equal(othvar[,i]/othvar[,j], othvarnew[,i]/othvarnew[,j]))){
        stop("Ratios between the non-pivot variables are not the same")
   }
    }
  }
  
  # RETURN DIFFERENCE IN FIRST ILR COORDINATE----------------------------
  return(ilr_new - ilr_old)
  
  
}
