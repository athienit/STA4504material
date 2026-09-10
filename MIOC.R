library(DescTools) # For built-in BreslowDayTest

############################################################
# 1. DATA SETUP
############################################################

MIOC <- array(
  c(4, 62, 2, 224,
    9, 33, 12, 390,
    4, 26, 33, 330,
    6,  9, 65, 362,
    6,  5, 93, 301),
  dim = c(2, 2, 5),
  dimnames = list(
    Status = c("Case", "Control"),
    OCuse  = c("Yes", "No"),
    Agegrp = c("1", "2", "3", "4", "5")
  )
)

# Stratum-specific odds ratios (with 0.5 continuity adjustment)
calc_or <- function(tbl, adjust = TRUE) {
  if (adjust) tbl <- tbl + 0.5
  (tbl[1, 1] * tbl[2, 2]) / (tbl[1, 2] * tbl[2, 1])
}
apply(MIOC, 3, calc_or)


############################################################
# 2. CUSTOM ASSUMPTION CHECKING FUNCTIONS
############################################################

#' Check Asymptotic Conditions for the Cochran-Mantel-Haenszel Test
#' Tests sample size sufficiency under H0: Conditional Independence (OR = 1)
check_cmh <- function(data) {
  K <- dim(data)[3]
  exp_cells <- array(0, dim = dim(data), dimnames = dimnames(data))
  var_sum <- 0
  
  for (k in 1:K) {
    n_k <- sum(data[, , k])
    row_sums <- rowSums(data[, , k])
    col_sums <- colSums(data[, , k])
    
    # Expected cells under independence: E = (row_total * col_total) / stratum_total
    exp_cells[, , k] <- outer(row_sums, col_sums) / n_k
    
    # Hypergeometric variance for cell (1,1,k)
    var_sum <- var_sum + (row_sums[1] * row_sums[2] * col_sums[1] * col_sums[2]) / 
      (n_k^2 * (n_k - 1))
  }
  
  pct_gt_5 <- mean(exp_cells > 5) * 100
  min_cell <- min(exp_cells)
  
  cat("\n================ CMH Assumption Diagnostics ================\n")
  cat("Null Model: Conditional Independence (OR = 1 per stratum)\n")
  cat(sprintf("- Minimum expected cell count: %.2f\n", min_cell))
  cat(sprintf("- Expected cell counts > 5:    %.1f%% (Rule: >= 80%%)\n", pct_gt_5))
  cat(sprintf("- Pooled variance sum:          %.2f (Rule: > 5)\n", var_sum))
  
  if (pct_gt_5 >= 80 && var_sum > 5) {
    cat("[PASS] Asymptotic assumptions for the CMH test are satisfied.\n")
  } else {
    cat("[WARNING] Sparse cells detected. Use exact CMH test if available.\n")
  }
  cat("============================================================\n\n")
  
  invisible(exp_cells)
}


#' Check Asymptotic Conditions for the Breslow-Day Test
#' Tests sample size sufficiency under H0: Homogeneity of ORs (OR = OR_MH)
check_bdt <- function(data, OR = NA) {
  if (is.na(OR)) {
    OR <- as.numeric(mantelhaen.test(data)$estimate)
  }
  
  K <- dim(data)[3]
  exp_cells <- array(0, dim = dim(data), dimnames = dimnames(data))
  
  for (k in 1:K) {
    n1_ <- sum(data[1, , k])
    n2_ <- sum(data[2, , k])
    n_1 <- sum(data[, 1, k])
    n_2 <- sum(data[, 2, k])
    
    # Quadratic coefficients for fitted cell A = E_11k under H0: OR = OR_MH
    a <- 1 - OR
    b <- (n1_ + n_1) * OR + (n2_ - n_1)
    c <- -OR * n1_ * n_1
    
    if (abs(a) < 1e-7) {
      A <- (n1_ * n_1) / sum(data[, , k])
    } else {
      roots <- (-b + c(1, -1) * sqrt(b^2 - 4 * a * c)) / (2 * a)
      A <- roots[roots >= max(0, n1_ - n_2) & roots <= min(n1_, n_1)]
    }
    
    exp_cells[1, 1, k] <- A
    exp_cells[1, 2, k] <- n1_ - A
    exp_cells[2, 1, k] <- n_1 - A
    exp_cells[2, 2, k] <- n2_ - (n_1 - A)
  }
  
  pct_gt_5 <- mean(exp_cells > 5) * 100
  min_cell <- min(exp_cells)
  
  cat("\n============ Breslow-Day Assumption Diagnostics ============\n")
  cat(sprintf("Null Model: Homogeneous Odds Ratio (OR = %.4f)\n", OR))
  cat(sprintf("- Minimum fitted cell count: %.2f (Rule: >= 1)\n", min_cell))
  cat(sprintf("- Fitted cell counts > 5:    %.1f%% (Rule: >= 80%%)\n", pct_gt_5))
  
  if (pct_gt_5 >= 80 && min_cell >= 1) {
    cat("[PASS] Asymptotic assumptions for the Breslow-Day test are satisfied.\n")
  } else {
    cat("[WARNING] Breslow-Day test may be unreliable due to small strata counts.\n")
  }
  cat("============================================================\n\n")
  
  invisible(exp_cells)
}


############################################################
# 3. METHODOLOGICAL WORKFLOW (JUSTIFICATION -> IMPLEMENTATION)
############################################################

# Step 1: Justification (Check assumptions)
# STep 2: Test
check_cmh(MIOC)
mantelhaen.test(MIOC)


check_bdt(MIOC)
BreslowDayTest(MIOC)
