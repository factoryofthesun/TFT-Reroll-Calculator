#' ---
#'Title: TFT Reroll Calculations (10.12)
#'Author: Richard Liu
#'Output: PDF
#'---
rm(list = ls())
library(itertools2)
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)
library(openxlsx)
library(randtests)
library(Matrix)
library(matrixcalc)
library(RColorBrewer)

# ================ SET POOL STATS FOR CURRENT PATCH ==============
ChosenProb <- 0.5 
ChosenCum <- 0.05 # Additional prob of seeing chosen for every shop where you don't see one 
UnitPoolSize <- as.matrix(c(29, 22, 18, 12, 10))
colnames(UnitPoolSize) <- "Unit Copies Per Tier"
rownames(UnitPoolSize) <- 1:5
NumUnits <- as.matrix(c(13, 13, 13, 11, 8))
rownames(NumUnits) <- 1:5
colnames(NumUnits) <- "Unique Units Per Tier"
ExpToLevel <- c(0, 2, 6, 10, 20, 36, 56, 80, 0)
ShopProbMat <- matrix(c(1, 0, 0, 0, 0, 
                        1, 0, 0, 0, 0, 
                        .75, .25, 0, 0, 0, 
                        .55, .3, .15, 0, 0, 
                        .45, .33, .2, .02, 0, 
                        .25, 0.4, .3, .05, 0,
                        .19, .30, .35, .15, .01, 
                        .15, .20, .35, .25, .05, 
                        .1, .15, .3, .3, .15), 
                      nrow=9, ncol=5, byrow=T, dimnames=list(1:9, 1:5))
ChosenProbMat <- matrix(c(1, 0, 0, 0, 0, 
                          1, 0, 0, 0, 0, 
                          1, 0, 0, 0, 0, 
                          .8, .2, 0, 0, 0, 
                          .4, .55, .05, 0, 0, 
                          0, .6, .4, 0, 0, 
                          0, .4, .55, .05, 0,
                          0, 0, 0.6, 0.4, 0, 
                          0, 0, 0, 0.6, 0.4), 
                        nrow=9, ncol=5, byrow=T, dimnames=list(1:9, 1:5))

# ================ DEFINE HELPER FUNCTIONS ===============

# Generate ordered list of keys for all unit permutations given vector of how many looking for 
# Current implemented conditions: "any" and "all"
# To get all absorbing states at the end: start with the base order {(0,0), (0,1), ..., (1,0)} (reversed StarscapeTFT)
# then take all absorbing permutations and throw them to the end with the same general ordering 

getOrderedPermutations <- function(lookingfor, condition="any", chosen=0){
  if(!is.numeric(lookingfor)){
    stop("Expected numeric vector")
  }
  final_lookingfor <- lookingfor
  if (chosen > 0){
    for (i in 1:chosen){
      final_lookingfor[i] <- final_lookingfor[i] + 2
    }
  }
  final_lookingfor <- c(final_lookingfor, chosen) # Chosen causes additional absorbing state conditions to be possible (max(units) + 2)
  rangeslist <- lapply(final_lookingfor, function(x) seq(0,x))
  perms <- do.call(iproduct, rangeslist) # Default ordering is fine 
  perms_df <- do.call(rbind, as.list(perms))
  perms_df <- data.table(perms_df)
  
  # Drop permutations from table where chosen is positive, but respective unit count is < 3 (chosen guarantees at least 3)
  chosen_inds <- setdiff(unique(perms_df$V4), 0)
  for (chosen_ind in chosen_inds){
    perms_df <- perms_df[!(V4 == chosen_ind & get(names(perms_df)[chosen_ind]) < 3)]
  }
  
  # TODO: Should I drop infeasible absorbing conditions? I don't think it actually changes computational complexity bc we drop absorbing states 
  
  # Absorbing vector conditions 
  if(condition == "any"){
    absorb_inds <- apply(perms_df[, 1:(ncol(perms_df)-1)], 1, function(x) any(x >= lookingfor))
  }
  else if(condition == "all"){
    absorb_inds <- apply(perms_df[, 1:(ncol(perms_df)-1)], 1, function(x) all(x >= lookingfor))
  }
  else if(grepl("any", condition, fixed=T)){ # any "n" hit condition
    num <- as.numeric(trimws(sub("any ", "", condition, fixed=T)))
    absorb_inds <- apply(perms_df[, 1:(ncol(perms_df)-1)], 1, function(x) sum(x >= lookingfor) >= num)
  }
  else{ stop("This termination condition has not been implemented yet!") }
  
  # Move absorbing rows to the end 
  perms_df_ordered <- rbind(perms_df[!absorb_inds,,drop=F], perms_df[absorb_inds,,drop=F]) # Drop=F against 1-col edge case
  absorb_cutoff_ind <- nrow(as.data.frame(perms_df[!absorb_inds,,drop=F])) + 1
  
  # Convert to array of character vectors
  perms_char <- as.vector(apply(perms_df_ordered, 1, paste0, collapse=","))
  
  return(list(perms_char, absorb_cutoff_ind))
}

# Define function for computing transition probability 
getStepTransitionProb <- function(base_state, step_state, player_lvl, unit_lvl, num_taken_unit, num_taken_other_unit, 
                                  ShopProbMat, ChosenProbMat, UnitPoolSize, NumUnits, chosen_search = F){
  state_diff <- step_state - base_state
  chosen_step <- F
  
  # If step size 0, return error (0 step is 1-sum(all steps))
  if(sum(state_diff) == 0){
    stop("getStepTransitionProb: Don't make me compute the 0-step it's too hard!")
  }
  
  # If chosen step, then allow step size of 3
  if (chosen_search == T & any(state_diff == 3)){
    unit_ind <- which(state_diff == 3)[1]
    chosen_step <- T
  } else if(sum(state_diff) != 1){ 
    return(0)
  } else{ # Default is step size of 1
    unit_ind <- which(state_diff == 1)
  }
  
  # Compute probability of step
  total_pool_lvl <- UnitPoolSize[unit_lvl] * NumUnits[unit_lvl] - num_taken_unit - num_taken_other_unit - base_state[unit_ind]
  units_left <- UnitPoolSize[unit_lvl]-num_taken_unit-base_state[unit_ind]
  
  # Edge cases: total pool lvl size is 0, units left is negative OR less than 3 (if chosen step)
  # Default return 0 
  if (total_pool_lvl <= 0){
    return(0)
  } else if(units_left < 0){
    return(0)
  } else if (chosen_step == T & units_left < 3){
    return(0)
  }
  
  if (chosen_step == T){
    step_prob <- ChosenProb * ChosenProbMat[player_lvl, unit_lvl] * units_left/total_pool_lvl
  } else if (chosen_search == T){
    step_prob <- (1 - ChosenProb) * ShopProbMat[player_lvl, unit_lvl] * units_left/total_pool_lvl
  } else{
    step_prob <- ShopProbMat[player_lvl, unit_lvl] * units_left/total_pool_lvl
  }
  
  return(step_prob)
}

# Computes the complete number of "other" units taken from a lvl pool, including the other searched-for units 
# of the same level. 
getStatePoolTakenOther <- function(perm_num, unit_lvls, unit_index, num_taken, num_taken_other){
  unit_lvl <- unit_lvls[unit_index] 
  other_inds <- which(unit_lvls == unit_lvl) # Will find at least one
  other_inds <- other_inds[other_inds != unit_index]
  if(length(other_inds) == 0){
    return(num_taken_other[unit_lvl])
  }
  else{
    base_taken <- sum(num_taken[other_inds])
    state_taken <- sum(perm_num[other_inds])
    tot_taken <- base_taken + state_taken + num_taken_other[unit_lvl]
    return(tot_taken)
  }
}

# Generalized function for 1 slot transition matrix 
# Matrix[0,0] will represent the initial state (set by user) 
createOneSlotMatrix <- function(ordered_perms, absorb_cutoff, player_lvl, unit_lvls, num_taken, num_taken_other, initial_state,
                                ShopProbMat, ChosenProbMat, UnitPoolSize, NumUnits, chosen = 0){
  mat_dim <- length(ordered_perms)
  one_slot_transition_mat <- matrix(rep(0, mat_dim^2), nrow=mat_dim,ncol=mat_dim)
  chosen_search <- chosen > 0
  
  # Adjust num_taken by initial_state 
  num_taken <- num_taken + initial_state
  
  # Set matrix row and column names 
  row.names(one_slot_transition_mat) <- ordered_perms
  colnames(one_slot_transition_mat) <- ordered_perms
  
  # Fill out probabilities one section at a time 
  # Absorbed states: identity matrix 
  for(i in absorb_cutoff:nrow(one_slot_transition_mat)){
    one_slot_transition_mat[i,i] <- 1
  }
  
  # Other transitions: Q and R 
  # Loop through string permutations and get list of feasible steps (max 1 step away from any state)
  # Then set probability based on pool size, lvl, and num units out 
  for(perm in ordered_perms[1:absorb_cutoff-1]){
    perm_num <- charPermToNumeric(perm)
    
    # Set of feasible steps is just +1 to any element, within the permutation bounds 
    for(i in 1:(length(perm_num) - 1)){
      stepi <- perm_num
      stepi[i] <- stepi[i] + 1

      # Compute probability of step and assign to matrix
      unit_lvl_i <- unit_lvls[i]
      num_taken_i <- num_taken[i]
      num_taken_other_i <- getStatePoolTakenOther(perm_num, unit_lvls, i, num_taken, num_taken_other)
      stepi_char <- paste0(stepi, collapse=",")
      
      # Check if proposed step is within bounds 
      if (stepi_char %in% colnames(one_slot_transition_mat)){
        one_slot_transition_mat[perm, stepi_char] <- getStepTransitionProb(perm_num, stepi, player_lvl, 
                                                                           unit_lvl_i, num_taken_i, 
                                                                           num_taken_other_i, ShopProbMat, 
                                                                           ChosenProbMat, UnitPoolSize, NumUnits,
                                                                           chosen_search)
      }
      
      # If chosen > 0 and don't have chosen, then need to check chosen steps as well 
      if (chosen > 0 & perm_num[length(perm_num)] == 0){
        for (ind in 1:chosen){
          chosen_stepi <- perm_num 
          chosen_stepi[ind] <- chosen_stepi[ind] + 3
          chosen_stepi[length(chosen_stepi)] <- ind 
          chosen_stepi_char <- paste0(chosen_stepi, collapse=",")
          
          # Check if proposed step is within bounds 
          if (chosen_stepi_char %in% colnames(one_slot_transition_mat)){
            one_slot_transition_mat[perm, chosen_stepi_char] <- getStepTransitionProb(perm_num, chosen_stepi, player_lvl, 
                                                                               unit_lvl_i, num_taken_i, 
                                                                               num_taken_other_i, ShopProbMat, 
                                                                               ChosenProbMat, UnitPoolSize, NumUnits,
                                                                               chosen_search)
          }
        }
      }
    }
    
    # 0 step is just 1 - sum of all other steps
    one_slot_transition_mat[perm, perm] <- 1 - sum(one_slot_transition_mat[perm,])
    
  }
  return(one_slot_transition_mat)
}

# Fundamental Matrix: expected number of visits to state j starting from state i before being absorbed
# Input: Q matrix (sub-matrix of absorbing Markov chain with absorbing rows/cols removed)
# Formula: (I-Q)^-1
# Expected number of steps before being absorbed when starting in state i = sum of row i 
getFundamentalMatrix <- function(Q){
  id_mat <- diag(nrow(Q))
  fund_mat <- solve(id_mat - Q)
  return(fund_mat)
}

# Compute probability of going from one state to the other in N steps 
getNStepProb <- function(oneslotmat, state1_num, state2_num, N){
  Nstepmat <- matrix.power(oneslotmat, N)
  state1_ind <- paste0(state1_num, collapse=",")
  state2_ind <- paste0(state2_num, collapse=",")
  return(Nstepmat[state1_ind, state2_ind])
} 

# PDF: Prob hit in exactly N slots f(k) = Q^(k-1)R (makes sense: odds of getting to step before then hitting)
# CDF: Prob hit within N slots F(k) = 1 - Q^k_1_ (Can also be sum of absorbed states for Q^k)
generateDistributionData <- function(oneslotmat, absorb_cutoff){
  Q <- oneslotmat[1:absorb_cutoff-1, 1:absorb_cutoff-1]
  T0 <- oneslotmat[1:absorb_cutoff-1,absorb_cutoff:ncol(oneslotmat)] # 1-step probs to absorption 
  cdf_probs <- c()
  
  # Generate CDFs up until 99%: PDF(k) = CDF(k) - CDF(k-1)
  i <- 1
  cdf_i <- 0
  generate <- T
  cdf_mat <- oneslotmat
  while(generate){
    if (cdf_i >= 0.99 | i == 500){ # Cap at 500 calcs
      if (i %% 5 == 0){# Enforce multiple of 5 for shops calc
        generate <- F
      }
    }
    cdf_i <- sum(cdf_mat[1,absorb_cutoff:ncol(cdf_mat)])
    cdf_probs <- c(cdf_probs, cdf_i)
    cdf_mat <- cdf_mat %*% oneslotmat
    i <- i+1
  }
  cdf_data <- setDT(data.frame("CDF" = cdf_probs, "Step" = 1:length(cdf_probs)))
  cdf_data$PDF <- cdf_data$CDF - shift(cdf_data$CDF)
  cdf_data$PDF[1] <- cdf_data$CDF[1] # At the first step the CDF and PDF are the same 
  
  # Create indicator for 25-75 percentile 
  cdf_data$Perc_Range <- as.numeric(cdf_data$CDF >= 0.25 & cdf_data$CDF <= 0.75)
  cdf_data[CDF > .75, Perc_Range := 2,]
  return(cdf_data)
}

# Plots PDF by different aggregations: gold, shops, identity
# Imitate StarscapeTFT's pretty graphs and color the 25% to 75% CDF region 
plotPDF <- function(distribution_data, x_by){
  if(x_by == "identity"){ # Plot data as is 
    plt <- ggplot(data=distribution_data, aes(x=Step, y=PDF)) + geom_bar(aes(fill=factor(Perc_Range)), stat="identity", width=1) + 
      labs(x="Shop Slots", y="Probability of Hitting", title="PDF: P(Hit) vs # Shop Slots") +
      scale_x_continuous(breaks=scales::pretty_breaks(n=10)) +
      scale_fill_manual(name="Percentile Ranges", labels=c("<25%", "25%-75%", ">75%"), values=c("#293352", "#3A75A2", "#3DBFFE"), 
                        limits=c(0,1,2)) 
    return(plt)
  }
  else if(x_by == "Gold"){
    grouped_dat <- distribution_data[,.(PDF=sum(PDF), Perc_Range=round(mean(Perc_Range))), Step-0:4] # 5 shops every 2 gold 
    grouped_dat$Gold <- 1:nrow(grouped_dat)
    gold_breaks <- pretty(seq(0,tail(grouped_dat$Gold,1)), n=10)
    gold_labels <- as.character(2*gold_breaks)
    plt <- ggplot(data=grouped_dat, aes(x=Gold, y=PDF)) + geom_bar(aes(fill=factor(Perc_Range)), stat="identity", width=1) + 
      labs(x="Gold", y="Probability of Hitting", title="PDF: P(Hit) vs Gold Spent") + 
      scale_x_continuous(breaks= gold_breaks, labels=gold_labels) + #Hacky way but works for our purposes 
      scale_fill_manual(name="Percentile Ranges", labels=c("<25%", "25%-75%", ">75%"), values=c("#293352", "#3A75A2", "#3DBFFE"),
                        limits=c(0,1,2))
    return(plt)
  }
  else if(x_by == "Shops"){
    grouped_dat <- distribution_data[,.(PDF=sum(PDF), Perc_Range=round(mean(Perc_Range))), Step-0:4] # 5 shops every 2 gold 
    grouped_dat$Shops <- 1:nrow(grouped_dat)
    plt <- ggplot(data=grouped_dat, aes(x=Shops, y=PDF)) + geom_bar(aes(fill=factor(Perc_Range)), stat="identity", width=1) + 
      labs(x="Shops", y="Probability of Hitting", title="PDF: P(Hit) vs # Shops") + 
      scale_x_continuous(breaks=scales::pretty_breaks(n=10)) + 
      scale_fill_manual(name="Percentile Ranges", labels=c("<25%", "25%-75%", ">75%"), values=c("#293352", "#3A75A2", "#3DBFFE"),
                        limits=c(0,1,2))
    return(plt)
  }
  else {
    stop(paste("Have not implement by", x_by, "yet!", sep=" "))
  }
}

plotCDF <- function(distribution_data, x_by){
  if(x_by == "identity"){ # Plot data as is 
    plt <- ggplot(data=distribution_data, aes(x=Step, y=CDF)) + geom_bar(aes(fill=factor(Perc_Range)), stat="identity", width=1) + 
      labs(x="Shop Slots", y="Probability of Hitting", title="CDF: P(Hit) vs # Shop Slots") +
      scale_x_continuous(breaks=scales::pretty_breaks(n=10)) +
      scale_fill_manual(name="Percentile Ranges", labels=c("<25%", "25%-75%", ">75%"), values=c("#293352", "#3A75A2", "#3DBFFE"), 
                        limits=c(0,1,2)) 
    return(plt)
  }
  else if(x_by == "Gold"){
    grouped_dat <- distribution_data[,.(CDF=max(CDF), Perc_Range=round(mean(Perc_Range))), Step-0:4] # 5 shops every 2 gold 
    grouped_dat$Gold <- 1:nrow(grouped_dat)
    gold_breaks <- pretty(seq(0,tail(grouped_dat$Gold,1)), n=10)
    gold_labels <- as.character(2*gold_breaks)
    plt <- ggplot(data=grouped_dat, aes(x=Gold, y=CDF)) + geom_bar(aes(fill=factor(Perc_Range)), stat="identity", width=1) + 
      labs(x="Gold", y="Probability of Hitting", title="CDF: P(Hit) vs Gold Spent") + 
      scale_x_continuous(breaks= gold_breaks, labels=gold_labels) + #Hacky way but works for our purposes 
      scale_fill_manual(name="Percentile Ranges", labels=c("<25%", "25%-75%", ">75%"), values=c("#293352", "#3A75A2", "#3DBFFE"),
                        limits=c(0,1,2))
    return(plt)
  }
  else if(x_by == "Shops"){
    grouped_dat <- distribution_data[,.(CDF=max(CDF), Perc_Range=round(mean(Perc_Range))), Step-0:4] # 5 shops every 2 gold 
    grouped_dat$Shops <- 1:nrow(grouped_dat)
    plt <- ggplot(data=grouped_dat, aes(x=Shops, y=CDF)) + geom_bar(aes(fill=factor(Perc_Range)), stat="identity", width=1) + 
      labs(x="Shops", y="Probability of Hitting", title="CDF: P(Hit) vs # Shops") + 
      scale_x_continuous(breaks=scales::pretty_breaks(n=10)) + 
      scale_fill_manual(name="Percentile Ranges", labels=c("<25%", "25%-75%", ">75%"), values=c("#293352", "#3A75A2", "#3DBFFE"),
                        limits=c(0,1,2))
    return(plt)
  }
  else {
    stop(paste("Have not implement by", x_by, "yet!", sep=" "))
  }
}

# Function to check any possible nonsense with the requested scenario
# Output: list(TRUE/FALSE, error message)
validateScenario <- function(player_lvl, num_taken_other, unit_lvls, num_taken, lookingfor, initial_state,
                             ShopProbMat, ChosenProbMat, UnitPoolSize, NumUnits, chosen){
  # == Validate base data first == 
  # Check if base data completely filled out 
  if (any(is.na(ShopProbMat)) | any(is.na(UnitPoolSize)) | any(is.na(NumUnits)) | any(is.na(ChosenProbMat))){
    return(list(FALSE, "Please finish filling out the pool size and reroll probabilities."))
  }
  
  # Reroll probabilities must be non-negative
  if (any(ShopProbMat < 0) | any(ChosenProbMat < 0)){
    return(list(FALSE, "Reroll probabilities must be non-negative."))
  }
  
  # Reroll probabilities must sum to 1 per level 
  if (!(all(rowSums(ShopProbMat) == 1))){
    faulty_reroll_rows <- which(rowSums(ShopProbMat) != 1)
    faulty_reroll_char <- paste0(faulty_reroll_rows, collapse=", ")
    return(list(FALSE, paste("Reroll probabilities must sum to 1. Please adjust for level(s):", faulty_reroll_char)))
  }
  
  # Chosen reroll probabilities must sum to 1 per level 
  if (!(all(rowSums(ChosenProbMat) == 1))){
    faulty_reroll_rows <- which(rowSums(ChosenProbMat) != 1)
    faulty_reroll_char <- paste0(faulty_reroll_rows, collapse=", ")
    return(list(FALSE, paste("Chosen probabilities must sum to 1. Please adjust for level(s):", faulty_reroll_char)))
  }
  
  # Unit pool size must be positive 
  if (any(UnitPoolSize <= 0)){
    return(list(FALSE, "Unit pool sizes must all be positive."))
  }
  
  # Num units must be positive 
  if (any(NumUnits <= 0)){
    return(list(FALSE, "Number of units must all be positive."))
  }
  
  # Check if unit pool size large enough
  unit_availability_check <- sapply(1:length(unit_lvls), function(x) 
        (num_taken[x] + lookingfor[x] + initial_state[x]) <= UnitPoolSize[unit_lvls[x]])
  if (!all(unit_availability_check)){
    faulty_unit_ind <- which(!unit_availability_check)
    unit_ind_char <- paste0(faulty_unit_ind, collapse = ", ")
    return(list(FALSE, paste("Can't hit copies of units that don't exist. Please adjust the 'Copies Others Own' and", 
                             "'Total Copies Wanted' fields for unit(s):", 
                             unit_ind_char)))
  }
  
  # Check if tier pool size large enough 
  tier_taken <- num_taken_other
  for (i in 1:length(unit_lvls)){
    unit_lvl <- unit_lvls[i]
    tier_taken[unit_lvl] <- tier_taken[unit_lvl] + num_taken[i] + lookingfor[i] + initial_state[i]
  }
  tier_pool_check <- sapply(1:length(tier_taken), function(x) tier_taken[x] <= UnitPoolSize[x]*NumUnits[x])
  if (!all(tier_pool_check)){
    faulty_tier <- which(!tier_pool_check)
    tier_char <- paste0(faulty_tier, collapse = ", ")
    return(list(FALSE, paste("Your scenario involves more units than exist in the tier pools. Please adjust the scenario for tier(s):", 
                             tier_char)))
  }
  
  # All unique units being looked for are excluded from the num_taken_other count 
  other_taken <- num_taken_other 
  for (i in 1:length(unit_lvls)){
    unit_lvl <- unit_lvls[i]
    other_taken[unit_lvl] <- other_taken[unit_lvl] + UnitPoolSize[unit_lvl]
  }
  other_taken_check <- sapply(1:length(other_taken), function(x) other_taken[x] <= UnitPoolSize[x]*NumUnits[x])
  if (!all(other_taken_check)){
    faulty_tier <- which(!other_taken_check)
    tier_char <- paste0(faulty_tier, collapse = ", ")
    return(list(FALSE, paste("You currently have too many units taken out of the pool,",
                             "given the number of units you are looking for, for tier(s):", 
                             tier_char)))
  }

  # Check if player level adequate for unit level
  available_unit_lvls <- which(ShopProbMat[player_lvl,] > 0)
  player_lvl_check <- sapply(unit_lvls, function(x) x %in% available_unit_lvls)
  if (!all(player_lvl_check)){
    fauly_unit_ind <- which(!player_lvl_check)
    faulty_unit <- paste0(fauly_unit_ind, collapse = ", ")
    return(list(FALSE, paste("Player level too low to hit unit(s):", faulty_unit)))
  }
  
  # Check if total looking for is non-positive 
  positive_check <- sum(lookingfor) > 0
  if (!positive_check){
    return(list(FALSE, paste("Please select a positive number of copies to hit.")))
  }
  
  # Chosen value can't be larger than max unit index 
  if (chosen > length(unit_lvls)){
    return(list(FALSE, "Chosen toggle error."))
  }
  
  return(list(TRUE, "passed validation!"))

}

charPermToNumeric <- function(perm){
  return(as.numeric(unlist(strsplit(perm, ","))))
}

getExpToLevel <- function(lvl, ExpToLevel){
  return(ExpToLevel[lvl])
}

# Use fundamental matrix to get the expected number of shops before hitting 
getExpectedShopsToHit <- function(oneslotmat, absorb_cutoff){
  q <- oneslotmat[1:absorb_cutoff-1, 1:absorb_cutoff-1]
  # Check if q is scalar
  if (length(q) == 1){
    expected_slots <- 1/(1-q)
    expected_shops <- round(expected_slots/5)
  }
  else{
    fundamental_mat <- getFundamentalMatrix(q)
    expected_slots <- sum(fundamental_mat[1,]) + 1 
    expected_shops <- round(expected_slots/5)
  }
  
  return(expected_shops)
}

# ================== Chosen Probability Functions =========================
# Single shop probability of hitting any chosen 
getChosenStepProbability <- function(initial_state, player_lvl, unit_lvls, num_taken, num_taken_other,
                                     ChosenProbMat, UnitPoolSize, NumUnits, chosen_prob){
  p_hit <- 0
  for (i in 1:length(unit_lvls)){
    player_owned <- initial_state[i]
    unit_lvl <- unit_lvls[i]
    unit_prob <- ChosenProbMat[player_lvl, unit_lvl]
    unit_pool_size <- UnitPoolSize[unit_lvl]
    num_units <- NumUnits[unit_lvl]
    unit_num_taken <- num_taken[i] + initial_state[i]
    unit_num_taken_other <- getStatePoolTakenOther(initial_state, unit_lvls, i, num_taken, num_taken_other)
    
    # Compute probability of step
    total_pool_lvl <- unit_pool_size * num_units - unit_num_taken - unit_num_taken_other
    units_left <- unit_pool_size - unit_num_taken
    
    # Edge cases: total pool lvl size is 0, units left is negative OR less than 3 (if chosen step)
    # Default return 0 
    if (total_pool_lvl < 3 | units_left < 3){
      next
    }
    
    prob <- chosen_prob * unit_prob * units_left/total_pool_lvl
    p_hit <- p_hit + prob 
  }

  return(p_hit)
}
  
# Expected shops to hit any chosen of choice 
getChosenExpectedShopsToHit <- function(initial_state, player_lvl, unit_lvls, num_taken, num_taken_other,
                                  ChosenProbMat, UnitPoolSize, NumUnits, chosen_prob){
  p_hit <- getChosenStepProbability(initial_state, player_lvl, unit_lvls, num_taken, num_taken_other,
                                    ChosenProbMat, UnitPoolSize, NumUnits, chosen_prob)
  expected_shops <- round(1/p_hit)
  
  return(expected_shops)
}

# Distribution data for chosen probabilities (ignore cumulative factor for now: it's HARD)
generateChosenDistributionData <- function(initial_state, player_lvl, unit_lvls, num_taken, num_taken_other,
                                           ChosenProbMat, UnitPoolSize, NumUnits, chosen_prob){
  cdf_probs <- c()
  pdf_probs <- c()
  
  # Generate CDFs up until 99%: PDF(k) = CDF(k) - CDF(k-1)
  i <- 1
  cdf_i <- 0
  generate <- T
  p_hit <- getChosenStepProbability(initial_state, player_lvl, unit_lvls, num_taken, num_taken_other,
                                    ChosenProbMat, UnitPoolSize, NumUnits, chosen_prob)
  while(generate){
    if (cdf_i >= 0.99 | i == 100){ # Cap at 100 shops
      generate <- F
    }
    pdf <- p_hit * (1-p_hit)^(i-1)
    cdf_i <- cdf_i + pdf
    cdf_probs <- c(cdf_probs, rep(cdf_i, 5))
    pdf_probs <- c(pdf_probs, rep(pdf, 5))
    i <- i+1
  }
  cdf_data <- setDT(data.frame("CDF" = cdf_probs, "PDF" = pdf_probs, "Step" = 1:length(cdf_probs)))

  # Create indicator for 25-75 percentile 
  cdf_data$Perc_Range <- as.numeric(cdf_data$CDF >= 0.25 & cdf_data$CDF <= 0.75)
  cdf_data[CDF > .75, Perc_Range := 2,]
  return(cdf_data)
}

# Function to check any possible nonsense with the requested scenario
# Output: list(TRUE/FALSE, error message)
validateChosenScenario <- function(player_lvl, num_taken_other, unit_lvls, num_taken, initial_state, 
                                   ChosenProbMat, UnitPoolSize, NumUnits, chosen){
  # == Validate base data first == 
  # Check if base data completely filled out 
  if (any(is.na(UnitPoolSize)) | any(is.na(NumUnits)) | any(is.na(ChosenProbMat))){
    return(list(FALSE, "Please finish filling out the pool size and reroll probabilities."))
  }
  
  # Reroll probabilities must be non-negative
  if (any(ChosenProbMat < 0)){
    return(list(FALSE, "Reroll probabilities must be non-negative."))
  }
  
  # Chosen reroll probabilities must sum to 1 per level 
  if (!(all(rowSums(ChosenProbMat) == 1))){
    faulty_reroll_rows <- which(rowSums(ChosenProbMat) != 1)
    faulty_reroll_char <- paste0(faulty_reroll_rows, collapse=", ")
    return(list(FALSE, paste("Chosen probabilities must sum to 1. Please adjust for level(s):", faulty_reroll_char)))
  }
  
  # Unit pool size must be positive 
  if (any(UnitPoolSize <= 0)){
    return(list(FALSE, "Unit pool sizes must all be positive."))
  }
  
  # Num units must be positive 
  if (any(NumUnits <= 0)){
    return(list(FALSE, "Number of units must all be positive."))
  }
  
  # Check if unit pool size large enough
  unit_availability_check <- sapply(1:length(unit_lvls), function(x) 
    (num_taken[x] + 3 + initial_state[x]) <= UnitPoolSize[unit_lvls[x]])
  if (!all(unit_availability_check)){
    faulty_unit_ind <- which(!unit_availability_check)
    unit_ind_char <- paste0(faulty_unit_ind, collapse = ", ")
    return(list(FALSE, paste("Chosen will not appear for unit with < 3 copies left in pool.", 
                             "Please adjust for unit(s):", 
                             unit_ind_char)))
  }
  
  # Check if tier pool size large enough 
  tier_taken <- num_taken_other
  for (i in 1:length(unit_lvls)){
    unit_lvl <- unit_lvls[i]
    tier_taken[unit_lvl] <- tier_taken[unit_lvl] + num_taken[i] + 3 + initial_state[i]
  }
  tier_pool_check <- sapply(1:length(tier_taken), function(x) tier_taken[x] <= UnitPoolSize[x]*NumUnits[x])
  if (!all(tier_pool_check)){
    faulty_tier <- which(!tier_pool_check)
    tier_char <- paste0(faulty_tier, collapse = ", ")
    return(list(FALSE, paste("Your scenario involves more units than exist in the tier pools. Please adjust the scenario for tier(s):", 
                             tier_char)))
  }
  
  # All unique units being looked for are excluded from the num_taken_other count 
  other_taken <- num_taken_other 
  for (i in 1:length(unit_lvls)){
    unit_lvl <- unit_lvls[i]
    other_taken[unit_lvl] <- other_taken[unit_lvl] + UnitPoolSize[unit_lvl]
  }
  other_taken_check <- sapply(1:length(other_taken), function(x) other_taken[x] <= UnitPoolSize[x]*NumUnits[x])
  if (!all(other_taken_check)){
    faulty_tier <- which(!other_taken_check)
    tier_char <- paste0(faulty_tier, collapse = ", ")
    return(list(FALSE, paste("You currently have too many units taken out of the pool,",
                             "given the number of units you are looking for, for tier(s):", 
                             tier_char)))
  }
  
  # Check if player level adequate for unit level
  available_unit_lvls <- which(ShopProbMat[player_lvl,] > 0)
  player_lvl_check <- sapply(unit_lvls, function(x) x %in% available_unit_lvls)
  if (!all(player_lvl_check)){
    fauly_unit_ind <- which(!player_lvl_check)
    faulty_unit <- paste0(fauly_unit_ind, collapse = ", ")
    return(list(FALSE, paste("Player level too low to hit unit(s):", faulty_unit)))
  }
  
  # Chosen value can't be larger than max unit index 
  if (chosen > length(unit_lvls)){
    return(list(FALSE, "Chosen toggle error."))
  }
  
  return(list(TRUE, "passed validation!"))
  
}

# ======================== TESTING =======================
# player_lvl <- 4
# num_taken_other <- c(0,0,0,0,0) # this is ordered by level
# unit_lvls <- c(1,1,1)
# num_taken <- c(6,6,6)
# lookingfor <- c(1,1,1) # THIS IS NOW LOOKING FOR ON TOP OF INITIAL STATE INSTEAD OF TOTAL
# condition <- "all"
# initial_state <- c(3,3,3)
# chosen <- 2
# 
# test_validate <- validateScenario(player_lvl, num_taken_other, unit_lvls, num_taken, lookingfor, initial_state,
#                                   ShopProbMat, ChosenProbMat, UnitPoolSize, NumUnits, chosen)
# print(test_validate[[2]])
# 
# ordered_ret <- getOrderedPermutations(lookingfor, condition, chosen)
# ordered_perms <- ordered_ret[[1]]
# absorb_cutoff <- ordered_ret[[2]]
# one_slot_transition_mat <- createOneSlotMatrix(ordered_perms, absorb_cutoff, player_lvl, unit_lvls, num_taken, num_taken_other, initial_state,
#                                                ShopProbMat, ChosenProbMat, UnitPoolSize, NumUnits, chosen)
# 
# # Q <- one_slot_transition_mat[1:absorb_cutoff-1, 1:absorb_cutoff-1]
# n_shops <- getExpectedShopsToHit(one_slot_transition_mat, absorb_cutoff)
# 
# # Test pdf plots
# dist_data <- generateDistributionData(one_slot_transition_mat, absorb_cutoff)
# plotCDF(dist_data, x_by="Shops")
# plotPDF(dist_data, x_by="Shops")
# 
# # Test chosen hit conditions -------------------------
# test_validate <- validateChosenScenario(player_lvl, num_taken_other, unit_lvls, num_taken, initial_state,
#                                         ChosenProbMat, UnitPoolSize, NumUnits, chosen)
# print(test_validate[[2]])
# expected_shops <- getChosenExpectedShopsToHit(initial_state, player_lvl, unit_lvls, num_taken, num_taken_other,
#                                               ChosenProbMat, UnitPoolSize, NumUnits, ChosenProb)
# 
# # Test pdf plots
# dist_data <- generateChosenDistributionData(initial_state, player_lvl, unit_lvls, num_taken, num_taken_other,
#                                             ChosenProbMat, UnitPoolSize, NumUnits, ChosenProb)
# plotCDF(dist_data, x_by="Shops")
# plotPDF(dist_data, x_by="Shops")
