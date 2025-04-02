## BEI scoring 
##   scoring_cols <- c("epsa_vegetables",
# "epsa_vegetables_leafy",
# "epsa_vegetables_cruciferous",
# "epsa_fruit",
# "epsa_fruit_berries",
# "epsa_nuts",
# "epsa_nuts_walnuts",
# "epsa_seafood",
# "epsa_seafood_fatty_fish",
# "epsa_beans",
# "epsa_meat",
# "epsa_meat_processed",
# "epsa_high_fat_dairy",
# "epsa_white_bread",
# "epsa_food_past_month_servings") 

BEI_score <- function(row){
  BEI <- 0

  
  if (row["epsa_vegetables"] %in% c("2 per day", "1 per day", "< 1 per day")){
    BEI <- 0
  } else if (row["epsa_vegetables"] %in% c("3 per day", "4 per day")){
    BEI <- 0.5
  } else {#(scoring_cols$epsa_vegetables %in% c("5 per day", "6 or more per day")){
    BEI <- 1
  }
    
  if (row["epsa_vegetables_leafy"] %in% c("2-3 per week","< 1 per week", "1 per week")){
    BEI <- BEI + 0
  } else if (row["epsa_vegetables_leafy"] %in% c("4-6 per week")){
    BEI <- BEI + 0.5
  } else {#(scoring_cols$epsa_vegetables_leafy %in% c("2 or more per day","1 per day")){
    BEI <- BEI + 1
  }
  
  if (row["epsa_vegetables_cruciferous"] %in% c("< 1 per week", "1 per week")){
    BEI <- BEI + 0
  } else if (row["epsa_vegetables_cruciferous"] %in% c("2 per week")){
    BEI <- BEI + 0.5
  } else {#(scoring_cols$epsa_vegetables_leafy %in% c("3-4 per week","1 or more per day","5-6 per week")){
    BEI <- BEI + 1
  }
  
  if (row["epsa_fruit"] %in% c("< 1 per day", "1 per day")){
    BEI <- BEI + 0
  } else if (row["epsa_fruit"] %in% c("2 per day","3 per day")){
    BEI <- BEI + 0.5
  } else {
    BEI <- BEI + 1
  }
  
  if (row["epsa_fruit_berries"] %in% c("< 1 per week", "1 per week",NA)){
    BEI <- BEI + 0
  } else if (row["epsa_fruit_berries"] %in% c("2 per week")){
    BEI <- BEI + 0.5
  } else {
    BEI <- BEI + 1
  }
  
  if (row["epsa_nuts"] %in% c("< 1 per week", "1 per week","2-3 per week", NA)){
    BEI <- BEI + 0
  } else if (row["epsa_nuts"] %in% c("4-6 per week")){
    BEI <- BEI + 0.5
  } else {
    BEI <- BEI + 1
  }
  
  if (row["epsa_nuts_walnuts"] %in% c("< 1 per week", "1 per week", NA)){
    BEI <- BEI + 0
  } else if (row["epsa_nuts_walnuts"] %in% c("2-3 per week")){
    BEI <- BEI + 0.5
  } else {
    BEI <- BEI + 1
  }
  
  if (row["epsa_seafood"] %in% c("< 1 per week", "1 per week", NA)){
    BEI <- BEI + 0
  } else if (row["epsa_seafood"] %in% c("2 per week")){
    BEI <- BEI + 0.5
  } else {
    BEI <- BEI + 1
  }

  if (row["epsa_beans"] %in% c("< 1 per week")){
    BEI <- BEI + 0
  } else if (row["epsa_beans"] %in% c("1 per week")){
    BEI <- BEI + 0.5
  } else {
    BEI <- BEI + 1
  }
  
  if (row["epsa_meat"] %in% c("2 per day","3 or more per day")){
    BEI <- BEI + 0
  } else {
    BEI <- BEI + 1
  }
  
  if (row["epsa_meat_processed"] %in% c("< 1 per week", NA)){
    BEI <- BEI + 1
  } else if (row["epsa_meat_processed"] %in% c("1 per week")){
    BEI <- BEI + 0.5
  } else {
    BEI <- BEI + 0
  }
  
  if (row["epsa_high_fat_dairy"] %in% c("< 1 per week", NA)){
    BEI <- BEI + 1
  } else if (row["epsa_high_fat_dairy"] %in% c("1 per week")){
    BEI <- BEI + 0.5
  } else {
    BEI <- BEI + 0
  }
  
  if (row["epsa_white_bread"] %in% c("< 1 per week", "1 per week")){
    BEI <- BEI + 1
  } else {
    BEI <- BEI + 0
  }
  
  if (row["epsa_food_past_month_servings"] %in% c("3 per week", "< 1 per week",
                                                        "1-2 per week", NA)){
    BEI <- BEI + 1
  } else if (row["epsa_food_past_month_servings"] %in% c("4 per week")){
    BEI <- BEI + 0.5
  } else {
    BEI <- BEI + 0
  }
  
  return(BEI)
}
