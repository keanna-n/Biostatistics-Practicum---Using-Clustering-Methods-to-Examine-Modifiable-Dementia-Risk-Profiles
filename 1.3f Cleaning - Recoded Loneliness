## need to recode the values for loneliness so we can sum them 

recoded_loneliness <- function(dat){
  recoding_cols <- c("social_engagement_01",
                     "social_engagement_02",
                     "social_engagement_03",
                     "social_engagement_04",
                     "social_engagement_05",
                     "social_engagement_06",
                     "social_engagement_07",
                     "social_engagement_08",
                     "social_engagement_11",
                     "social_engagement_12",
                     "social_engagement_13",
                     "social_engagement_14",
                     "social_engagement_15",
                     "social_engagement_16",
                     "social_engagement_17",
                     "social_engagement_18",
                     "social_engagement_19",
                     "social_engagement_20",
                     "social_engagement_21",
                     "social_engagement_22")
  
  recoded_data <- dat %>%
    mutate(across(
      .cols = all_of(recoding_cols),
      .fns = ~ recode(.x,
                      "Never" = 1, 
                      "Rarely" = 2, 
                      "Sometimes" = 3, 
                      "Always" = 4)
    ))
}
