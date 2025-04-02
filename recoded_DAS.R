## recode DAS scores to sum

#0, Did not apply to me at all | 
#1, Applied to some degree or some of the time | 
#2, Applied to me a considerable degree or a good part of the time | 
#3, Applied to me very much or most of the time

### frailty exhaust 
#0, Rarely or none of the time (less than one day) | 
#1, Some or a little of the time (1-2 days) | 
#2, A moderate amount of the time (3-4 days) | 
#3, Most of the time


recoded_DAS <- function(dat){
  recoding_cols <- c("dass_01",
                     "dass_02",
                     "dass_03",
                     "dass_04",
                     "dass_05",
                     "dass_06",
                     "dass_07",
                     "dass_08",
                     "dass_09",
                     "dass_10",
                     "dass_11",
                     "dass_12",
                     "dass_13",
                     "dass_14",
                     "dass_15",
                     "dass_16",
                     "dass_17",
                     "dass_18",
                     "dass_19",
                     "dass_20",
                     "dass_21")
  
  recoding_cols2 <- c("frailty_exhaust_1",
                      "frailty_exhaust_2")
  
  recoded_data <- dat %>%
    mutate(across(
      .cols = all_of(recoding_cols2),
      .fns = ~ recode(.x,
                      "Rarely or none of the time (less than one day)" = 0, 
                      "Some or a little of the time (1-2 days)" = 1, 
                      "A moderate amount of the time (3-4 days)" = 2,
                      "Most of the time" = 3)
    )) %>%
    mutate(across(
      .cols = all_of(recoding_cols),
      .fns = ~ recode(.x,
                      "Did not apply to me at all" = 0,
                      "Applied to some degree or some of the time" = 1, 
                      "Applied to me a considerable degree or a good part of the time" = 2, 
                      "Applied to me very much or most of the time" = 3)
    ))
  
  return(recoded_data)
}
