# A function to compute perceived stress score based on responses to the perceived stress score scale
# The perceived stress scale (PSS) is composed of 10 items. Each item uses a 5-point (0-4) Likert scale:
  # Never, Almost never, Sometimes, Fairly often, Very often
  # Some items are reverse scored and is accounted for in the code
  # The PSS score is computed by summing the numeric score of all 10 items
  # NOTE: The current version of this function treats missing values as 0. Check data for missingness before using the function.

# Input: A df consisting of all the relevant questionnaire variables (record_id, pss_01:pss_10). Data from REDCap should be supplied so that variable names are in "raw" format and data values are in "label" format.
# Output: A revised df that contains record_id and PSS score

compute_perceived_stress <- function(dat){
  
  conventional_scoring_vars <- c("pss_01", "pss_02", "pss_03",
                                 "pss_06", "pss_09", "pss_10")
  
  reverse_scoring_vars <- c("pss_04", "pss_05", "pss_07", "pss_08")
  
  pss_score <- dat %>% 
    select(record_id,
           pss_01:pss_10) %>% 
    mutate_at(vars(all_of(conventional_scoring_vars)), ~case_when(. == "Never" ~ "0",
                                           . == "Almost never" ~ "1",
                                           . == "Sometimes" ~ "2",
                                           . == "Fairly often" ~ "3",
                                           . == "Very often" ~ "4",
                                           TRUE ~ .)) %>% 
    mutate_at(vars(all_of(reverse_scoring_vars)), ~case_when(. == "Never" ~ "4",
                                                    . == "Almost never" ~ "3",
                                                    . == "Sometimes" ~ "2",
                                                    . == "Fairly often" ~ "1",
                                                    . == "Very often" ~ "0",
                                                    TRUE ~ .)) %>% 
    mutate_at(vars(-record_id), as.numeric) %>% 
    rowwise() %>% 
    mutate(pss_score = sum(c_across(pss_01:pss_10), na.rm=T)) %>% 
    select(record_id, pss_score)
  
  return(pss_score)
}
