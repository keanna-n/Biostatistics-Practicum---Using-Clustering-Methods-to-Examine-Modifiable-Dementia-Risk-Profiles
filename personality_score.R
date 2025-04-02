## personality score 
## scoring based on each group 

#1, Strongly disagree | 
#2, Disagree | 
#3, Somewhat disagree | 
#4, Neither agree nor disagree | 
#5, Somewhat agree | 
#6, Agree | 
#7, Strongly agree


BFI_scoring <- function(dat) {
  # Define variable groups
  neuroticism <- c("bfi_worry_n_01", 
                   "bif_nervous_n_02")
  neuroticism_neg <- c("bfi_calm_n_03")
  extroversion <- c("bfi_talkative_e_04", 
                    "bfi_outgoing_e_05")
  extroversion_neg <- c("bfi_reserved_e_06")
  openness <- c("bfi_original_o_07", 
                "bfi_artistic_o_08", 
                "bfi_imagination_o_09")
  agreeableness <- c("bfi_forgive_a_11", 
                     "bfi_kind_a_12")
  agreeableness_neg <- c("bfi_rude_a_10")
  conscientiousness <- c("bfi_thorough_c_13", 
                         "bfi_efficient_c_15")
  conscientiousness_neg <- c("bfi_lazy_c_14")
  
  # Recode items
  recoding_data <- dat %>%
    mutate(across(
      .cols = all_of(c(neuroticism, extroversion, openness, agreeableness, conscientiousness)),
      .fns = ~ recode(.x,
                      "Strongly disagree" = 1,
                      "Disagree" = 2,
                      "Somewhat disagree" = 3,
                      "Neither agree nor disagree" = 4,
                      "Somewhat agree" = 5,
                      "Agree" = 6,
                      "Strongly agree" = 7)
    )) %>%
    mutate(across(
      .cols = all_of(c(neuroticism_neg, extroversion_neg, agreeableness_neg, conscientiousness_neg)),
      .fns = ~ recode(.x,
                      "Strongly disagree" = 7,
                      "Disagree" = 6,
                      "Somewhat disagree" = 5,
                      "Neither agree nor disagree" = 4,
                      "Somewhat agree" = 3,
                      "Agree" = 2,
                      "Strongly agree" = 1)
    ))
  
  # Calculate personality scores
  personality_scores <- recoding_data %>%
    rowwise() %>%
    mutate(
      neuroticism = sum(c_across(all_of(c(neuroticism, neuroticism_neg)))),
      extroversion = sum(c_across(all_of(c(extroversion, extroversion_neg)))),
      openness = sum(c_across(all_of(openness))),
      agreeableness = sum(c_across(all_of(c(agreeableness, agreeableness_neg)))),
      conscientiousness = sum(c_across(all_of(c(conscientiousness, conscientiousness_neg))))
    ) %>%
    ungroup() %>%
    select(record_id,neuroticism, extroversion, openness, agreeableness, conscientiousness)
  
  return(personality_scores)
}
