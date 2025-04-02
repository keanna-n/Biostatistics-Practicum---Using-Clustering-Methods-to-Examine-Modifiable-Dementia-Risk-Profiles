## self-efficacy score 
## 1, Not at all true | 2, Hardly true | 3, Moderately true | 4, Exactly true

self_efficacy_coding <- function(dat){
  dat2recode <- c("self_efficacy_solve_problems",
                  "self_efficacy_oppose",
                  "self_efficacy_goals",
                  "self_efficacy_unexpected_events",
                  "self_efficacy_unforeseen",
                  "self_efficacy_effort",
                  "self_efficacy_coping",
                  "self_efficacy_several_solutions",
                  "self_efficacy_trouble_solution",
                  "self_efficacy_handle")
  
  recoded_data <- dat %>% 
    mutate(across(
      .cols = all_of(dat2recode),
      .fns = ~ recode(.x,
                      "Not at all true" = 1,
                      "Hardly true" = 2,
                      "Moderately true" = 3,
                      "Exactly true" = 4
      )
    ))

}
