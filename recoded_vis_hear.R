## recoding vision and hearing for rhhis & other vision and hearirng questions 
#rhhis: 0, No | 2, Sometimes | 4, Yes
#other qs: 2, Yes | 1, Sometimes | 0, No

vis_hear_recoding <- function(dat){
  recode_rhhis <- c("rhhis_1",
                    "rhhis_2",
                    "rhhis_3",
                    "rhhis_4",
                    "rhhis_5",
                    "rhhis_6",
                    "rhhis_7",
                    "rhhis_8",
                    "rhhis_9",
                    "rhhis_10")
  vision_hearing_qs <- c("hearing_phone",
                         "vision_print",
                         "vision_hearing_understand_info",
                         "vision_hearing_pa",
                         "vision_hearing_angry",
                         "vision_hearing_recognize",
                         "vision_hearing_social",
                         "vision_hearing_difficult")
  recoded_dat <- dat %>% 
    mutate(across(
      .cols = recode_rhhis,
      .fns = ~ recode(.x,
                      "No" = 0,
                      "Sometimes" = 2,
                      "Yes" = 4)
    )) %>%
    mutate(across(
      .cols = vision_hearing_qs,
      .fns = ~ recode(.x,
                      "No" = 0,
                      "Sometimes" = 1,
                      "Yes" = 2)
    ))
  vis_hear_dat <- recoded_dat %>%
    rowwise() %>%
    mutate(
      rhhis_sum = sum(c_across(all_of(recode_rhhis))),
      vis_hear_sum = sum(c_across(all_of(vision_hearing_qs)))
    ) %>%
    ungroup() %>%
    select(all_of(recode_rhhis), all_of(vision_hearing_qs), rhhis_sum, vis_hear_sum)
  
  return(vis_hear_dat)
}
