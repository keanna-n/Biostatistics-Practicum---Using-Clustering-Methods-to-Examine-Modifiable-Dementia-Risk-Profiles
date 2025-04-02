# ##health conditions 
# 
# #cardiovascular
###vascular
# health_conditions_pvd
# health_conditions_hbp
# health_conditions_high_chol
# health_conditions_tia
# health_conditions_stroke
# health_conditions_cerebrovascular_disease

# health_conditions_cardiovascular_disease

# health_conditions_heart_attack
# health_conditions_heart_surgery
###structural
# health_conditions_heart_disease
# health_conditions_valve_disease
# health_conditions_heart_failure
###rhythim 
# health_conditions_palpitations
# health_conditions_afib
# health_conditions_murmur
# health_conditions_pacemaker
###symptoms
# health_conditions_pain_rest
# health_conditions_discomfort
# health_conditions_ankle_swell
# 
# #pancreas
# health_conditions_diabetes
# 
# # kidney 
# health_conditions_kidney_disease
# 
# #respiratory 
# health_conditions_sob_rest
# health_conditions_sob_flat
# health_conditions_sob_sleep
# health_conditions_fatigue
# health_conditions_lung_disease
# 
# #neurological
# health_conditions_dizzy
# health_conditions_apnea
# health_conditions_sci
# health_conditions_neuropathy
# health_conditions_ms
# health_conditions_mci
# health_conditions_pd
# 
# #oral and dental 
# health_conditions_gum_disease
# 
# #MSK
# health_conditions_arthritis
# health_conditions_osteo_arthritis
# health_conditions_osteoporosis
# health_conditions_back_pain
# health_conditions_pain
# health_conditions_muscle_pain
# 
# #psychological health (mental illness)
# health_conditions_attn_deficit
# health_conditions_depression
# health_conditions_anxiety
# health_conditions_bipolar
# health_conditions_alchl_sub_disorder
# health_conditions_maj_psychiatric
# health_conditions_eating
# 
# #bladder 
# health_conditions_incontinence
# 
# #cancer 
# health_conditions_cancer
# 
# #covid
# health_conditions_covid19
#
# #insomnia 
# health_conditions_insomnia

hlth_coding <- function(row) {
  hlth_conditions <- list()
  hlth_conditions$record_id <- row["record_id"]
  ## cardiovascular
  # vascular
  if (row["health_conditions_pvd"] == "Yes" || 
      row["health_conditions_hbp"] == "Yes" ||
      row["health_conditions_high_chol"] == "Yes" || 
      row["health_conditions_tia"] == "Yes" || 
      row["health_conditions_stroke"] == "Yes" ||
      row["health_conditions_cardiovascular_disease"] == "Yes" || 
      row["health_conditions_cerebrovascular_disease"] == "Yes" ||
      row["health_conditions_heart_attack"] == "Yes" ||
      row["health_conditions_heart_surgery"] == "Yes"
      ) {
    hlth_conditions$cardio_vascular <- "Yes"
  } else {
    hlth_conditions$cardio_vascular <- "No"
  }
  
  # structural
  if (row["health_conditions_heart_disease"] == "Yes" || 
      row["health_conditions_valve_disease"] == "Yes" || 
      row["health_conditions_heart_failure"] == "Yes") {
    hlth_conditions$cardio_structural <- "Yes"
  } else {
    hlth_conditions$cardio_structural <- "No"
  }
  
  # rhythm
  if (row["health_conditions_palpitations"] == "Yes" || 
      row["health_conditions_afib"] == "Yes" ||
      row["health_conditions_murmur"] == "Yes" ||
      row["health_conditions_pacemaker"] == "Yes") {
    hlth_conditions$cardio_rhythm <- "Yes"
  } else {
    hlth_conditions$cardio_rhythm <- "No"
  }
  
  # symptoms
  if (row["health_conditions_pain_rest"] == "Yes" || 
      row["health_conditions_discomfort"] == "Yes" || 
      row["health_conditions_ankle_swell"] == "Yes") {
    hlth_conditions$cardio_symptoms <- "Yes"
  } else {
    hlth_conditions$cardio_symptoms <- "No"
  }
  
  ## pancreas
  hlth_conditions$diabetes <- row["health_conditions_diabetes"]
  
  ## kidney
  hlth_conditions$kidney <- row["health_conditions_kidney_disease"]
  
  ## respiratory
  if (row["health_conditions_sob_rest"] == "Yes" || 
      row["health_conditions_sob_flat"] == "Yes" || 
      row["health_conditions_sob_sleep"] == "Yes" || 
      row["health_conditions_fatigue"] == "Yes" || 
      row["health_conditions_lung_disease"] == "Yes") {
    hlth_conditions$respiratory <- "Yes"
  } else {
    hlth_conditions$respiratory <- "No"
  }
  
  ## neurological
  if (row["health_conditions_dizzy"] == "Yes" || 
      row["health_conditions_apnea"] == "Yes" || 
      row["health_conditions_sci"] == "Yes" || 
      row["health_conditions_neuropathy"] == "Yes" || 
      row["health_conditions_ms"] == "Yes" || 
      row["health_conditions_mci"] == "Yes" || 
      row["health_conditions_pd"] == "Yes") {
    hlth_conditions$neurological <- "Yes"
  } else {
    hlth_conditions$neurological <- "No"
  }
  
  ## oral and dental
  hlth_conditions$oral_dental <- row["health_conditions_gum_disease"]
  
  ## msk
  if (row["health_conditions_arthritis"] == "Yes" || 
      row["health_conditions_osteo_arthritis"] == "Yes" || 
      row["health_conditions_osteoporosis"] == "Yes" || 
      row["health_conditions_back_pain"] == "Yes" || 
      row["health_conditions_pain"] == "Yes" || 
      row["health_conditions_muscle_pain"] == "Yes") {
    hlth_conditions$msk <- "Yes"
  } else {
    hlth_conditions$msk <- "No"
  }
  
  ## psychological
  if (row["health_conditions_attn_deficit"] == "Yes" || 
      row["health_conditions_depression"] == "Yes" || 
      row["health_conditions_anxiety"] == "Yes" || 
      row["health_conditions_bipolar"] == "Yes" || 
      row["health_conditions_alchl_sub_disorder"] == "Yes" || 
      row["health_conditions_maj_psychiatric"] == "Yes" || 
      row["health_conditions_eating"] == "Yes") {
    hlth_conditions$psych <- "Yes"
  } else {
    hlth_conditions$psych <- "No"
  }
  
  ## bladder
  hlth_conditions$bladder <- row["health_conditions_incontinence"]
  
  ## cancer
  hlth_conditions$cancer <- row["health_conditions_cancer"]
  
  ## covid
  hlth_conditions$covid <- row["health_conditions_covid19"]
  
  ## insomnia
  hlth_conditions$insomnia <- row["health_conditions_insomnia"]
  

  return(unlist(hlth_conditions))
}
