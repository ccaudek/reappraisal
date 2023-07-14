# Script name: ed_prl_fnc.R
# Project: Eating disorders, Montecatini
# Script purpose: functions to be used
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Jun 2 10:48 2021
# Last Modified Date: Tue Oct 19 09:34:53 2021
# 
# Notes: 


#' @description 
#' generate RDS file for PRL data
#' @return NULL.
#' @param 
#' GROUP: patient, control
#' STIMULUS: food, social
generate_raw_prl_data <- function() {
  
  dir <- here("data", "raw", "prl")
  
  file_names <- as.character(list.files(path=dir, pattern = "PRL_"))
  n_files <- length(file_names)
  n_files
  
  d_list <- list()
  
  for (i in 1:n_files) {
    
    d  <- read.table(here("data", "raw", "prl", file_names[i]))
    
    d$subj_idx <- file_names[i]
    d$epoch <- d$V1
    d$target_position <- d$V2
    d$stimulus_type <- d$V3
    d$stimulus_class <- d$V4
    d$which_image_is_rewarded_more <- d$V5
    d$image_category <- d$V6
    d$keypress <- d$V7 # (1=a, 2=l)
    d$rt <- d$V8
    d$feedback <- d$V9 # 1 = reward, 2 = punishment, 3 = too slow
    d$inter_trial_delay <- d$V10
    d$img1 <- d$V11
    d$img2 <- d$V12
    
    # on each trial, a food picture and a neutral image
    # the food/social image is coded as image2
    d$is_target_img_rewarded_in_first_epoch <- ifelse(
      d$V5 == "image1_rewarded7", "no", "yes"
    )
    
    d$epoch <- as.numeric(stri_sub(d$V1, 5, 5))
    # d$V1 <- NULL
    
    d$is_target_rewared_in_present_epoch <- rep(c(0, 1, 0, 1), each = 40)
    
    # V2: position of target image (sad face)
    # where is the target img?
    d$position_target_img <- stri_sub(d$V2, 8, 9)
    
    # V7 key pressed by participant
    # 1 : "a" -> sx
    # 2 : "l" -> dx
    
    # participant has chosen the image to the right or to the left
    d$resp <- ifelse(
      d$V7 == 1, "sx",
      ifelse(d$V7 == 2, "dx", "ERROR")
    )
    
    d$is_target_img_chosen <- ifelse(
      d$resp == d$position_target_img, 1, 0
    )
    
    d$is_positive_feedback <- case_when(
      d$V9 == 1 ~ "yes",
      d$V9 == 2 ~ "no",
      d$V9 == 3 ~ "no response"
    )
    
    d$trial <- 1:160
    
    d_list[[i]] <- d
  }
  
  # convert list into data.frame
  df <- do.call(rbind.data.frame, d_list)
  
  saveRDS(df, here("data", "proc", "prl", "prl_with_psychtoolkit_code.rds"))
  
}



# gen_data_list_with_prl_raw_data() ---------------------------------------

# Generate data list and add subj_name
# Reads the four RDS files with the raw PRL data, adds subj_name used in the
# questionnaires. The four data.frame are added to a list.

#' @description 
#' Read the raw data and add subj_name.
#' @input NULL
#' @return data_list list with 4 DataFrames:
#' data_list[[1]] : df_food_patients
#' data_list[[2]] : df_food_controls
#' data_list[[3]] : df_social_patients
#' data_list[[4]] : df_social_controls

write_prl_raw_data_list <- function() {
  
  df_food_patients <- add_quest_code_to_data(
    STIMULUS = "data_food", 
    GROUP = "patients"
  )
  
  df_food_controls <- add_quest_code_to_data(
    STIMULUS = "data_food", 
    GROUP = "controls"
  )
  
  df_social_patients <- add_quest_code_to_data(
    STIMULUS = "data_social", 
    GROUP = "patients"
  )
  
  df_social_controls <- add_quest_code_to_data(
    STIMULUS = "data_social", 
    GROUP = "controls"
  )
  
  data_list <- list()
  
  data_list[[1]] <- df_food_patients
  data_list[[2]] <- df_food_controls
  data_list[[3]] <- df_social_patients
  data_list[[4]] <- df_social_controls
  
  data_list
}



# add_quest_code_to_data() ------------------------------------------------


#' @description 
#' Generate a data.frame with the raw PRL data and the subj_name codes.
#' As input are used the psytoolkit codes.
#' @return 
#' Data.frame.
add_quest_code_to_data <- function(STIMULUS, GROUP) {
  
  correspondence_table_for_subj_codes <- gen_correspondence_table_codes()
  
  data_psytoolkit_code <- readRDS(
      here("data", "proc", "prl", "prl_with_psychtoolkit_code.rds")
    ) %>% 
      dplyr::rename("code_psytoolkit" = "subj_idx")
  
  
  df <- inner_join(
    correspondence_table_for_subj_codes, data_psytoolkit_code, 
    by = "code_psytoolkit"
  )
  df
}



# gen_correspondence_table_codes() ----------------------------------------


#' @description 
#' by using the information in the Excel file, generates a correspondence table
#' which associates the codes used by psytoolkit with the codes used in the
#' questionnaires
#' @return data.frame.
gen_correspondence_table_codes <- function(GROUP, STIMULUS) {
  
  d <- read_excel_code()

  d_clean <- d %>% 
    dplyr::rename(
      "subj_name" = "codice_matricola_1",
      "code_psytoolkit" = "esperimento_1"
      ) %>%  
    dplyr::select(subj_name, code_psytoolkit)
  
  d_clean2 <- d_clean[!is.na(d_clean$code_psytoolkit), ] 
  
  d_clean2
}


# read_excel_code() -------------------------------------------------------


#' @description 
#' read Excel file.
#' @return data.frame.
#' @param 
#' GROUP = "patients", EXCEL_FILE = "misc_food".
read_excel_code <- function() {
  d <- readxl::read_excel(
    here("data", "raw", "prl", "data.xlsx")
  )
  d
}


# gen_subj_name() ---------------------------------------------------------


#' @description 
#' generate subject code
#' @return data.frame.
#' @param data.frame.
gen_subj_name <- function(d) {
  
  library("stringi")
  
  d$mese_c <- ifelse(
    d$`mese:1` < 10, stri_join("0", as.character(d$`mese:1`), sep=''), as.character(d$`mese:1`)
  )
  
  d$giorno_c <- ifelse(
    d$`giorno:1` < 10, 
    stri_join("0", as.character(d$`giorno:1`), sep=''), 
    as.character(d$`giorno:1`)
  )
  
  d$cellulare_c <- ifelse(
    d$`cellulare:1` < 100, 
    stri_join("0", as.character(d$`cellulare:1`), sep=''), 
    as.character(d$`cellulare:1`)
  )
  
  d$sex <- ifelse(d$`sesso:1` == 1, "f",
                  ifelse(d$`sesso:1` == 2, "m", NA))
  
  d$subj_id <- tolower(
    stri_join(d$`nome:1`, d$`cognome:1`, d$`anno:1`, 
              d$mese_c, d$giorno_c, d$cellulare_c, d$sex, 
              sep='_')
  )
  
  d$code_psytoolkit <- d$`esperimento:1`
  
  d
}



# find_subj_code_low_rich_choices() ---------------------------------------


# Find control subjects with rich choices <= 0.5.

#' @description 
#' Only for controls: find subj_code with proportion of rich choices < 0.5.
#' @input 
#' Address of data frame.
#' @return vector of subj_code.
find_subj_code_low_rich_choices <- function(file_address) {
  
  d <- readRDS(file_address)
  
  d$feedback <- ifelse(
    d$feedback == 2, 0, d$feedback
  )
  d$feedback <- ifelse(d$feedback == 3, NA, d$feedback)
  
  d$rich_choice <- case_when(
    d$is_target_rewared_in_present_epoch & d$is_target_img_chosen ~ 1,
    !d$is_target_rewared_in_present_epoch & !d$is_target_img_chosen ~ 1,
    TRUE ~ 0
  )
  
  foo <- d %>% 
    group_by(group, subj_name) %>% 
    summarise(
      avg_rich_choices = mean(rich_choice, na.rm = TRUE),
      avg_feedback = mean(feedback, na.rm = TRUE),
      n = n()
    ) 
  
  boxplot(foo$avg_feedback ~ foo$group)
  
  temp <- foo[foo$group == "controls" & foo$avg_feedback < 0.5, ]
  length(temp$subj_name)
  

  bad_ids_accuracy <- foo[
    foo$avg_rich_choices <= 0.5 & foo$group == "controls", 
  ]$subj_name
  
  bad_ids_accuracy
  
}


# find_subj_code_extreme_keypress() ---------------------------------------


# Find subjects with extreme keypress distributions.
# Each stimulus (e.g., food image) was presented to the left in the 
# 50% of cases, and on the right the other 50%. Pressing too often one key 
# means that the participant does not perform the task. Flagged subjects are 
# outside the interval [0.2, 0.8].

#' @description 
#' Find subject codes for control participants who pressed too often one of   
#' the two keys. 
#' @input 
#' Address of data frame.
#' @return 
#' Vector with subj_name codes.
find_subj_code_extreme_keypress <- function (d) {
  
  foo <- d %>% 
    group_by(group, stimulus_type, subj_name) %>% 
    summarise(
      y = mean(keypress-1, na.rm = TRUE)
    ) 
  
  foo1 <- foo[(foo$y < 0.3 | foo$y > 0.7) & foo$group == "controls", ]
  foo1 %>% 
    as.data.frame()
  
  bad_ids_controls_keypress <- factor(unique(foo1$subj_name))
  length(bad_ids_controls_keypress)
  
  bad_ids_controls_keypress
}


# add_diagnostic_category() ----------------------------------------------------

add_diagnostic_category <- function() {
  
  # Read patients list and diagnostic category.
  patients_from_excel_list <- 
    rio::import(
      here::here(
        "data", "raw", "diagnostic_categories", "diagn_cat.xlsx"
      )
    ) 
  
  tbl_patients <- tibble(
    subj_name = patients_from_excel_list$subj_code,
    diag_cat = patients_from_excel_list$category
  )
  
  # Read complete raw PRL data.
  prl_raw_data <- readRDS(
    here("data", "processed", "prl", "complete_cleaned_raw_data", 
         "raw_data_prl_both_groups.rds")
  )
  
  prl_raw_data_and_diag_cat <- 
    left_join(prl_raw_data, tbl_patients, by = "subj_name")
  # table(prl_raw_data_and_diag_cat$diag_cat, prl_raw_data_and_diag_cat$group)
  
  prl_raw_data_and_diag_cat$diag_cat <- ifelse(
    is.na(prl_raw_data_and_diag_cat$diag_cat), "HC", 
    prl_raw_data_and_diag_cat$diag_cat
  )
  # summary(factor(prl_raw_data_and_diag_cat$diag_cat))
  
  # Read questionaires data
  quest_data <- readRDS(
    here::here("data", "processed", "quest", "quest_diagn_data.rds")
  )
  
  quest_info <- quest_data %>%
    dplyr::select(subj_code, eat26_tot) %>% 
    distinct() %>% 
    dplyr::rename(
      subj_name = subj_code
    )
  
  prl_tot_raw_data <- left_join(prl_raw_data_and_diag_cat, quest_info, by = "subj_name")
  
  prl_tot_raw_data$diag_cat <- ifelse(
    ((prl_tot_raw_data$diag_cat == "HC") & (prl_tot_raw_data$eat26_tot > 19)), 
    "RI", prl_tot_raw_data$diag_cat
  )
  
  saveRDS(
    prl_tot_raw_data,
    here::here("data", "processed", "prl", "raw_prl_data", "prl_tot_raw_data.rds")
  )
}


# write_input_for_hddmrl() -----------------------------------------------

write_input_for_hddmrl <- function(prl_data) {
  
  prl_data$feedback <- ifelse(
    prl_data$feedback == 2, 0, prl_data$feedback
  )
  prl_data$feedback <- ifelse(prl_data$feedback == 3, NA, prl_data$feedback)
  
  prl_data$feedback <- 
    ifelse(prl_data$rt < 150 | prl_data$rt > 2499, NA, prl_data$feedback)
  
  prl_data$rt1 <- 
    ifelse(prl_data$rt < 150 | prl_data$rt > 2499, NA, prl_data$rt)
  
  prl_data$rt1 <- ifelse(prl_data$trial == 1, NA, prl_data$rt1)

  prl_data %>% 
    summarise(
      n = n_distinct(subj_name)
    )
  
  
  data_for_imp <- prl_data %>% 
    dplyr::select(
      subj_name, stimulus_type, epoch, target_position, which_image_is_rewarded_more,
      keypress, rt1, feedback, is_target_img_rewarded_in_first_epoch, 
      is_target_rewared_in_present_epoch, position_target_img, resp, 
      is_target_img_chosen, trial
    )
  
  set.seed(123)
  miceObj <- miceRanger::miceRanger(
    data_for_imp
    , m = 1
    , returnModels = TRUE
    , verbose = TRUE
  )
  
  dataList <- miceRanger::completeData(miceObj)
  
  d <- dataList[[1]]
  
  accuracy_df <- d %>% 
    group_by(subj_name) %>% 
    summarise(
      avg_acc = mean(feedback, na.rm = TRUE),
      avg_rt = median(rt1, na.rm = TRUE)
    ) %>% 
    arrange(avg_acc, desc(avg_rt))
  
  accuracy_df %>% as.data.frame()
  
  ids_bad_cntr_acc_rt <- 
    accuracy_df[(accuracy_df$avg_acc < 0.5 | accuracy_df$avg_rt < 300), ]$subj_name
  
  prl_data3 <- d[!(d$subj_name %in% ids_bad_cntr_acc_rt), ]
  
  prl_data3 %>% 
    # group_by(group, diag_cat) %>% 
    summarise(
      n = n_distinct(subj_name)
    )
  
  prl_data3$is_rich_choice <- case_when(
    prl_data3$is_target_rewared_in_present_epoch & prl_data3$is_target_img_chosen ~ 1,
    !prl_data3$is_target_rewared_in_present_epoch & !prl_data3$is_target_img_chosen ~ 1,
    TRUE ~ 0
  )
  
  # response: which image has been chosen in each trial
  prl_data3$response <- prl_data3$is_target_img_chosen
  
  # d$is_patient <- ifelse(d$group == "patients", 1, 0)
  
  # 
  # # Multiple imputation on NAs.
  # temp <- data.frame(
  #   rt1         = d$rt1, 
  #   trial       = d$trial,
  #   feedback    = d$feedback, 
  #   rich_choice = d$is_rich_choice,
  #   is_patient  = d$is_patient,
  #   response    = d$response
  # )
  # 
  # # Imputes the "best value" according to the linear regression model, also 
  # # known as regression imputation.
  # imp <- mice::mice(temp, method = "norm.predict", m = 1) 
  # temp <- complete(imp)
  
  # d$feedback <- ifelse(temp$feedback > 0.5, 1, 0)
  
  prl_data3$rt <- prl_data3$rt1 / 1000
  prl_data3$rt1 <- NULL
  
  # I want to number each subject in the data.frame so that subjects are 
  # ordered sequentially, according to the order they appear in the data frame. 
  # https://community.rstudio.com/t/why-does-group-indices-use-alphabetical-ordering/5452
  # As suggested in the above link, I wrap group_indices in another function:
  grpid = function(x) match(x, unique(x))
  # then
  d1 <- prl_data3 %>% 
    mutate(subj_idx = group_indices(., subj_name) %>% grpid)
  # In this manner, the variable subj_idx assigns an integer to each subject;
  # this integer is ordered according to the sequence in which the subjects are 
  # present in the data.frame.
  # table(d3$subj_idx)
  # unique(d3$subj_idx)
  
  # d1$key_press <- ifelse(d1$resp == "sx", 0, 1)
  # sdb <- function(x) {
  #   sqrt((1/length(x)) *  mean(x) * (1 - mean(x)))
  # }
  # 
  # out <- d1 %>% 
  #   group_by(subj_name, stimulus) %>% 
  #   summarise(
  #     intra_sd = sdb(key_press)
  #   )
  # 
  # ggplot(out, aes(intra_sd, fill = stimulus)) + 
  #   geom_density(alpha = 0.2) +
  #   xlim(0.0375, 0.04)
  # out[out$intra_sd < 0.0385, ]$subj_name
  
  
  df_for_hddm <- data.frame(
    subj_idx   = d1$subj_idx,
    response   = d1$response,
    rt         = d1$rt,
    trial      = d1$trial,
    split_by   = 1,
    feedback   = d1$feedback,
    subj_code  = d1$subj_name,
    q_init     = 0.5
  )
  
  mydat <- df_for_hddm %>% 
    dplyr::arrange(subj_idx, trial, split_by)
  
  mydat %>% 
    summarise(
      n = n_distinct(subj_code)
    )
  
  rio::export(
    mydat, 
    here("data", "proc", "prl", "input_for_hddmrl", "hddm_input_20220603v1.csv")
  )
  
  lut <- tibble(
    subj_idx = mydat$subj_idx, 
    subj_code = mydat$subj_code
  ) %>% 
    distinct()
  
  rio::export(
    lut, 
    here("data", "proc", "prl", "input_for_hddmrl", "hddm_look_up_table.csv")
  )
  
}




#---- EOF ----#


























#' 
#' #' @description 
#' #' Flag at-risk (EAT-26) controls
#' #' @return NULL. Save file "tot_raw_data_prl.rds".
#' flag_at_risk_eat26_controls <- function() {
#'   
#'   at_risk_subj_names <- get_subj_name_at_risk_eat26() %>% 
#'     factor()
#'   patients_codes <- get_patients_codes()
#'   
#'   at_risk_controls_codes <- setdiff(at_risk_subj_names, patients_codes)
#'   
#' }
#' 
#' 
#' 
#' 
#' #' @description 
#' #' Get subj_name of at-risk subjects from questionnaires data.
#' #' @return Vector of subj_names.
#' get_subj_name_at_risk_eat26 <- function() {
#'   
#'   # Read questionnaire data
#'   quest_df <- readRDS(here("data", "processed", "quest", "quest_data.rds"))
#'   
#'   
#'   subjects_at_risk <- quest_df %>% 
#'     dplyr::filter(at_risk == 1)
#'   
#'   # these are both controls and patients
#'   subj_name_at_risk <- subjects_at_risk$subj_code
#'   
#'   subj_name_at_risk
#' }






