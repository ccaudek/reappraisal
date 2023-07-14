# Script name: 001_gen_data_for_hddmrl_food_neutral.R
# Project: eating disorders, Montecatini
# Script purpose: create single file with all raw PRL data.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Jun  2 10:07:50 2021
# Last Modified Date: Sat Jun 18 10:04:13 2022
# 
# Notes: 
#   1. Identify at-risk controls (EAT-26) and exclude them.

# Prelims
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("stringi")
  library("readxl")
})

# Increase max print
options(max.print = .Machine$integer.max)

# source(here("code", "functions", "funs_gen_data_for_hddm.R"))
source(here::here("scripts", "functions", "funs_prl.R"))


# 1. Merge psytoolkit PRL files
generate_raw_prl_data()

# 2. Add subj_code
df <- add_quest_code_to_data()

# 3. Write input for hddmrl
write_input_for_hddmrl(df)






# ----- eof -------

