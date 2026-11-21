library(data.table)
library(tidyverse)
library(dplyr)
library(stringr)
library(openxlsx)
library(ggplot2)
`%notin%` <- Negate(`%in%`)

#Set working directory
setwd("/sc/arion/projects/mscic1/mullins_lab_biome_projs/lorenza/")

#Read in data
metadata <- fread("/sc/arion/projects/MSM/data/phenotypes/2026-08-12/Clarity/Encounter_Diagnosis.txt",
                  select = c("sample_name", "dx_code", "dx_code_type", "dx_name", "encounter_date")) %>%
  rename(FID = sample_name, ICD_code = dx_code) %>%
  mutate(data_src = "EncounterDiagnosis") %>%
  distinct() 

problem_list <- fread("/sc/arion/projects/MSM/data/phenotypes/2026-08-12/Clarity/Problem_List.txt", 
                      select = c("sample_name", "icd9_code", "code_type","dx_name","encounter_date")) %>%
  rename(FID = sample_name, ICD_code = icd9_code, dx_code_type = code_type) %>%
  mutate(data_src = "ProblemList") %>%
  distinct()

med_hist_list <- fread("/sc/arion/projects/MSM/data/phenotypes/2026-08-12/Clarity/Medical_History.txt", 
                       select = c("sample_name", "current_icd9_list", "current_icd10_list",
                                  "MEDICAL_HX_DESCRIPTION", "encounter_date")) %>%
  rename(FID = sample_name, dx_name = MEDICAL_HX_DESCRIPTION) %>%
  pivot_longer(c(current_icd9_list, current_icd10_list), 
               names_pattern = "current_(icd\\d+)_list", 
               names_to = "dx_code_type", values_to = "ICD_code") %>%
  separate_longer_delim(ICD_code, delim = ",") %>%
  mutate(ICD_code = str_trim(ICD_code), 
         data_src = "MedHist") %>%
  filter(!is.na(ICD_code), ICD_code != "") %>%
  distinct()

# Okay me re looking at all this . like . lemme fix this but play around with it .
# if you know faster checking things other than like %in% lmk because thats what 
# i was obviously relying on .

# So i think we should only pool tg the encounter diagnosis and problems list bc
# example of FID w Suicidal ideation is : FID = "SINAI-Million_1067118_3600794459"
# and ICD_code == "R45.851"
# if you filter med hist w that you just see it repeats over and over again 
# and the patient has no SI labels since coming to sinai (i.e. not in 
# encounters or problem list)

# I didn't really look at this too hard bc for SI / SA we just need 
# one ICD code to qualify someone as SI / SA patient
# but anyways if we want to look at like counts per patient
# I might qualify med history as just one time if a code for like SI comes
# up multiple times bc i think otherwise we'd overcount

full_metadata <- rbind(metadata, problem_list, med_hist_list) %>% 
  distinct(FID, ICD_code, encounter_date, .keep_all = TRUE)


# Changing to get previous --> Realizing you don't have access to this sid but I 
# can just pull the final data table and send to you for anyone 
# who got codes from Sarah's earlier NLP method
prior_population <- fread("/sc/arion/projects/mscic1/mullins_lab_biome_projs/cnv_sui/final_analytic_set_all_notes_20240515_subjectids.txt", 
                          select = c("SUBJECT_ID", "si_icd", "nsa_icd", "any_icd", 
                                     "predSI", "predSB", "nlp_si_sb", "any_sui", 
                                     "any_si", "any_sa", "in_notes",
                                     "any_si_cleaned_controls", "any_sa_cleaned_controls", "any_si_only_cleaned_controls",
                                     "genetically_determined", "sex", "CURRENT_AGE_2024")) %>%
  rename(FID = SUBJECT_ID) 


covs <- fread("files/case_controls/demographic_covs.tsv")

covariates <- fread("/sc/arion/projects/MSM/data/phenotypes/2025-07-08/SINAI-Million-BioMe1_Freeze_One.covariates.tsv",
                    select = c("FID", "SequencedGender", "Class"))

#Create vectors of codes and normalize
na_sa_codes <- fread("files/case_controls/narrow_sa_ICD.txt", header = FALSE)[,.(ICD_code=V1)]
b_sa_codes <- fread("files/case_controls/broad_sa_ICD.txt", header = FALSE)[,.(ICD_code=V1)] %>%
  filter(ICD_code %notin% unique(na_sa_codes$ICD_code))
si_codes <- fread("files/case_controls/si_ICD.txt", header = FALSE)[,.(ICD_code=V1)]

na_sa_codes$ICD_group <- "na_sa"
b_sa_codes$ICD_group <- "b_sa"
si_codes$ICD_group <- "si"

include_codes <- rbind(si_codes, na_sa_codes, b_sa_codes)

metadata_clean <- full_metadata %>%
  mutate(ICD_code = trimws(as.character(ICD_code))) %>%
  distinct(FID, ICD_code, encounter_date, .keep_all = TRUE)%>%
  group_by(FID) %>%
  mutate(has_included = any(ICD_code %in% unique(include_codes$ICD_code)),
         status = ifelse(has_included, "case", "control")) %>%
  
  ungroup() %>%
  select(-has_included)

#Define cases and controls
cases <- metadata_clean %>% filter(status == "case", ICD_code %in% unique(include_codes$ICD_code)) %>%
  merge(include_codes, by = "ICD_code")
controls  <- metadata_clean %>% filter(status == "control" )

#Reshape so one row per FID, columns are ICD codes, add SUM column totaling all ICD code counts per individual
matrix <- cases %>% 
  group_by(ICD_group) %>%
  count(FID, ICD_code) %>%
  pivot_wider(names_from = ICD_code, values_from = n, values_fill = 0) %>%
  mutate(SUM = rowSums(across(-FID))) %>%
  ungroup()

unique_cases <- matrix %>%
  select(FID, ICD_group, SUM) %>%
  pivot_wider(names_from = ICD_group, values_from = SUM, values_fill = 0) %>% 
  mutate(si_icd = case_when(
    si > 0 ~ 1),
    nsa_icd = case_when(
      na_sa > 0 ~ 1),
    any_icd = 1, 
    any_sui = case_when(
      na_sa > 0 | b_sa > 0 ~ 1), 
    any_sa = case_when(
      na_sa > 0 ~ 1 ),
    any_si = case_when(
      si > 0 ~ 1),
    any_si_cleaned_controls = case_when(
      any_si > 0 ~ 1, 
      b_sa > 0 | any_sa > 0 ~ -9 ), 
    any_sa_cleaned_controls = case_when(
      na_sa > 0 ~ 1, 
      si > 0 ~ -9
    ),
    any_si_only_cleaned_controls = case_when(
      si > 0 & b_sa == 0 & na_sa == 0 ~ 1, 
      b_sa != 0 | na_sa != 0 ~ -9),
    # any_nsa = case_when(
    #   na_sa > 0 ~ 1)
  ) %>%
  mutate(across(everything(), ~replace_na(.x, 0)))



msm_0826 <- bind_rows(unique_cases, 
                      (controls %>% select(FID) %>% distinct())) %>%
  mutate(across(everything(), ~ replace(., is.na(.), 0)))

# Examine Continued Cases
continued_cases <- msm_0826 %>% filter(FID %in% unique(prior_population$FID)) %>%
  left_join(prior_population, by = "FID") %>%
  mutate(si_status = case_when(
    any_si.x == 1 & any_si.y == 1 ~ "ContinuedCase",
    any_si.x == 0 & any_si.y == 0 ~ "ContinuedControl",
    any_si.x == 0 & any_si.y == 1 & in_notes != "0" & si_icd.y == 0 ~ "NLP_SI",
    any_si.x == 0 & any_si.y == 1 ~ "Unsure",
    any_si.x == 1 & any_si.y == 0 ~ "NewCase"
  ), sa_status =  case_when(
    any_sa.x == 1 & any_sa.y == 1 ~ "ContinuedCase",
    any_sa.x == 0 & any_sa.y == 0 ~ "ContinuedControl",
    any_sa.x == 0 & any_sa.y == 1 & in_notes != "0" & nsa_icd.y == 0 ~ "NLP_SA", 
    any_sa.x == 0 & any_sa.y == 1 ~ "Unsure",
    any_sa.x == 1 & any_sa.y == 0 ~ "NewCase"
  ), nsa_status = case_when(
    nsa_icd.x == 1 & nsa_icd.y == 1 ~ "Continuedcase", 
    nsa_icd.x == 1 & nsa_icd.y == 0 ~ "New", 
    nsa_icd.x == 0 & nsa_icd.y == 1 ~ "Uncertain", 
    nsa_icd.x == 0 & nsa_icd.y == 0 ~ "ContinuedControl")) %>%
  relocate(FID, si_status, sa_status, nsa_status)


total_data <- msm_0826 %>% 
  left_join((continued_cases %>% select(FID, si_status,
                                        sa_status, nsa_status,
                                        nlp_si_sb, in_notes)), by = "FID" ) %>%
  mutate(across(everything(), ~ if (is.numeric(.x)) {
    replace_na(.x, 0)
  } else {
    replace_na(.x, "")
  }))


total_data <- total_data  %>%
  mutate( 
    any_sa = ifelse(sa_status == "NLP_SA", 1, any_sa), 
    any_si = ifelse(si_status == "NLP_SI", 1, any_si), 
    any_si_cleaned_controls = case_when(
      any_si > 0 ~ 1,
      any_sa > 0 | b_sa > 0 ~ -9),
    any_sa_cleaned_controls = case_when(
      any_sa > 0 ~ 1,
      b_sa > 0 ~ -9
    ),
    any_si_only_cleaned_controls = case_when(
      any_si > 0 & any_sa == 0 ~ 1,  # b_sa == 0
      any_sa != 0  | b_sa != 0 ~ -9),
    across(everything(), ~replace_na(.x, 0)))

#------------Case counts random files 

#Count total occurrences and number of unique individuals per SI/SA code
code_counts <- cases %>%
  group_by(ICD_group) %>% 
  count(ICD_code, name = "count") %>%
  mutate(num_individuals = map_int(ICD_code, ~ n_distinct(cases$FID[cases$ICD_code == .x]))) %>%
  arrange(desc(num_individuals)) %>%
  ungroup()

#Save file
fwrite(cases, "upd_0826_cases_filtered.tsv", sep='\t')
fwrite(controls, "upd_0826_controls.tsv", sep='\t')
fwrite(code_counts, "upd_0826_codes_counts.txt", sep='\t')
fwrite(matrix, "upd_0826_codes_matrix.txt", sep='\t')
write.xlsx(code_counts, file="upd_0826_codes_counts.xlsx", sheetName="si_nsa_bsa_code_counts", rowNames=FALSE, append=TRUE)

#Check case/control counts by SequencedGender
case_ids <- as.data.table(unique(cases[,2]))[,.(FID=V1)]
controls_ids <- as.data.table(unique(controls[,1]))

case_covs <- inner_join(case_ids, covariates, by="FID")
controls_covs <- inner_join(controls_ids, covariates, by="FID")

table(case_covs$Class,case_covs$SequencedGender)
table(controls_covs$Class,controls_covs$SequencedGender)

#Create .pheno files
cases_ids <- case_ids[, .(FID, si_sb = 1)]
controls_ids <- controls_ids[, .(FID, si_sb = 0)]
pheno <- rbind(cases_ids, controls_ids)[, .(FID, IID = FID, si_sb)]

#Merge pheno and covs
merged <- inner_join(pheno, covariates, by = "FID")
#(merged %>% select(FID, SequencedGender, Class) %>% inner_join(total_data, by = "FID"))

#Create output directory
out_dir <- "pheno_files"
dir.create(out_dir, showWarnings = FALSE)

#Helper function
write_pheno_and_keep <- function(data, prefix) {
  pheno_file <- file.path(out_dir, paste0(prefix, ".pheno"))
  keep_file  <- file.path(out_dir, paste0(prefix, ".keep"))
  #Write .pheno (FID, IID, bip)
  #fwrite(data[, .(FID, IID, si_sb)], pheno_file, sep = "\t", col.names = TRUE)
  #Write .keep (FID, IID only)
  #fwrite(data[, .(FID, IID)], keep_file, sep = "\t", col.names = FALSE)
  
  #Calculate summary
  n_total <- nrow(data)
  n_cases <- sum(data$si_sb == 1, na.rm = TRUE)
  n_ctrls <- sum(data$si_sb == 0, na.rm = TRUE)
  
  return(data.frame(
    File = prefix,
    Total = n_total,
    Cases = n_cases,
    Controls = n_ctrls
  ))
}

#Create files and summary
summary_list <- list()

#All individuals
summary_list[["ALL"]] <- write_pheno_and_keep(merged, "si_sb_ALL")

#Save summary
summary_df <- do.call(rbind, summary_list)
fwrite(summary_df, file.path(out_dir, "pheno_summary.txt"), sep = "\t")

#Save Excel file
wb <- createWorkbook()
addWorksheet(wb, "Phenotype Summary")
writeDataTable(wb, "Phenotype Summary", summary_df)
addStyle(wb, "Phenotype Summary",
         style = createStyle(textDecoration = "bold", halign = "center", border = "bottom"),
         rows = 1, cols = 1:ncol(summary_df), gridExpand = TRUE)
saveWorkbook(wb, file.path(out_dir, "pheno_summary.xlsx"), overwrite = TRUE)

#---------------- Some hard coded testing lol 
total_data_covs <- total_data %>% merge(covs, by = "FID")

total_data <- rbind(
  (msm_0826 %>% select(-b_sa, -na_sa,-si)
   #(), -any_nsa, -any_si_only_cleaned_controls) 
   %>%
     mutate(CURRENT_AGE_2024 = 0)), 
  (prior_population %>% filter(FID %notin% unique(msm_0826$FID)) %>%
     select(FID, si_icd, nsa_icd, any_icd, any_sui, 
            any_si_cleaned_controls, any_sa, any_sa_cleaned_controls, 
            any_si_only_cleaned_controls, any_si, CURRENT_AGE_2024)))

chisq.test(table(total_data_covs$gender, total_data_covs$any_sa))
chisq.test(table(total_data_covs$gender, total_data_covs$any_si))
# chisq.test(table(total_data_covs$gender, total_data_covs$nsa_icd))

wilcox.test(CURRENT_AGE_2026 ~ any_sa, data = total_data_covs)
wilcox.test(CURRENT_AGE_2026 ~ any_si, data = total_data_covs)
# wilcox.test(CURRENT_AGE_2026 ~ any_sa, data = total_data)

#---------------- Random Plots

gf_histogram(~ sa_status, data = continued_cases, stat = "count", color = ~ sa_status, fill = ~sa_status) %>%
  gf_labs(x = "Broad SA Status", y = "Count", title = "Patient SA Status Follow Up") %>%
  gf_text(stat = "count", label = ~ after_stat(count), vjust = -0.5)

ggplot(continued_cases, aes(x = sa_status, color = sa_status, fill = sa_status)) +
  geom_bar() +
  labs(
    x = "SA Status",
    y = "Count",
    title = "Patient SA Status Follow Up"
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5
  )

ggplot(continued_cases, aes(x = si_status, color = si_status, fill = si_status)) +
  geom_bar() +
  labs(
    x = "SI Status",
    y = "Count",
    title = "Patient SI Status Follow Up"
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5
  )

gf_histogram(~ si_status, stat = "count", data = continued_cases, color = ~ si_status, fill = ~si_status) %>%
  gf_labs(x = "SI Status", y = "Count", title = "Patient SI Status Follow Up") %>%
  gf_text(stat = "count", label = ~ after_stat(count), vjust = -0.5)


ggplot(continued_cases, aes(x = nsa_status, color = nsa_status, fill = nsa_status)) +
  geom_bar() +
  labs(
    x = "SA ICD Status",
    y = "Count",
    title = "Patient SA Status Follow Up"
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5
  )
gf_histogram(~ nsa_status, stat = "count", data = continued_cases, color = ~ nsa_status, fill = ~nsa_status) %>%
  gf_labs(x = "NSA Status", y = "Count", title = "Patient NSA Status Follow Up") %>%
  gf_text(stat = "count", label = ~ after_stat(count), vjust = -0.5)

ggplot(total_data, aes(x = as.factor(any_si_cleaned_controls), fill = as.factor(any_si_cleaned_controls))) +
  geom_bar() +
  labs(
    x = "SI Totals",
    y = "Count",
    title = "MSM 0826 SI Counts"
  ) +
  scale_fill_discrete(labels = c("N_SA_Control", "Control", "SI")) + 
  labs(fill = "SI Groups") + 
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5
  ) 
