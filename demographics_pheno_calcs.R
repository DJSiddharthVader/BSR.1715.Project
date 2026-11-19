# Phenotype calculations rq 
library(data.table)
library(tidyverse)
library(dplyr)

set_2026 <- fread("/sc/arion/projects/MSM/data/phenotypes/2026-08-12/Clarity/Demographics.txt", 
                  select = c("sample_name", "gender", "religion",
                             "marital_status", "patient_race",
                             "birth_date", "death_date")) %>%
                  rename(FID = sample_name)

set_2026$dob <- as.Date(set_2026$birth_date, format = "%m/%d/%Y")
set_2026$dod <- as.Date(set_2026$death_date, format = "%m/%d/%Y")

# Religiosity 
## make list of religions
# Ones im like erm . do we include bc cultural vs actural organized religion? 
# NATIVE AMERICAN TRADITIONS, WICCA, HUMANIST, 
religious <- c("AFRICAN DIASPORA TRADITIONS", "BAHÁ'Í", "BAPTIST", "BUDDHIST", "CATHOLIC", 
               "CHRISTIAN","EVANGELICAL", "HINDU", "JAIN","JEHOVAH WITNESS", "JEWISH", 
               "LATTER-DAY SAINTS/MORMON", "MUSLIM", "NATIVE AMERICAN TRADITIONS", 
               "ORTHODOX CHRISTIAN", "PAGAN/WICCAN", "PENTECOSTAL", "PROTESTANT",
               "SEVENTH DAY ADVENTIST","SHINTO", "SIKH")
unknown <- c("UNKNOWN/UNABLE TO OBTAIN", "PATIENT DECLINED", "OTHER", "Unknown", "Other", "PT Declined")
non_religious <- c("UNAFFILIATED/NONE", "None", "ATHEIST", "HUMANIST", "SPIRITUAL, NOT RELIGIOUS")

# Marital Status
# lists of marital statuses
# here we could shift because we have like divorced, widowed to add a
# component of change but like we dk when that happened saur 
#sticking to either unknown, married, relationship, or single for everyone else
married <- c("MARRIED/CIVIL UNION")
sign_other <- c("MARRIED/CIVIL UNION", "SIGNIFICANT OTHER/LIFE PARTNER")
unk <- c("UNKNOWN/OTHER")

demographic_phenotypes <- set_2026 %>% mutate(CURRENT_AGE_2026 = case_when(
                is.na(dod) ~ floor(as.numeric(Sys.Date() - set_2026$dob) / 365.25), 
                !is.na(dod) ~ floor(as.numeric(set_2026$dod - set_2026$dob) / 365.25)
              ), deceased = !is.na(dod), 
              religiosity = case_when(
                religion %in% religious ~ 1, 
                religion %in% non_religious ~ 0, 
                religion %in% unknown ~ NA), 
              married_yn = case_when(
                marital_status %in% married ~ 1, 
                marital_status %in% unk ~ NA, 
                marital_status %notin% married ~0
              ), 
              relationship = case_when(
                marital_status %in% sign_other ~ 1, 
                marital_status %in% unk ~ NA,
                marital_status %notin% sign_other ~ 0
              )) %>%
              select(FID, gender, CURRENT_AGE_2026, religiosity,
                     married_yn, relationship, patient_race, deceased)
              
filename <- "/sc/arion/projects/mscic1/mullins_lab_biome_projs/lorenza/files/case_controls/demographic_covs.tsv"
fwrite(demographic_phenotypes, filename)