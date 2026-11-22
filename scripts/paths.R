PROJECT_ROOT                    <- here()                       
INPUT_DATA_DIR                  <- here("input.data")           
# Raw data                        
RAW_MSM_DATA_DIR                <- here("msm.data")             
ENCOUNTER_DIAGNOSIS_FILE        <- file.path(RAW_MSM_DATA_DIR, "Encounter_Diagnosis.txt")
PROBLEMS_FILE                   <- file.path(RAW_MSM_DATA_DIR, "Problem_List.txt")
MEDICAL_HISTORY_FILE            <- file.path(RAW_MSM_DATA_DIR, "Medical_History.txt")
DEMOGRAPHICS_FILE               <- file.path(RAW_MSM_DATA_DIR, "Demographics.txt")
PATIENT_COVARIATES_DATA_FILE    <-  "/sc/arion/projects/MSM/data/phenotypes/2025-07-08/SINAI-Million-BioMe1_Freeze_One.covariates.tsv"
# Results
# RESULTS_DIR                     <- here('results')             
RESULTS_DIR                     <- 'sc/arion/scratch/reeds05/BSR.1715.Project/'
CLEAN_DATA_DIR                  <- file.path(RESULTS_DIR, 'clean.patient.data')
CLEAN_DEMOGRAPHICS_DATA_FILE    <- file.path(CLEAN_DATA_DIR, 'clean.demograhics.tsv')
# CLEAN_ENCOUNTER_DATA_FILE       <- file.path(CLEAN_DATA_DIR, 'clean.encounters.tsv')
# CLEAN_MEDICAL_HISTORY_DATA_FILE <- file.path(CLEAN_DATA_DIR, 'clean.medical.history.tsv')
# CLEAN_PROBLEM_LIST_DATA_FILE    <- file.path(CLEAN_DATA_DIR, 'clean.problems.tsv')
CLEAN_ALL_DATA_FILE             <- file.path(CLEAN_DATA_DIR, 'all.clean.data.tsv')
# SA Codes
BROAD_SA_CODES_FILE             <- file.path(INPUT_DATA_DIR, 'broad_sa_ICD.txt')
NARROW_SA_CODES_FILE            <- file.path(INPUT_DATA_DIR, 'narrow_sa_ICD.txt')
SI_CODES_FILE                   <- file.path(INPUT_DATA_DIR, 'si_ICD.txt')
