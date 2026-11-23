PROJECT_ROOT                    <- here()                       
# Raw data                        
RAW_MSM_DATA_DIR                <- here("msm.data")             
ENCOUNTER_DIAGNOSIS_FILE        <- file.path(RAW_MSM_DATA_DIR, "Encounter_Diagnosis.txt")
PROBLEMS_FILE                   <- file.path(RAW_MSM_DATA_DIR, "Problem_List.txt")
MEDICAL_HISTORY_FILE            <- file.path(RAW_MSM_DATA_DIR, "Medical_History.txt")
DEMOGRAPHICS_FILE               <- file.path(RAW_MSM_DATA_DIR, "Demographics.txt")
PATIENT_COVARIATES_DATA_FILE    <-  "/sc/arion/projects/MSM/data/phenotypes/2025-07-08/SINAI-Million-BioMe1_Freeze_One.covariates.tsv"
# SA Codes
INPUT_DATA_DIR                  <- here("input.data")           
BROAD_SA_CODES_FILE             <- file.path(INPUT_DATA_DIR, 'broad_sa_ICD.txt')
NARROW_SA_CODES_FILE            <- file.path(INPUT_DATA_DIR, 'narrow_sa_ICD.txt')
SI_CODES_FILE                   <- file.path(INPUT_DATA_DIR, 'si_ICD.txt')
# Clean patient data
CLEAN_DEMOGRAPHICS_DATA_FILE    <- file.path(INPUT_DATA_DIR, 'clean.patient.demograhics.tsv')
CLEAN_OUTCOMES_DATA_FILE <-  file.path(INPUT_DATA_DIR, 'clean.patient.outcomes.tsv')
CLEAN_FEATURIZED_HISTORY_DATA_FILE <-  file.path(INPUT_DATA_DIR, 'clean.patient.history.features.tsv')
