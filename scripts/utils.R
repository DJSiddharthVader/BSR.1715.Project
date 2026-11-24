############################################################
# Dependencies
############################################################
suppressPackageStartupMessages({
    library(data.table)
    library(tidyverse)
    library(magrittr)
    library(tictoc)
    library(glue)
    library(forcats)
    library(lubridate)
    library(furrr)
})

############################################################
# Utilities
############################################################
`%notin%` <- Negate(`%in%`)

check_cached_results <- function(
    results_file,
    force_redo=FALSE,
    return_data=TRUE,
    silence=FALSE,
    p=NULL,
    results_fnc,
    ...){
    # Now check if the results file exists and load it
    tic()
    if (is.null(results_file)) {
        if (silence) { 
            results <- invisible(results_fnc(...))
        } else {
            message("No results file, will just return data") 
            results <- results_fnc(...)
        }
        return_data <- TRUE
    } else {
        output.filetype <- results_file %>% str_extract('\\.[^\\.]*$') 
        if (!silence) { message(output.filetype) }
        if (output.filetype == '.rds') {
            load_fnc <- readRDS
            save_fnc <- saveRDS
        } else if (output.filetype %in% c('.txt', '.tsv')) {
            load_fnc <- read_tsv
            save_fnc <- write_tsv
        } else {
            stop(glue('Invalid file extesion: {output.filetype}'))
        }
        # silence functions is specified
        if (silence) {
            load_fnc <- quietly(load_fnc)
            save_fnc <- quietly(save_fnc)
        }
        if (file.exists(results_file) & !force_redo) {
            if (!silence) { message(glue('{results_file} exists, not recomputing results')) }
            if (return_data) {
                if (!silence) { message('Loading cached results...') }
                results <- load_fnc(results_file)
            }
        # or force recompute+cache the data and return it
        } else {
            if (file.exists(results_file) & force_redo) {
                if (!silence) { message(glue('{results_file} exists, recomputing results anyways')) }
            } else {
                if (!silence) { message(glue('No cached results, generating: {results_file}')) }
            }
            dir.create(dirname(results_file), recursive=TRUE, showWarnings=FALSE)
            if (silence) { 
                # results <- invisible(results_fnc(...))
                results <- invisible(results_fnc(...)) %T>% save_fnc(results_file)
            } else {
                results <- results_fnc(...) %T>% save_fnc(results_file)
            }
            # If results dont exist or force_redo is TRUE compute + cache results
            # Assumes save_fnc is of fomr save_fnc(result_object, filename)
        }
    }
    if (!is.null(p)) { p() } # update progress bar if specified
    # Or dont save the results, just return the results
    if (!silence) { toc() }
    if (return_data) {
        return(results)
    } else { 
        return(invisible(NULL))
    }
}

############################################################
# Load patient demographics + histories
############################################################
clean_demographics_data <- function() {
    DEMOGRAPHICS_FILE %>%
    read_delim(
        show_col_types=FALSE,
        delim='|',
        col_select=
            c(
                "sample_name",
                "gender",
                "religion",
                "marital_status",
                "patient_race",
                "birth_date",
                "death_date"
            )
    ) %>% 
    dplyr::rename('dob'=birth_date, 'dod'=death_date) %>% 
    mutate(
        across(
            .cols=c(dob, dod),
            .fn=~ as.Date(.x, format="%m/%d/%Y")
        )
    ) %>% 
    mutate(
        current_age=
            case_when(
                is.na(dod) ~ floor(as.numeric(Sys.Date() - dob) / 365.25), 
                !is.na(dod) ~ floor(as.numeric(dod - dob) / 365.25)
            ), 
        is_deceased=!is.na(dod), 
        is_religious= 
            case_when(
                religion %in% RELIGIONS_LABELS ~ 1, 
                religion %in% NON_RELIGIOUS_LABELS ~ 0, 
                religion %in% UNKNOWN_LABELS ~ NA
            ), 
        is_married= 
            case_when(
                marital_status %in% MARRIED_LABELS ~ 1, 
                marital_status %in% UNKNOWN_LABELS ~ NA, 
                marital_status %notin% MARRIED_LABELS ~0
            ), 
        is_relationship=
            case_when(
                marital_status %in% SIGNIFICANT_OTHER_LABELS ~ 1, 
                marital_status %in% UNKNOWN_LABELS ~ NA,
                marital_status %notin% SIGNIFICANT_OTHER_LABELS ~ 0
            )
    ) %>%
    dplyr::rename('FID'=sample_name) %>% 
    select(
        FID,
        gender,
        current_age,
        is_deceased,
        is_religious,
        is_married,
        is_relationship
    ) %>%
    left_join(
        PATIENT_COVARIATES_DATA_FILE %>% 
        read_tsv(show_col_types=FALSE) %>%
        # seems like a reasonable definition for this dataset if we want to analyze
        mutate(is_cisgender=(SelfIdentifiedGender == SequencedGender)) %>% 
        dplyr::rename('ethnicity'=Class) %>% 
        select(
            FID,
            # SelfIdentifiedGender,
            # SequencedGender,
            is_cisgender,
            # HM3_population,
            ethnicity
        ),
        by='FID'
    )
}

load_demographics_data <- function(force_redo=FALSE, ...) {
    check_cached_results(
        results_file=CLEAN_DEMOGRAPHICS_DATA_FILE,
        force_redo=force_redo,
        results_fnc=clean_demographics_data,
        ...
    )
}

clean_encounter_diagnosis <- function() {
    ENCOUNTER_DIAGNOSIS_FILE %>% 
    read_delim(
        show_col_types=FALSE,
        delim='|',
        col_select=
            c(
                "sample_name",
                "dx_code",
                "dx_code_type",
                "dx_name",
                "encounter_date"
            )
    ) %>% 
    rename(
        'FID'=sample_name,
        'ICD_code'=dx_code
    ) %>%
    add_column(data_src="EncounterDiagnosis") %>%
    distinct() 
}

clean_medical_history <- function() {
    MEDICAL_HISTORY_FILE %>% 
    read_delim(
        show_col_types=FALSE,
        delim='|',
        col_select=
            c(
                "sample_name",
                "current_icd9_list",
                "current_icd10_list",
                "MEDICAL_HX_DESCRIPTION",
                "encounter_date"
            )
    ) %>% 
    rename(
        'FID'=sample_name,
        'dx_name'=MEDICAL_HX_DESCRIPTION
    ) %>%
    # pivot all codes to tidy format
    pivot_longer(
        c(current_icd9_list, current_icd10_list), 
        names_pattern = "current_(icd\\d+)_list", 
        names_to = "dx_code_type",
        values_to = "ICD_code"
    ) %>%
    separate_longer_delim(ICD_code, delim = ",") %>%
    mutate(ICD_code = str_trim(ICD_code)) %>% 
    # only keep non-empty code assignments
    filter(!is.na(ICD_code), ICD_code != "") %>%
    distinct() %>%
    add_column(data_src="MedicalHistory")
}

clean_problem_list <- function(){
    PROBLEMS_FILE %>% 
    read_delim(
        show_col_types=FALSE,
        delim='|',
        col_select=
            c(
                "sample_name",
                "icd9_code",
                "code_type",
                "dx_name",
                "encounter_date"
            )
    ) %>% 
    rename(
        'FID'=sample_name,
        'ICD_code'=icd9_code,
        'dx_code_type'=code_type
    ) %>%
    add_column(data_src="ProblemList") %>%
    distinct()
}

clean_and_combine_all_pateint_histories <- function(){
    # get all patient-ICD code assignments from diagnoses/history
    bind_rows(
        # load_encounter_data(),
        # load_problems_data(),
        # load_medical_history()
        clean_encounter_diagnosis(),
        clean_problem_list(),
        clean_medical_history()
    ) %>%  
    # 91M total code assignments
    mutate(ICD_code=trimws(as.character(ICD_code))) %>%
    mutate(encounter_date=lubridate::mdy(encounter_date)) %>% 
    # mutate(encounter_date=as.Date(encounter_date)) %>% 
    # Remove empty/duplicate entries of ICD codes 
    filter(!is.na(ICD_code), ICD_code != "") %>%
    distinct(
        ICD_code,
        FID,
        encounter_date,
        .keep_all=TRUE
    ) %>%  
    # 82M unique code assignments across all patients and dates after filtering
    select(-c(data_src))
}

load_all_patient_histories <- function(force_redo=FALSE, ...){
    check_cached_results(
        results_file='/sc/arion/scratch/reeds05/all_patient_histories.tsv',
        force_redo=force_redo,
        # force_redo=TRUE,
        results_fnc=clean_and_combine_all_pateint_histories,
        ...
    ) %>% 
    mutate(encounter_date=lubridate::ymd(encounter_date)) %>% 
    # for each patient, collapse all code assignment into nested tibbles, 1 row per patient
    nest(patient_codes=c(ICD_code, dx_code_type, dx_name, encounter_date))
}

############################################################
# Compute patient outcomes
############################################################
load_ICD_Phenotype_Mappings <- function(){
    SI_codes <- 
        SI_CODES_FILE %>%
        read_tsv(show_col_types=FALSE, col_names=FALSE) %>% pull(X1) %>% unique()
    narrow_SA_codes <- 
        NARROW_SA_CODES_FILE %>% 
        read_tsv(show_col_types=FALSE, col_names=FALSE) %>% pull(X1) %>% unique()
    broad_SA_codes <- 
        BROAD_SA_CODES_FILE %>% 
        read_tsv(show_col_types=FALSE, col_names=FALSE) %>% 
        filter(X1 %notin% SI_codes) %>% pull(X1) %>% unique()
    # ICD code definitions for use disorders
    # https://codingclarified.com/icd-10-coding-for-substance-use-disorders/
    # F10: Alcohol-related disorders
    # F10.10 – Alcohol use disorder, mild
    # F10.20 – Alcohol use disorder, moderate
    # F10.21 – Alcohol use disorder, severe
    # F10.239 – Alcohol use disorder with withdrawal, unspecified
    alcohol_UD_codes  <- c('F10', 'F10.10', 'F10.20', 'F10.21', 'F10.239')
    # F11: Opioid-related disorders
    # F14: Cocaine-related disorders
    # F15: Other stimulant-related disorders
    # F16: Hallucinogen-related disorders
    # F18: Inhalant-related disorders
    # F11.21 – Opioid dependence with withdrawal
    substance_UD_codes <- c('F11', 'F14', 'F15', 'F16', 'F18', 'F11.21')
    # tibble, every row is a possible patient outcome
    # a patient has an outcome if they match >= 1 code from each corresponding list
    # can be unnested for joining/mapping to patient ICD frequencies
    tribble(
        ~outcome.type, ~ICD_code_list,
        "SI",             SI_codes,
        "Narrow_SA",      narrow_SA_codes,
        "Broad_SA",       broad_SA_codes,
        "Alcohol_UD",     alcohol_UD_codes,
        "Substance_UD",   substance_UD_codes
        # "Narrow SA",      narrow_SA_codes,
        # "Broad SA",       broad_SA_codes,
        # "Alcohol UD",     alcohol_UD_codes,
        # "Substance UD",   substance_UD_codes
    ) %>% 
    mutate(outcome.type=factor(outcome.type))
}

compute_patient_outcomes <- function(
    patient_codes,
    phenotype_code_mappings_df,
    ...){
    # patient_codes=tmp$patient_codes[[1]]; phenotype_code_mappings_df=load_ICD_Phenotype_Mappings() %>% unnest(ICD_code_list)
    history_start <-  min(patient_codes$encounter_date)
    history_length <- time_length(max(patient_codes$encounter_date) - history_start, unit='seconds')
    # Compute all binary phenotypes for all pre-specifed ICD codes
    patient_codes %>%
    # only keep codes asssigned to patient AND defining a phenotype of interest
    left_join(
        phenotype_code_mappings_df,
        by=join_by(ICD_code == ICD_code_list)
    ) %>% 
    group_by(outcome.type) %>% 
    summarize(
        # outcome "severity"/longevity?
        n_codes=n(),                
        # number of unique codes for same outcome, "severity"?
        n_unique_codes=n_distinct(ICD_code), 
        # Get earliest point in history where a outcome-associated code was assigned i.e.
        # => [0,1] range, 0 == earliest, 1 == latest, NA they are a control
        earliest_diagnosis=
            {-1 + min(encounter_date) - history_start} %>%
            time_length(unit='seconds') %>%
            {. / history_length} %>%
            {ifelse(. < 0, NA, .)}
    ) %>%  
    complete(outcome.type, fill=list(n=0)) %>% # make outcomes with 0 matching codes explicit
    filter(!is.na(outcome.type)) %>%   # remove empty case
    # binarize and pivot to M binary columns for all M phenotypes
    mutate(case_control_binary=ifelse(is.na(n_codes), FALSE, n_codes > 0)) %>% 
    select(
        outcome.type,        # what the outcome is 
        case_control_binary, # patient-specific outcome (0/1)
        earliest_diagnosis,  # if case -> normalized time they first had it
        n_codes,             # total number of codes matching the case definition
        n_unique_codes       # unique number of codes matching the case definition
    )
}

compute_all_patient_outcomes <- function(){
    # for every patient, see which relevant phenotypes it has
    direct_code_outcomes <- 
        load_all_patient_histories() %>% 
        # Get binary phenotypes for patient i.e. have they ever had a code for SI, SA, etc.
        mutate(
            patient_outcomes=
                future_pmap(
                    .l=.,
                    .f=compute_patient_outcomes,
                    phenotype_code_mappings_df=
                        load_ICD_Phenotype_Mappings() %>% 
                        unnest(ICD_code_list),
                    .progress=TRUE
                )
        ) %>%
        select(-c(patient_codes)) %>%
        unnest(patient_outcomes)
    # create new possible outcomes using combinations of SI, Broad_SA and Narrow_SA
    indirect_code_outcomes <- 
        direct_code_outcomes %>%
        # 1 binary column for each  outcome
        select(FID, outcome.type, case_control_binary) %>% 
        pivot_wider(names_from=outcome.type, values_from=case_control_binary) %>% 
        # Define alternate outcome encodings from lorenza's original logic
        # So define cases/controls explicitly and exclude any samples with NA in the outcome
        mutate(
            # Any outcome
            any_ICD =
                case_when(
                     SI | ( Broad_SA |  Narrow_SA) ~ 'case',
                    !SI & (!Broad_SA & !Narrow_SA) ~ 'control',
                    .default=NA
                ),
            # Only thoughts
            only_SI =
                case_when(
                     SI & (!Broad_SA & !Narrow_SA) ~ 'case',
                    !SI & (!Broad_SA & !Narrow_SA) ~ 'control',
                    .default=NA
                ),
            # only narrow action
            only_Narrow = 
                case_when(
                    !SI & (!Broad_SA &  Narrow_SA) ~ 'case',
                    !SI & (!Broad_SA & !Narrow_SA) ~ 'control',
                    .default=NA
                ),
            # thoughts and all actions
            all_ICD =        
                case_when(
                     SI & ( Broad_SA &  Narrow_SA) ~ 'case',
                    !SI & (!Broad_SA & !Narrow_SA) ~ 'control',
                    .default=NA
                ),
            # if the person showed no Ideation before action + action severity
            only_Action = 
                case_when(
                    !SI & ( Broad_SA &  Narrow_SA) ~ 'case',
                    !SI & (!Broad_SA &  Narrow_SA) ~ 'case',
                    !SI & ( Broad_SA & !Narrow_SA) ~ 'case',
                    !SI & (!Broad_SA & !Narrow_SA) ~ 'control',
                    .default=NA
                ),
            # thoughts and some kind of action
            both_SASI= 
                case_when(
                     SI & ( Broad_SA &  Narrow_SA) ~ 'case',
                     SI & (!Broad_SA &  Narrow_SA) ~ 'case',
                     SI & ( Broad_SA & !Narrow_SA) ~ 'case',
                     SI & (!Broad_SA & !Narrow_SA) ~ 'control',
                    .default=NA
                ),
            across(where(is.logical), ~ ifelse(.x, 'case', 'control')),
        ) %>%
        # tidy format different outcomes
        pivot_longer(
            -c(FID),
            names_to='outcome.type',    # how labels are defined per patient
            values_to='case_control_status' # actual per-patient label assignments
        ) %>%
        filter(outcome.type %notin% unique(direct_code_outcomes$outcome.type)) %>% 
        # only keep outcomes for patients included in each outcome type
        filter(!is.na(case_control_status))
        # factorize all cases/controls now defined explicity
    # get direct and inffered outcomes
    direct_code_outcomes %>%
    mutate(case_control_status=ifelse(case_control_binary, 'case', 'control')) %>%
    select(-c(case_control_binary)) %>% 
    bind_rows(indirect_code_outcomes)
}

load_all_patient_outcomes <- function(force_redo=FALSE, ...){
    check_cached_results(
        results_file=CLEAN_OUTCOMES_DATA_FILE,
        force_redo=force_redo,
        results_fnc=compute_all_patient_outcomes,
        ...
    ) %>% 
    mutate(case_control_status=factor(case_control_status, levels=c('control', 'case')))
}

    )
}

        )
    # Compute some descriptive features from their total history of code assignments
    history_features_df <- 
        patient_codes %>%
        add_count(ICD_code) %>% 
        arrange(desc(n)) %>% 
        summarize(
            n_total_codes=n(),
            n_unique_codes=n_distinct(ICD_code),
            n_visits=n_distinct(encounter_date),
            history_length=
                difftime(max(encounter_date), min(encounter_date))  %>%
                time_length(unit='days')
        ) %>%
        mutate(
            mean_total_codes=n_total_codes / n_visits,
            mean_unique_codes=n_unique_codes / n_visits,
            freq_codes=n_total_codes / history_length,
            freq_unique_codes=n_unique_codes / history_length
        )
    # Compute cumulative code distrivutions over time
    codes_cummulative_df <- 
        patient_codes %>%
        select(ICD_code, encounter_date) %>% 
        # nest all codes, so 1 row per date + list of assigned codes on thta date
        nest(encounter_codes=c(ICD_code)) %>% 
        arrange(encounter_date) %>% 
        mutate(
            cumulative_total_codes=
                accumulate(
                    encounter_codes,
                    \(so_far, current) bind_rows(so_far, current) 
                ),
            cumulative_unique_codes=
                accumulate(
                    encounter_codes,
                    \(so_far, current) bind_rows(so_far, current) %>% distinct()
                )
        ) %>% 
        mutate(
            across( 
                ends_with('_codes'), 
                purrr::map_int,
                nrow
            )
        )
        # at each timepoint we have the total/unique number of codes assigned
    # Now compute "shape"/"behaviour" features from code history
    history_shape_features_df <- 
        codes_cummulative_df %>%
            ???
    # reutrn everything as a single row
    # codes_cummulative_df
    # history_shape_features_df
    # history_features_df
    # patient_outcomes_binary_df
    bind_cols(
        history_shape_features_df,
        history_features_df
    ) %>%
    cross_join(patient_outcomes_binary_df)
}

summarize_all_patient_histories <- function(){
    # map ICD codes to relvant phenotypes
    phenotype_code_mappings_df <- load_ICD_Phenotype_Mappings() %>% unnest(ICD_code_list)
    # get all patient-ICD code assignments from diagnoses/history
    bind_rows(
        # load_encounter_data(),
        # load_problems_data(),
        # load_medical_history()
        clean_encounter_diagnosis(),
        clean_problem_list(),
        clean_medical_history()
    ) %>%  
    # 91M total code assignments
    mutate(ICD_code=trimws(as.character(ICD_code))) %>%
    mutate(encounter_date=lubridate::mdy(encounter_date)) %>% 
    # mutate(encounter_date=as.Date(encounter_date)) %>% 
    # Remove empty/duplicate entries of ICD codes 
    filter(!is.na(ICD_code), ICD_code != "") %>%
    distinct(
        ICD_code,
        FID,
        encounter_date,
        .keep_all=TRUE
    ) %>%  
    select(-c(data_src)) %>% 
    # 82M unique code assignments across all patients and dates after filtering
    # for each patient, collapse all code assignment into nested tibbles, 1 row per patient
    nest(patient_codes=c(ICD_code, dx_code_type, dx_name, encounter_date)) %>% 
        {.} -> tmp; tmp 
        tmp$patient_codes[[1]]
        tmp %>% head(3) %>% 
    # for every patient, see which relevant phenotypes it has
    # and compute some features from a patients code history
    mutate(
        patient_code_stats=
            future_pmap(
                .l=.,
                .f=summarize_patient_history,
                phenotype_code_mappings_df=phenotype_code_mappings_df,
                .progress=TRUE
            )
    ) %>%
    select(-c(patient_codes)) %>%
}

load_all_patient_history_features <- function(force_redo=FALSE, ...){
    check_cached_results(
        results_file=CLEAN_FEATURIZED_HISTORY_DATA_FILE,
        force_redo=force_redo,
        results_fnc=featurize_all_patient_histories,
        ...
    )
}

