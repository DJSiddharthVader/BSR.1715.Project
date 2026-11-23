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
# Clean up individual fiels
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

load_encounter_data <- function(force_redo=FALSE, ...) {
    check_cached_results(
        results_file=CLEAN_ENCOUNTER_DATA_FILE,
        force_redo=force_redo,
        results_fnc=clean_encounter_diagnosis,
        ...
    )
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

load_medical_history <- function(force_redo=FALSE, ...) {
    check_cached_results(
        results_file=CLEAN_MEDICAL_HISTORY_DATA_FILE,
        force_redo=force_redo,
        results_fnc=clean_medical_history,
        ...
    )
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

load_problems_data <- function(force_redo=FALSE, ...) {
    check_cached_results(
        results_file=CLEAN_PROBLEM_LIST_DATA_FILE,
        force_redo=force_redo,
        results_fnc=clean_problem_list,
        ...
    )
}

############################################################
# Handle Case Counting per patient
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
        ~patient_outcome, ~ICD_code_list,
        "SI",             SI_codes,
        "Narrow SA",      narrow_SA_codes,
        "Broad SA",       broad_SA_codes,
        "Alcohol UD",     alcohol_UD_codes,
        "Substance UD",   substance_UD_codes
    )
}

compute_patient_outcomes <- function(
    patient_codes,
    phenotype_code_mappings_df,
    ...){
    # First compute all binary phenotypes for all pre-specifed ICD codes
    history_start <-  min(patient_codes$encounter_date)
    history_length <- time_length(max(patient_codes$encounter_date) - history_start, unit='seconds')
    # history_start; history_length
    patient_codes %>%
    # only keep codes asssigned to patient AND defining a phenotype of interest
    left_join(
        phenotype_code_mappings_df %>% mutate(patient_outcome=factor(patient_outcome)),
        by=join_by(ICD_code == ICD_code_list)
    ) %>% 
    # count(patient_outcome) %>% # how many code assignments per outcome
    # # get earliest point where a phenotype term was found
    # then compute where that term is in their medical history i.e. 
    # (diagnosis_date - history_start) / history_length to put in [0,1] range, 0 == early 1 == late
    group_by(patient_outcome) %>% 
    summarize(
        n=n(),
        earliest_diagnosis=
            time_length(-1 + min(encounter_date) - history_start, unit='seconds') / history_length
    ) %>%  
    # the -1 offset tells us when it is null vs the diagnosis is actually on the first day
    mutate(earliest_diagnosis=ifelse(earliest_diagnosis < 0, NA, earliest_diagnosis)) %>% 
    complete(
        patient_outcome,
        fill=list(n=0)
    ) %>% # make outcomes with 0 matching codes explicit
    filter(!is.na(patient_outcome)) %>%   # remove empty case
    # binarize and pivot to M binary columns for all M phenotypes
    mutate(
        case_control_status=
            ifelse(n > 0, 'case', 'control') %>% 
            factor(levels=c('control', 'case'))
    ) %>% 
    select(
        patient_outcome,
        earliest_diagnosis,
        case_control_status
    )
}

summarize_patient_history <- function(
    patient_codes,
    phenotype_code_mappings_df,
    ...){
    # patient_codes <- tmp$patient_codes[[1]]
    # Get binary phenotypes for patient i.e. have they ever had a code for SI, SA, etc.
    patient_outcomes_binary_df <- 
        compute_patient_outcomes(
            patient_codes,
            phenotype_code_mappings_df
        )
    # Compute some descriptive features from their total history of code assignments
    history_features_df <- 
        patient_codes %>%
        add_count(ICD_code) %>% 
        arrange(desc(n)) %>% 
        summarize(
            n_codes=n(),
            n_unique_codes=n_distinct(ICD_code),
            n_visits=n_distinct(encounter_date),
            history_length=
                difftime(max(encounter_date), min(encounter_date))  %>%
                time_length(unit='days')
        ) %>%
        mutate(freq_ICD=n_codes / history_length)
    # Compute cumulative code distrivutions over time
    codes_cummulative_df <- 
        patient_codes %>%
        select(ICD_code, encounter_date) %>% 
        nest(ICD_codes=c(ICD_code)) %>% 
        arrange(encounter_date) %>% 
        mutate(
            cumulative_total_codes=
                accumulate(
                    ICD_codes,
                    # \(so_far, current) unlist(c(so_far, current))
                    \(so_far, current) bind_rows(so_far, current) 
                ),
            cumulative_unique_codes=
                accumulate(
                    ICD_codes,
                    \(so_far, current) bind_rows(so_far, current) %>% distinct()
                )
        ) %>% 
        mutate(
            across( 
                c(ICD_codes, starts_with('cumulative_')), 
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
        # history_shape_features_df,
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
    unnest(patient_code_stats) %>% 
    # Define alternate outcome encodings from lorenza original logic
    mutate(
        si_icd = case_when( si > 0 ~ 1),
        nsa_icd = case_when( na_sa > 0 ~ 1),
        any_icd = 1, 
        any_sui = case_when( na_sa > 0 | b_sa > 0 ~ 1), 
        any_sa = case_when( na_sa > 0 ~ 1 ),
        any_si = case_when( si > 0 ~ 1),
        any_si_cleaned_controls = case_when( any_si > 0 ~ 1, b_sa > 0 | any_sa > 0 ~ -9 ), 
        any_sa_cleaned_controls = case_when( na_sa > 0 ~ 1, si > 0 ~ -9),
        any_si_only_cleaned_controls = case_when( si > 0 & b_sa == 0 & na_sa == 0 ~ 1, b_sa != 0 | na_sa != 0 ~ -9),
        # any_nsa = case_when(na_sa > 0 ~ 1)
    ) %>%
    mutate(across(everything(), ~replace_na(.x, 0)))
}

load_all_summarized_patient_histories <- function(force_redo=FALSE, ...){
    check_cached_results(
        results_file=CLEAN_ALL_DATA_FILE,
        force_redo=force_redo,
        results_fnc=combine_all_data,
        ...
    )
}

