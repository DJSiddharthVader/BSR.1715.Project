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
    mutate(
        across(
            c(dob, dod),
            as.Date,
            format="%m/%d/%Y"
        )
    ) %>% 
    mutate(
        curent_age=
            case_when(
                is.na(dod) ~ floor(as.numeric(Sys.Date() - set_2026$dob) / 365.25), 
                !is.na(dod) ~ floor(as.numeric(set_2026$dod - set_2026$dob) / 365.25)
            ), 
        is_deceased=!is.na(dod), 
        is_religious= 
            case_when(
                religion %in% RELIGIONS_LABELS ~ 1, 
                religion %in% NON_RELIGIOS_LABELS ~ 0, 
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
    pivot_longer(c(current_icd9_list, current_icd10_list), 
                 names_pattern = "current_(icd\\d+)_list", 
                 names_to = "dx_code_type", values_to = "ICD_code") %>%
    separate_longer_delim(ICD_code, delim = ",") %>%
    mutate(ICD_code = str_trim(ICD_code), 
           data_src = "MedHist") %>%
    filter(!is.na(ICD_code), ICD_code != "") %>%
    distinct()
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
# Combine data sources together
############################################################
load_patient_covariates <- function(){
    PATIENT_COVARIATES_DATA_FILE %>% 
    read_tsv(show_col_types=FALSE) %>%
    dplyr::rename('Ethnicity'=Class) %>% 
    select(
        FID,
        SelfIdentifiedGender,
        SequencedGender,
        HM3_population,
        Ethnicity
    ) %>%
    mutate(is_cis= (SelfIdentifiedGender == SequencedGender))
}

load_SA_codes <- function(){
    si_codes <- 
        fread(SI_CODES_FILE, header=FALSE)[,.(ICD_code=V1)]
    si_codes$ICD_group <- "si"
    na_sa_codes <- 
        fread(NARROW_SA_CODES_FILE, header=FALSE)[,.(ICD_code=V1)]
    na_sa_codes$ICD_group <- "na_sa"
    b_sa_codes <- 
        fread(BROAD_SA_CODES_FILE, header = FALSE)[,.(ICD_code=V1)] %>%
        filter(ICD_code %notin% unique(na_sa_codes$ICD_code))
    b_sa_codes$ICD_group <- "b_sa"

    rbind(si_codes, na_sa_codes, b_sa_codes)
}

combine_all_data <- function(){
    # encounters_df <- load_encounter_data()
    # problems_df <- load_problems_data()
    # medical_history_df <- load_medical_history()
    encounters_df <- clean_encounter_diagnosis()
    problems_df <- clean_problem_list()
    medical_history_df <- clean_medical_history()
    include_codes <- load_SA_codes()
    bind_rows(
        encounters_df,
        problems_df,
        medical_history_df
    ) %>% 
    left_join(
        load_patient_covariates(),
        by='FID'
    ) %>% 
    left_join(
        clean_demographics_data(),
        by='FID'
    ) %>% 
    mutate(ICD_code=trimws(as.character(ICD_code))) %>%
    distinct(
        FID,
        ICD_code,
        encounter_date,
        .keep_all=TRUE
    ) %>% 
    group_by(FID) %>%
    mutate(
        has_included=
            ifelse(
                ICD_code %in% unique(include_codes$ICD_code),
                "case",
                "control"
            )
    ) %>%
    ungroup()
}

load_all_combined_data <- function(force_redo=FALSE, ...){
    check_cached_results(
        results_file=CLEAN_ALL_DATA_FILE,
        force_redo=force_redo,
        results_fnc=combine_all_data,
        ...
    )
}

dedup_cases <- function(
