############################################################
# Dependencies
############################################################
suppressPackageStartupMessages({
    library(GGally)
    library(scales)
    library(cowplot)
    library(patchwork)
})

############################################################
# Utilities
############################################################
factorize_and_translate <- function(values_to_factorize, translation_key){
    pmap(
        .l=values_to_factorize, # column in mutate call
        .f=~ 
            ifelse(
                .x %in% names(translation_key),
                translation_key[[.x]],
                .x
            )
    ) %>%
    factor(levels=unname(unlist(translation_key))) # factor levels are same as list order of key
}

factorize_demographics_data <- function(demographics_df) {
    demographics_df %>% 
    mutate(
        gender=
            factorize_and_translate(
                values_to_factorize=list(gender), 
                translation_key=list('F'='Female', 'M'='Male', 'I'='Intersex', 'U'='Unknown')
            ),
        is_religious=
            factorize_and_translate(
                values_to_factorize=list(as.character(is_religious)),  # numeric 0/1
                translation_key=list('0'='not religious', '1'='religious')
            ),
        is_married=
            factorize_and_translate(
                values_to_factorize=list(as.character(is_married)),  # numeric 0/1
                translation_key=list('0'='not married', '1'='married')
            ),
        is_relationship=
            factorize_and_translate(
                values_to_factorize=list(as.character(is_relationship)),  # numeric 0/1
                translation_key=list('0'='single', '1'='serious partner')
            ),
        is_deceased=
            factorize_and_translate(
                values_to_factorize=list(as.character(is_deceased)),  # bool
                translation_key=list('FALSE'='Alive', 'TRUE'='Deceased')
            ),
        is_cisgender=
            factorize_and_translate(
                values_to_factorize=list(as.character(is_cisgender)),  # bool
                translation_key=list('TRUE'='cisgender', 'FALSE'='not cisgender')
            ),
        # unify different unknowns
        ethnicity=ifelse(str_detect(ethnicity, 'UNKNOWN'), 'Unknown', ethnicity) %>% factor()
    )
}
