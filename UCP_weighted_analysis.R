###############################################################################
## SURVEY-WEIGHTED BRFSS CANCER-PAIN AND MATERIAL-HARDSHIP ANALYSIS
##
## Study years: 2022, 2023, and 2024
## Outcome: uncontrolled current physical pain caused by cancer or cancer
##          treatment
## Exposures: difficulty paying bills, transportation insecurity, and utility
##            insecurity
##
## This is a new analysis script. It does not source, overwrite, or otherwise
## change the original UCPscript.R file.
##
## IMPORTANT ESTIMAND NOTE:
## The Cancer Survivorship: Pain Management and Social Determinants items were
## optional BRFSS modules. The states/territories contributing complete cases
## changed by year. The weighted results therefore describe the adult,
## noninstitutionalized populations represented by participating jurisdictions;
## they should not be labeled as estimates for the entire United States.
###############################################################################


###############################################################################
## 1. CHECK AND LOAD THE REQUIRED R PACKAGES
##
## The script stops with an informative message when a required package is not
## installed. It does not install packages automatically, which keeps the
## analysis reproducible and avoids changing the user's R library.
###############################################################################

required_packages <- c(
  "haven",
  "dplyr",
  "purrr",
  "readr",
  "tibble",
  "ggplot2",
  "survey",
  "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following required package(s) before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

## Package startup messages are suppressed so that the console output focuses
## on cohort counts, model progress, and validation results.
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(haven)
  library(purrr)
  library(readr)
  library(scales)
  library(survey)
  library(tibble)
})


###############################################################################
## 2. LOCATE THE PROJECT DATA AND OUTPUT DIRECTORY
##
## The new script lives in UCP_output. The three source XPT files live one
## directory above it. Resolving paths from the script location allows the
## analysis to run from any working directory.
###############################################################################

command_line_arguments <- commandArgs(trailingOnly = FALSE)
script_argument <- grep(
  "^--file=",
  command_line_arguments,
  value = TRUE
)

if (length(script_argument) == 1) {
  script_path <- normalizePath(
    sub("^--file=", "", script_argument),
    mustWork = TRUE
  )
} else {
  ## This fallback supports interactive sourcing from RStudio.
  script_path <- normalizePath(
    file.path(getwd(), "UCP_output", "UCP_weighted_analysis.R"),
    mustWork = FALSE
  )
}

output_dir <- dirname(script_path)
project_dir <- normalizePath(file.path(output_dir, ".."), mustWork = TRUE)

xpt_paths <- c(
  "2022" = file.path(project_dir, "2022.XPT"),
  "2023" = file.path(project_dir, "2023.XPT"),
  "2024" = file.path(project_dir, "2024.XPT")
)

if (any(!file.exists(xpt_paths))) {
  missing_files <- names(xpt_paths)[!file.exists(xpt_paths)]
  stop(
    "The following BRFSS XPT file(s) are missing: ",
    paste(missing_files, collapse = ", ")
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


###############################################################################
## 3. DEFINE SOURCE VARIABLES, LABELS, AND REFERENCE CATEGORIES
##
## Only the variables needed for the analysis are read from the very large XPT
## files. This reduces memory use without changing the source data.
##
## BRFSS complex-design variables:
##   _LLCPWT = final person-level sampling weight
##   _STSTR  = sampling stratum
##   _PSU    = primary sampling unit
###############################################################################

source_variables <- c(
  "_STATE",
  "CSRVPAIN",
  "CSRVCTL2",
  "SDHBILLS",
  "SDHTRNSP",
  "SDHUTILS",
  "_AGEG5YR",
  "_SEX",
  "_INCOMG1",
  "_LLCPWT",
  "_STSTR",
  "_PSU"
)

age_labels <- c(
  "18-24",
  "25-29",
  "30-34",
  "35-39",
  "40-44",
  "45-49",
  "50-54",
  "55-59",
  "60-64",
  "65-69",
  "70-74",
  "75-79",
  "80+"
)
names(age_labels) <- as.character(1:13)

income_labels <- c(
  "Less than $15,000",
  "$15,000 to < $25,000",
  "$25,000 to < $35,000",
  "$35,000 to < $50,000",
  "$50,000 to < $100,000",
  "$100,000 to < $200,000",
  "$200,000 or more"
)
names(income_labels) <- as.character(1:7)

hardship_labels <- c(
  "bills" = "Difficulty paying bills",
  "transport" = "Transportation insecurity",
  "utility" = "Utility insecurity"
)

hardship_model_terms <- c(
  "bills_yes" = "Difficulty paying bills",
  "transport_yes" = "Transportation insecurity",
  "utility_yes" = "Utility insecurity"
)

## This lookup is used only to make the sample-tracking output readable.
jurisdiction_names <- c(
  "01" = "Alabama",
  "02" = "Alaska",
  "04" = "Arizona",
  "05" = "Arkansas",
  "06" = "California",
  "08" = "Colorado",
  "09" = "Connecticut",
  "10" = "Delaware",
  "11" = "District of Columbia",
  "12" = "Florida",
  "13" = "Georgia",
  "15" = "Hawaii",
  "16" = "Idaho",
  "17" = "Illinois",
  "18" = "Indiana",
  "19" = "Iowa",
  "20" = "Kansas",
  "21" = "Kentucky",
  "22" = "Louisiana",
  "23" = "Maine",
  "24" = "Maryland",
  "25" = "Massachusetts",
  "26" = "Michigan",
  "27" = "Minnesota",
  "28" = "Mississippi",
  "29" = "Missouri",
  "30" = "Montana",
  "31" = "Nebraska",
  "32" = "Nevada",
  "33" = "New Hampshire",
  "34" = "New Jersey",
  "35" = "New Mexico",
  "36" = "New York",
  "37" = "North Carolina",
  "38" = "North Dakota",
  "39" = "Ohio",
  "40" = "Oklahoma",
  "41" = "Oregon",
  "42" = "Pennsylvania",
  "44" = "Rhode Island",
  "45" = "South Carolina",
  "46" = "South Dakota",
  "47" = "Tennessee",
  "48" = "Texas",
  "49" = "Utah",
  "50" = "Vermont",
  "51" = "Virginia",
  "53" = "Washington",
  "54" = "West Virginia",
  "55" = "Wisconsin",
  "56" = "Wyoming",
  "66" = "Guam",
  "72" = "Puerto Rico",
  "78" = "U.S. Virgin Islands"
)

## These are the jurisdictions where both required optional modules were
## fielded in the supplied common/main LLCP files. They were verified against
## CDC's annual modules-by-state documentation and the local XPT schemas.
cross_module_jurisdictions <- list(
  "2022" = c(9, 10, 16, 18, 29, 34, 49, 50, 55, 72),
  "2023" = c(18, 34),
  "2024" = c(10, 18, 25)
)


###############################################################################
## 4. SET SURVEY VARIANCE OPTIONS
##
## Optional-module domain restriction can leave a stratum with one observed PSU.
## The "adjust" method centers a lonely PSU at the population mean. The domain
## option applies the same rule when a stratum is lonely only within the cancer-
## pain analytic subpopulation.
##
## Prior option values are restored at the end of the script.
###############################################################################

old_lonely_psu_option <- getOption("survey.lonely.psu")
old_domain_lonely_option <- getOption("survey.adjust.domain.lonely")

options(
  survey.lonely.psu = "adjust",
  survey.adjust.domain.lonely = TRUE
)


###############################################################################
## 5. READ, RECODE, AND FLAG ONE YEAR OF BRFSS DATA
##
## Cohort definitions:
##   1. Cancer-pain respondent: CSRVPAIN = 1.
##   2. Valid pain control: CSRVCTL2 is 1, 2, 3, or 4.
##   3. Complete analytic case: valid outcome, all three hardship items,
##      age, sex, income, and all complex-survey design variables.
##
## Pain-control coding follows the original UCP script:
##   CSRVCTL2 1 or 2 = Controlled
##   CSRVCTL2 3 or 4 = Uncontrolled
###############################################################################

read_and_prepare_year <- function(year, path) {
  message("Reading selected variables from ", basename(path), "...")

  year_value <- as.character(year)

  raw <- haven::read_xpt(
    path,
    col_select = dplyr::all_of(source_variables)
  )

  names(raw) <- toupper(names(raw))

  missing_variables <- setdiff(source_variables, names(raw))
  if (length(missing_variables) > 0) {
    stop(
      "Required variable(s) missing from ", year, ": ",
      paste(missing_variables, collapse = ", ")
    )
  }

  ## Haven imports BRFSS fields as labelled numeric vectors. Converting the
  ## selected variables to ordinary numeric vectors makes code checks explicit.
  prepared <- raw |>
    transmute(
      year = as.integer(year),
      jurisdiction_code = as.numeric(.data[["_STATE"]]),
      CSRVPAIN = as.numeric(.data[["CSRVPAIN"]]),
      CSRVCTL2 = as.numeric(.data[["CSRVCTL2"]]),
      SDHBILLS = as.numeric(.data[["SDHBILLS"]]),
      SDHTRNSP = as.numeric(.data[["SDHTRNSP"]]),
      SDHUTILS = as.numeric(.data[["SDHUTILS"]]),
      age_code = as.numeric(.data[["_AGEG5YR"]]),
      sex_code = as.numeric(.data[["_SEX"]]),
      income_code = as.numeric(.data[["_INCOMG1"]]),
      final_weight = as.numeric(.data[["_LLCPWT"]]),
      original_stratum = as.numeric(.data[["_STSTR"]]),
      original_psu = as.numeric(.data[["_PSU"]])
    ) |>
    mutate(
      design_eligible = (
        is.finite(final_weight) &
          final_weight > 0 &
          !is.na(original_stratum) &
          !is.na(original_psu)
      ),
      module_overlap_jurisdiction = (
        jurisdiction_code %in%
          cross_module_jurisdictions[[year_value]]
      ),
      cancer_pain_yes = !is.na(CSRVPAIN) & CSRVPAIN == 1,
      valid_pain_control = (
        cancer_pain_yes &
          !is.na(CSRVCTL2) &
          CSRVCTL2 %in% c(1, 2, 3, 4)
      ),
      valid_all_hardships = (
        valid_pain_control &
          SDHBILLS %in% c(1, 2) &
          SDHTRNSP %in% c(1, 2) &
          SDHUTILS %in% c(1, 2)
      ),
      analytic_eligible = (
        design_eligible &
          valid_pain_control &
          SDHBILLS %in% c(1, 2) &
          SDHTRNSP %in% c(1, 2) &
          SDHUTILS %in% c(1, 2) &
          age_code %in% 1:13 &
          sex_code %in% c(1, 2) &
          income_code %in% 1:7
      ),
      pain_control = case_when(
        CSRVCTL2 %in% c(1, 2) ~ "Controlled",
        CSRVCTL2 %in% c(3, 4) ~ "Uncontrolled",
        TRUE ~ NA_character_
      ),
      pain_control = factor(
        pain_control,
        levels = c("Controlled", "Uncontrolled")
      ),
      uncontrolled_pain = case_when(
        CSRVCTL2 %in% c(1, 2) ~ 0,
        CSRVCTL2 %in% c(3, 4) ~ 1,
        TRUE ~ NA_real_
      ),
      bills = case_when(
        SDHBILLS == 1 ~ "Yes",
        SDHBILLS == 2 ~ "No",
        TRUE ~ NA_character_
      ),
      bills = factor(bills, levels = c("No", "Yes")),
      bills_yes = case_when(
        SDHBILLS == 1 ~ 1,
        SDHBILLS == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      transport = case_when(
        SDHTRNSP == 1 ~ "Yes",
        SDHTRNSP == 2 ~ "No",
        TRUE ~ NA_character_
      ),
      transport = factor(transport, levels = c("No", "Yes")),
      transport_yes = case_when(
        SDHTRNSP == 1 ~ 1,
        SDHTRNSP == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      utility = case_when(
        SDHUTILS == 1 ~ "Yes",
        SDHUTILS == 2 ~ "No",
        TRUE ~ NA_character_
      ),
      utility = factor(utility, levels = c("No", "Yes")),
      utility_yes = case_when(
        SDHUTILS == 1 ~ 1,
        SDHUTILS == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      age_group = age_labels[as.character(age_code)],
      age_group = factor(age_group, levels = age_labels),
      sex = case_when(
        sex_code == 1 ~ "Male",
        sex_code == 2 ~ "Female",
        TRUE ~ NA_character_
      ),
      sex = factor(sex, levels = c("Male", "Female")),
      income_group = income_labels[as.character(income_code)],
      income_group = factor(income_group, levels = income_labels)
    )

  ## These counts enumerate every current cancer-pain respondent before the
  ## strict complete-case restriction used by Tables 1-4 and Figures 1a-1d.
  tracking <- tibble(
    analysis_period = as.character(year),
    total_brfss_n = nrow(prepared),
    current_cancer_pain_yes_n = sum(prepared$cancer_pain_yes),
    valid_pain_control_n = sum(prepared$valid_pain_control),
    controlled_all_classifiable_n = sum(
      prepared$valid_pain_control &
        prepared$pain_control == "Controlled",
      na.rm = TRUE
    ),
    uncontrolled_all_classifiable_n = sum(
      prepared$valid_pain_control &
        prepared$pain_control == "Uncontrolled",
      na.rm = TRUE
    ),
    valid_pain_control_and_all_three_hardships_n = sum(
      prepared$valid_all_hardships
    ),
    complete_case_analytic_n = sum(prepared$analytic_eligible),
    controlled_analytic_n = sum(
      prepared$analytic_eligible &
        prepared$pain_control == "Controlled",
      na.rm = TRUE
    ),
    uncontrolled_analytic_n = sum(
      prepared$analytic_eligible &
        prepared$pain_control == "Uncontrolled",
      na.rm = TRUE
    )
  )

  list(data = prepared, tracking = tracking)
}


###############################################################################
## 6. PREPARE ALL THREE YEARS AND CALCULATE POOLED-WEIGHT SCALARS
##
## CDC multi-year guidance recommends rescaling annual weights when annual
## sample sizes differ. For each year, the pooling base is the full number of
## BRFSS respondents in jurisdictions that fielded both required modules in the
## supplied main LLCP file. That count is divided by the three-year total. The
## resulting scalar is applied only in the pooled design. Annual designs retain
## the original _LLCPWT.
##
## The three annual full-file sample sizes are close, so these scalars are also
## close to one third. Prefixing strata and PSUs by year prevents identifiers
## from different survey cycles from being treated as the same design units.
###############################################################################

prepared_years <- purrr::imap(
  xpt_paths,
  ~ read_and_prepare_year(year = .y, path = .x)
)

all_source_data <- purrr::map_dfr(prepared_years, "data")
annual_tracking <- purrr::map_dfr(prepared_years, "tracking")

annual_pooling_base_sizes <- purrr::imap_dbl(
  prepared_years,
  function(prepared_object, year_label) {
    sum(
      prepared_object$data$module_overlap_jurisdiction
    )
  }
)

pooling_scalars <- annual_pooling_base_sizes /
  sum(annual_pooling_base_sizes)

all_source_data <- all_source_data |>
  mutate(
    pooling_scalar = unname(
      pooling_scalars[as.character(year)]
    ),
    year_factor = factor(year, levels = c(2022, 2023, 2024))
  )


###############################################################################
## 7. CONSTRUCT ANNUAL AND POOLED COMPLEX-SURVEY DOMAIN DESIGNS
##
## A full BRFSS design is created before restricting to the cancer-pain cohort.
## Subsetting the survey design afterward performs subpopulation/domain
## analysis and retains the design information needed for valid standard errors.
###############################################################################

make_brfss_design <- function(data, pooled = FALSE) {
  design_source <- data |>
    ## Jurisdictions that did not administer both optional modules are outside
    ## the target population and are removed before defining the design.
    filter(
      design_eligible,
      module_overlap_jurisdiction
    ) |>
    mutate(
      survey_stratum = as.integer(
        interaction(
          year,
          original_stratum,
          drop = TRUE,
          lex.order = TRUE
        )
      ),
      survey_psu = as.integer(
        interaction(
          year,
          original_psu,
          drop = TRUE,
          lex.order = TRUE
        )
      ),
      analysis_weight = if (pooled) {
        final_weight * pooling_scalar
      } else {
        final_weight
      }
    )

  full_design <- survey::svydesign(
    ids = ~survey_psu,
    strata = ~survey_stratum,
    weights = ~analysis_weight,
    data = design_source,
    nest = TRUE
  )

  ## The returned object contains only complete analytic cases, but the subset
  ## operation is performed on the already-defined full complex survey design.
  base::subset(full_design, analytic_eligible)
}

annual_source_sets <- split(all_source_data, all_source_data$year)

annual_designs <- purrr::map(
  annual_source_sets,
  ~ make_brfss_design(.x, pooled = FALSE)
)

pooled_design <- make_brfss_design(
  all_source_data,
  pooled = TRUE
)

analysis_designs <- c(
  annual_designs,
  list("Pooled 2022-2024" = pooled_design)
)


###############################################################################
## 8. COMPLETE AND SAVE THE CANCER-PAIN SAMPLE TRACKING FILE
##
## This file documents all cancer-pain respondents, valid controlled/
## uncontrolled outcomes, and the final complete-case cohort. Weighted pain-
## control percentages refer to the complete-case analytic cohort.
###############################################################################

weighted_outcome_percentages <- function(design) {
  design_weights <- as.numeric(stats::weights(design, type = "sampling"))
  uncontrolled <- design$variables$uncontrolled_pain

  uncontrolled_percent <- 100 * sum(
    design_weights * uncontrolled
  ) / sum(design_weights)

  c(
    controlled = 100 - uncontrolled_percent,
    uncontrolled = uncontrolled_percent
  )
}

annual_weighted_outcomes <- purrr::map_dfr(
  annual_designs,
  function(design) {
    estimates <- weighted_outcome_percentages(design)
    tibble(
      weighted_controlled_percent = estimates[["controlled"]],
      weighted_uncontrolled_percent = estimates[["uncontrolled"]]
    )
  },
  .id = "analysis_period"
)

format_jurisdictions <- function(data) {
  codes <- sort(unique(data$jurisdiction_code[data$analytic_eligible]))
  code_keys <- sprintf("%02d", as.integer(codes))
  readable_names <- unname(jurisdiction_names[code_keys])
  readable_names[is.na(readable_names)] <- paste0(
    "FIPS ",
    code_keys[is.na(readable_names)]
  )
  paste(readable_names, collapse = "; ")
}

annual_jurisdictions <- purrr::imap_dfr(
  annual_source_sets,
  function(data, year_label) {
    tibble(
      analysis_period = as.character(year_label),
      analytic_jurisdictions_n = dplyr::n_distinct(
        data$jurisdiction_code[data$analytic_eligible]
      ),
      analytic_jurisdictions = format_jurisdictions(data)
    )
  }
)

sample_tracking <- annual_tracking |>
  left_join(annual_weighted_outcomes, by = "analysis_period") |>
  left_join(annual_jurisdictions, by = "analysis_period") |>
  mutate(
    annual_pooling_base_n = unname(
      annual_pooling_base_sizes[analysis_period]
    ),
    pooled_weight_scalar = unname(
      pooling_scalars[analysis_period]
    )
  )

pooled_outcome_estimates <- weighted_outcome_percentages(pooled_design)

pooled_tracking <- tibble(
  analysis_period = "Pooled 2022-2024",
  total_brfss_n = sum(sample_tracking$total_brfss_n),
  current_cancer_pain_yes_n = sum(
    sample_tracking$current_cancer_pain_yes_n
  ),
  valid_pain_control_n = sum(sample_tracking$valid_pain_control_n),
  controlled_all_classifiable_n = sum(
    sample_tracking$controlled_all_classifiable_n
  ),
  uncontrolled_all_classifiable_n = sum(
    sample_tracking$uncontrolled_all_classifiable_n
  ),
  valid_pain_control_and_all_three_hardships_n = sum(
    sample_tracking$valid_pain_control_and_all_three_hardships_n
  ),
  complete_case_analytic_n = sum(
    sample_tracking$complete_case_analytic_n
  ),
  controlled_analytic_n = sum(sample_tracking$controlled_analytic_n),
  uncontrolled_analytic_n = sum(
    sample_tracking$uncontrolled_analytic_n
  ),
  weighted_controlled_percent = pooled_outcome_estimates[["controlled"]],
  weighted_uncontrolled_percent = pooled_outcome_estimates[["uncontrolled"]],
  analytic_jurisdictions_n = dplyr::n_distinct(
    all_source_data$jurisdiction_code[
      all_source_data$analytic_eligible
    ]
  ),
  analytic_jurisdictions = format_jurisdictions(all_source_data),
  annual_pooling_base_n = sum(
    sample_tracking$annual_pooling_base_n
  ),
  pooled_weight_scalar = NA_real_
)

sample_tracking <- bind_rows(sample_tracking, pooled_tracking) |>
  mutate(
    weighted_controlled_percent = round(
      weighted_controlled_percent,
      1
    ),
    weighted_uncontrolled_percent = round(
      weighted_uncontrolled_percent,
      1
    ),
    pooled_weight_scalar = round(pooled_weight_scalar, 6)
  )

readr::write_csv(
  sample_tracking,
  file.path(
    output_dir,
    "sample_tracking_weighted_2022_2024.csv"
  ),
  na = ""
)


###############################################################################
## 9. REMAKE TABLE 1 WITH SURVEY-WEIGHTED PERCENTAGES
##
## Table 1 retains unweighted respondent counts because a weighted survey
## percentage is not a count of interviewed people. The final requested column
## explicitly shows controlled and uncontrolled representation within each row:
## raw n followed by the complex-survey-weighted row percentage.
##
## Example final-column cell:
##   Controlled: 100 (70.0%); Uncontrolled: 40 (30.0%)
###############################################################################

table1_specifications <- list(
  list(
    variable = "pain_control",
    label = "Pain control",
    categories = c("Controlled", "Uncontrolled")
  ),
  list(
    variable = "bills",
    label = "Difficulty paying bills",
    categories = c("No", "Yes")
  ),
  list(
    variable = "transport",
    label = "Transportation insecurity",
    categories = c("No", "Yes")
  ),
  list(
    variable = "utility",
    label = "Utility insecurity",
    categories = c("No", "Yes")
  ),
  list(
    variable = "sex",
    label = "Sex",
    categories = c("Female", "Male")
  ),
  list(
    variable = "age_group",
    label = "Age group",
    categories = age_labels
  ),
  list(
    variable = "income_group",
    label = "Income group",
    categories = income_labels
  )
)

make_weighted_table1 <- function(design, period_label) {
  survey_data <- design$variables
  design_weights <- as.numeric(
    stats::weights(design, type = "sampling")
  )

  purrr::map_dfr(table1_specifications, function(specification) {
    purrr::map_dfr(specification$categories, function(category_label) {
      category_index <- (
        !is.na(survey_data[[specification$variable]]) &
          as.character(survey_data[[specification$variable]]) ==
            category_label
      )

      controlled_index <- (
        category_index &
          survey_data$pain_control == "Controlled"
      )
      uncontrolled_index <- (
        category_index &
          survey_data$pain_control == "Uncontrolled"
      )

      category_weight <- sum(design_weights[category_index])

      weighted_percent <- if (category_weight > 0) {
        100 * category_weight / sum(design_weights)
      } else {
        NA_real_
      }

      controlled_row_percent <- if (category_weight > 0) {
        100 * sum(design_weights[controlled_index]) / category_weight
      } else {
        NA_real_
      }

      uncontrolled_row_percent <- if (category_weight > 0) {
        100 * sum(design_weights[uncontrolled_index]) / category_weight
      } else {
        NA_real_
      }

      controlled_n <- sum(controlled_index, na.rm = TRUE)
      uncontrolled_n <- sum(uncontrolled_index, na.rm = TRUE)
      unweighted_n <- sum(category_index, na.rm = TRUE)

      tibble(
        analysis_period = period_label,
        variable = specification$label,
        category = category_label,
        unweighted_n = unweighted_n,
        weighted_percent = weighted_percent,
        unweighted_n_weighted_percent = sprintf(
          "%s (%.1f%%)",
          format(unweighted_n, big.mark = ",", scientific = FALSE),
          weighted_percent
        ),
        controlled_unweighted_n = controlled_n,
        controlled_weighted_row_percent = controlled_row_percent,
        uncontrolled_unweighted_n = uncontrolled_n,
        uncontrolled_weighted_row_percent = uncontrolled_row_percent,
        controlled_uncontrolled_representation = sprintf(
          paste0(
            "Controlled: %s (%.1f%%); ",
            "Uncontrolled: %s (%.1f%%)"
          ),
          format(controlled_n, big.mark = ",", scientific = FALSE),
          controlled_row_percent,
          format(uncontrolled_n, big.mark = ",", scientific = FALSE),
          uncontrolled_row_percent
        )
      )
    })
  })
}

table1_weighted <- purrr::imap_dfr(
  analysis_designs,
  ~ make_weighted_table1(.x, .y)
) |>
  mutate(
    weighted_percent = round(weighted_percent, 1),
    controlled_weighted_row_percent = round(
      controlled_weighted_row_percent,
      1
    ),
    uncontrolled_weighted_row_percent = round(
      uncontrolled_weighted_row_percent,
      1
    )
  )

readr::write_csv(
  table1_weighted,
  file.path(
    output_dir,
    "table1_weighted_descriptive_statistics_2022_2024.csv"
  ),
  na = ""
)


###############################################################################
## 10. REMAKE TABLE 2 WITH DESIGN-BASED RAO-SCOTT CHI-SQUARE TESTS
##
## An ordinary Pearson chi-square treats BRFSS observations as a simple random
## sample and is therefore inappropriate here. survey::svychisq() applies the
## second-order Rao-Scott correction and reports its recommended F
## approximation with numerator and denominator degrees of freedom.
##
## The two descriptive cells preserve the original Table 2 style:
##   unweighted uncontrolled n / unweighted subgroup n
##   (complex-survey-weighted percent uncontrolled)
###############################################################################

weighted_percent_uncontrolled <- function(
    design_weights,
    uncontrolled_outcome,
    subgroup_index
) {
  subgroup_weight <- sum(design_weights[subgroup_index])

  if (subgroup_weight <= 0) {
    return(NA_real_)
  }

  100 * sum(
    design_weights[subgroup_index] *
      uncontrolled_outcome[subgroup_index]
  ) / subgroup_weight
}

make_weighted_table2 <- function(design, period_label) {
  survey_data <- design$variables
  design_weights <- as.numeric(
    stats::weights(design, type = "sampling")
  )

  purrr::map_dfr(names(hardship_labels), function(variable_name) {
    no_index <- (
      !is.na(survey_data[[variable_name]]) &
        survey_data[[variable_name]] == "No"
    )
    yes_index <- (
      !is.na(survey_data[[variable_name]]) &
        survey_data[[variable_name]] == "Yes"
    )

    no_total <- sum(no_index)
    yes_total <- sum(yes_index)
    no_uncontrolled <- sum(
      no_index & survey_data$uncontrolled_pain == 1
    )
    yes_uncontrolled <- sum(
      yes_index & survey_data$uncontrolled_pain == 1
    )

    no_weighted_percent <- weighted_percent_uncontrolled(
      design_weights,
      survey_data$uncontrolled_pain,
      no_index
    )
    yes_weighted_percent <- weighted_percent_uncontrolled(
      design_weights,
      survey_data$uncontrolled_pain,
      yes_index
    )

    test_formula <- stats::as.formula(
      paste0("~", variable_name, " + pain_control")
    )

    ## The conservative domain-lonely adjustment can emit one warning per
    ## affected stratum. Those expected repetitive warnings are suppressed here;
    ## finite test statistics are validated below.
    rao_scott_test <- suppressWarnings(
      survey::svychisq(
        test_formula,
        design = design,
        statistic = "F"
      )
    )

    test_parameters <- unname(rao_scott_test$parameter)

    tibble(
      analysis_period = period_label,
      hardship = unname(hardship_labels[[variable_name]]),
      uncontrolled_among_no = sprintf(
        "%d/%d (%.1f%%)",
        no_uncontrolled,
        no_total,
        no_weighted_percent
      ),
      uncontrolled_among_yes = sprintf(
        "%d/%d (%.1f%%)",
        yes_uncontrolled,
        yes_total,
        yes_weighted_percent
      ),
      weighted_percent_uncontrolled_no = no_weighted_percent,
      weighted_percent_uncontrolled_yes = yes_weighted_percent,
      test = "Second-order Rao-Scott chi-square (F approximation)",
      rao_scott_F = unname(rao_scott_test$statistic[[1]]),
      numerator_df = if (length(test_parameters) >= 1) {
        test_parameters[[1]]
      } else {
        NA_real_
      },
      denominator_df = if (length(test_parameters) >= 2) {
        test_parameters[[2]]
      } else {
        NA_real_
      },
      p_value = rao_scott_test$p.value,
      p_value_display = ifelse(
        rao_scott_test$p.value < 0.001,
        "<0.001",
        sprintf("%.4f", rao_scott_test$p.value)
      )
    )
  })
}

table2_weighted <- suppressWarnings(
  purrr::imap_dfr(
    analysis_designs,
    ~ make_weighted_table2(.x, .y)
  )
) |>
  mutate(
    weighted_percent_uncontrolled_no = round(
      weighted_percent_uncontrolled_no,
      1
    ),
    weighted_percent_uncontrolled_yes = round(
      weighted_percent_uncontrolled_yes,
      1
    ),
    rao_scott_F = round(rao_scott_F, 3),
    numerator_df = round(numerator_df, 2),
    denominator_df = round(denominator_df, 2),
    p_value = signif(p_value, 6)
  )

readr::write_csv(
  table2_weighted,
  file.path(
    output_dir,
    "table2_weighted_rao_scott_chi_square_2022_2024.csv"
  ),
  na = ""
)


###############################################################################
## 11. FIT SURVEY-WEIGHTED LOGISTIC REGRESSION MODELS
##
## survey::svyglm() with quasibinomial() estimates the same logit coefficients
## as a survey-weighted binomial model while avoiding a warning about
## non-integer weighted counts. Standard errors, confidence intervals, and
## hypothesis tests incorporate the weights, strata, and PSUs.
##
## Table 3 model:
##   All three hardship indicators are entered together. Thus each hardship OR
##   is mutually adjusted for the other two hardships, but there is no
##   demographic adjustment. This retains the original UCP script formula and
##   avoids incorrectly calling the joint model a separate crude/bivariate OR.
##
## Table 4 model:
##   Annual models adjust for age group, sex, and income group.
##   The pooled model additionally adjusts for survey year.
###############################################################################

run_weighted_logistic_model <- function(
    design,
    period_label,
    adjusted
) {
  if (adjusted) {
    if (dplyr::n_distinct(design$variables$year) > 1) {
      model_formula <- uncontrolled_pain ~
        bills_yes + transport_yes + utility_yes +
        age_group + sex + income_group + year_factor
      model_adjustment <- paste0(
        "All three hardships; adjusted for age group, sex, ",
        "income group, and survey year"
      )
    } else {
      model_formula <- uncontrolled_pain ~
        bills_yes + transport_yes + utility_yes +
        age_group + sex + income_group
      model_adjustment <- paste0(
        "All three hardships; adjusted for age group, sex, ",
        "and income group"
      )
    }
  } else {
    model_formula <- uncontrolled_pain ~
      bills_yes + transport_yes + utility_yes
    model_adjustment <- paste0(
      "All three hardship indicators entered simultaneously; ",
      "no demographic covariates"
    )
  }

  ## Expected repetitive domain-lonely warnings are suppressed. Model
  ## convergence and coefficient finiteness are checked explicitly afterward.
  model <- suppressWarnings(
    survey::svyglm(
      formula = model_formula,
      design = design,
      family = quasibinomial()
    )
  )

  if (!isTRUE(model$converged)) {
    stop(period_label, " weighted logistic model did not converge.")
  }

  coefficient_matrix <- suppressWarnings(
    summary(model)$coefficients
  )
  model_residual_df <- stats::df.residual(model)

  ## Survey models use a t reference distribution when residual design degrees
  ## of freedom are available; otherwise the normal approximation is used.
  critical_value <- if (
    length(model_residual_df) == 1 &&
      is.finite(model_residual_df) &&
      model_residual_df > 0
  ) {
    stats::qt(0.975, df = model_residual_df)
  } else {
    stats::qnorm(0.975)
  }

  coefficient_table <- tibble(
    term = rownames(coefficient_matrix),
    log_odds = coefficient_matrix[, 1],
    standard_error = coefficient_matrix[, 2],
    test_statistic = coefficient_matrix[, 3],
    p_value = coefficient_matrix[, 4]
  ) |>
    filter(term %in% names(hardship_model_terms)) |>
    mutate(
      analysis_period = period_label,
      predictor = unname(hardship_model_terms[term]),
      reference = "No hardship",
      model_adjustment = model_adjustment,
      unweighted_model_n = nrow(design$variables),
      residual_design_df = model_residual_df,
      odds_ratio = exp(log_odds),
      ci_lower_95 = exp(
        log_odds - critical_value * standard_error
      ),
      ci_upper_95 = exp(
        log_odds + critical_value * standard_error
      )
    ) |>
    select(
      analysis_period,
      predictor,
      reference,
      model_adjustment,
      unweighted_model_n,
      residual_design_df,
      odds_ratio,
      ci_lower_95,
      ci_upper_95,
      p_value,
      log_odds,
      standard_error,
      test_statistic
    )

  if (nrow(coefficient_table) != length(hardship_model_terms)) {
    stop(
      "The ", period_label,
      " weighted logistic model did not return all hardship terms."
    )
  }

  coefficient_table
}

table3_model_results <- suppressWarnings(
  purrr::imap_dfr(
    analysis_designs,
    ~ run_weighted_logistic_model(
      design = .x,
      period_label = .y,
      adjusted = FALSE
    )
  )
)

table4_model_results <- suppressWarnings(
  purrr::imap_dfr(
    analysis_designs,
    ~ run_weighted_logistic_model(
      design = .x,
      period_label = .y,
      adjusted = TRUE
    )
  )
)

format_regression_table <- function(model_results) {
  model_results |>
    mutate(
      odds_ratio_ci = sprintf(
        "%.2f (%.2f-%.2f)",
        odds_ratio,
        ci_lower_95,
        ci_upper_95
      ),
      p_value_display = ifelse(
        p_value < 0.001,
        "<0.001",
        sprintf("%.4f", p_value)
      ),
      residual_design_df = round(residual_design_df, 2),
      odds_ratio = round(odds_ratio, 3),
      ci_lower_95 = round(ci_lower_95, 3),
      ci_upper_95 = round(ci_upper_95, 3),
      p_value = signif(p_value, 6),
      log_odds = round(log_odds, 5),
      standard_error = round(standard_error, 5),
      test_statistic = round(test_statistic, 4)
    ) |>
    select(
      analysis_period,
      predictor,
      reference,
      model_adjustment,
      unweighted_model_n,
      residual_design_df,
      odds_ratio,
      ci_lower_95,
      ci_upper_95,
      odds_ratio_ci,
      p_value,
      p_value_display,
      log_odds,
      standard_error,
      test_statistic
    )
}

table3_weighted <- format_regression_table(table3_model_results)
table4_weighted <- format_regression_table(table4_model_results)

readr::write_csv(
  table3_weighted,
  file.path(
    output_dir,
    "table3_weighted_hardship_only_logistic_regression_2022_2024.csv"
  ),
  na = ""
)

readr::write_csv(
  table4_weighted,
  file.path(
    output_dir,
    "table4_weighted_adjusted_logistic_regression_2022_2024.csv"
  ),
  na = ""
)


###############################################################################
## 12. MAKE WEIGHTED LOGISTIC-REGRESSION FIGURES 1A-1D
##
## Each forest plot shows adjusted odds ratios from Table 4 and design-based
## 95% confidence intervals. The x-axis is logarithmic, and the dashed vertical
## line at OR = 1 marks the null association. Files are saved in both the
## requested high-resolution PNG and editable vector SVG formats.
###############################################################################

figure_periods <- tribble(
  ~analysis_period, ~panel, ~file_suffix, ~title_year, ~subtitle,
  "2022", "1a", "2022", "2022",
  "Adjusted for age group, sex, and income group",
  "2023", "1b", "2023", "2023",
  "Adjusted for age group, sex, and income group",
  "2024", "1c", "2024", "2024",
  "Adjusted for age group, sex, and income group",
  "Pooled 2022-2024", "1d", "pooled_2022_2024", "2022-2024",
  "Adjusted for age group, sex, income group, and survey year"
)

make_adjusted_or_figure <- function(
    model_results,
    selected_period,
    panel_label,
    title_year,
    subtitle
) {
  plot_data <- model_results |>
    filter(analysis_period == selected_period) |>
    mutate(
      predictor = factor(
        predictor,
        levels = c(
          "Difficulty paying bills",
          "Transportation insecurity",
          "Utility insecurity"
        )
      ),
      estimate_label = sprintf(
        "%.2f (%.2f-%.2f)",
        odds_ratio,
        ci_lower_95,
        ci_upper_95
      ),
      label_x = ci_upper_95 * 1.08
    )

  if (nrow(plot_data) != 3) {
    stop(
      "Figure ", panel_label,
      " does not have all three hardship estimates."
    )
  }

  figure_caption <- paste0(
    "Figure ", panel_label,
    ". BRFSS complex-survey-weighted logistic regression; ",
    "points are odds ratios and lines are 95% design-based confidence ",
    "intervals. Reference: no hardship. Estimates represent ",
    "participating jurisdictions."
  )

  ggplot(
    plot_data,
    aes(y = predictor)
  ) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      linewidth = 0.55,
      color = "grey45"
    ) +
    geom_segment(
      aes(
        x = ci_lower_95,
        xend = ci_upper_95,
        yend = predictor
      ),
      linewidth = 0.8,
      color = "#24557A"
    ) +
    geom_point(
      aes(x = odds_ratio),
      size = 3.2,
      shape = 21,
      stroke = 0.8,
      color = "#173B55",
      fill = "#5FA8D3"
    ) +
    geom_text(
      aes(
        x = label_x,
        label = estimate_label
      ),
      hjust = 0,
      size = 3.5,
      color = "#1A1A1A"
    ) +
    scale_x_log10(
      breaks = scales::log_breaks(n = 6),
      labels = scales::label_number(accuracy = 0.01),
      expand = expansion(mult = c(0.08, 0.42))
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title = paste0(
        "Adjusted Odds Ratios for Uncontrolled Cancer Pain, ",
        title_year
      ),
      subtitle = subtitle,
      x = "Adjusted odds ratio (log scale)",
      y = NULL,
      caption = paste(
        strwrap(figure_caption, width = 115),
        collapse = "\n"
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title.position = "plot",
      plot.caption = element_text(
        hjust = 0,
        size = 8.5,
        color = "grey30",
        margin = margin(t = 10)
      ),
      plot.margin = margin(8, 145, 8, 10)
    )
}

purrr::pwalk(
  figure_periods,
  function(
      analysis_period,
      panel,
      file_suffix,
      title_year,
      subtitle
  ) {
    figure_object <- make_adjusted_or_figure(
      model_results = table4_model_results,
      selected_period = analysis_period,
      panel_label = panel,
      title_year = title_year,
      subtitle = subtitle
    )

    file_stem <- paste0(
      "figure",
      panel,
      "_weighted_adjusted_logistic_regression_",
      file_suffix
    )

    ggsave(
      filename = file.path(output_dir, paste0(file_stem, ".png")),
      plot = figure_object,
      width = 9.5,
      height = 5.8,
      units = "in",
      dpi = 300,
      bg = "white"
    )

    ggsave(
      filename = file.path(output_dir, paste0(file_stem, ".svg")),
      plot = figure_object,
      device = grDevices::svg,
      width = 9.5,
      height = 5.8,
      units = "in",
      bg = "white"
    )
  }
)


###############################################################################
## 13. VALIDATE THE COHORT, TABLES, MODELS, AND OUTPUT FILES
##
## These checks turn silent data drift into an explicit error. The expected
## counts come from independently auditing the source XPT files and the
## original UCP workflow.
###############################################################################

expected_complete_case_n <- c(
  "2022" = 785L,
  "2023" = 159L,
  "2024" = 281L
)

expected_current_cancer_pain_n <- c(
  "2022" = 1544L,
  "2023" = 222L,
  "2024" = 582L
)

expected_valid_pain_control_n <- c(
  "2022" = 1489L,
  "2023" = 211L,
  "2024" = 563L
)

observed_complete_case_n <- sample_tracking |>
  filter(analysis_period %in% names(expected_complete_case_n)) |>
  select(analysis_period, complete_case_analytic_n) |>
  tibble::deframe()

if (!identical(
  as.integer(observed_complete_case_n[names(expected_complete_case_n)]),
  as.integer(expected_complete_case_n)
)) {
  stop(
    "Complete-case counts do not match the audited source-data counts."
  )
}

observed_current_cancer_pain_n <- sample_tracking |>
  filter(analysis_period %in% names(expected_current_cancer_pain_n)) |>
  select(analysis_period, current_cancer_pain_yes_n) |>
  tibble::deframe()

observed_valid_pain_control_n <- sample_tracking |>
  filter(analysis_period %in% names(expected_valid_pain_control_n)) |>
  select(analysis_period, valid_pain_control_n) |>
  tibble::deframe()

if (!identical(
  as.integer(
    observed_current_cancer_pain_n[
      names(expected_current_cancer_pain_n)
    ]
  ),
  as.integer(expected_current_cancer_pain_n)
)) {
  stop("Cancer-pain counts do not match the audited source-data counts.")
}

if (!identical(
  as.integer(
    observed_valid_pain_control_n[
      names(expected_valid_pain_control_n)
    ]
  ),
  as.integer(expected_valid_pain_control_n)
)) {
  stop(
    "Classifiable controlled/uncontrolled counts do not match the ",
    "audited source-data counts."
  )
}

if (nrow(table2_weighted) != 12) {
  stop("Table 2 must contain 12 year-by-hardship results.")
}

if (any(!is.finite(table2_weighted$rao_scott_F)) ||
    any(!is.finite(table2_weighted$p_value))) {
  stop("Table 2 contains a non-finite test statistic or p value.")
}

if (nrow(table3_weighted) != 12 || nrow(table4_weighted) != 12) {
  stop("Tables 3 and 4 must each contain 12 regression results.")
}

regression_numeric_results <- bind_rows(
  table3_model_results,
  table4_model_results
)

if (
  any(!is.finite(regression_numeric_results$odds_ratio)) ||
    any(!is.finite(regression_numeric_results$ci_lower_95)) ||
    any(!is.finite(regression_numeric_results$ci_upper_95)) ||
    any(!is.finite(regression_numeric_results$p_value)) ||
    any(regression_numeric_results$ci_lower_95 <= 0) ||
    any(
      regression_numeric_results$ci_lower_95 >
        regression_numeric_results$odds_ratio
    ) ||
    any(
      regression_numeric_results$ci_upper_95 <
        regression_numeric_results$odds_ratio
    )
) {
  stop("A logistic-regression estimate or confidence interval is invalid.")
}

requested_output_files <- c(
  basename(script_path),
  "sample_tracking_weighted_2022_2024.csv",
  "table1_weighted_descriptive_statistics_2022_2024.csv",
  "table2_weighted_rao_scott_chi_square_2022_2024.csv",
  "table3_weighted_hardship_only_logistic_regression_2022_2024.csv",
  "table4_weighted_adjusted_logistic_regression_2022_2024.csv",
  paste0(
    "figure1",
    letters[1:4],
    "_weighted_adjusted_logistic_regression_",
    c("2022", "2023", "2024", "pooled_2022_2024"),
    ".png"
  ),
  paste0(
    "figure1",
    letters[1:4],
    "_weighted_adjusted_logistic_regression_",
    c("2022", "2023", "2024", "pooled_2022_2024"),
    ".svg"
  )
)

missing_outputs <- requested_output_files[
  !file.exists(file.path(output_dir, requested_output_files))
]

if (length(missing_outputs) > 0) {
  stop(
    "Expected output file(s) were not created: ",
    paste(missing_outputs, collapse = ", ")
  )
}

if (any(file.info(
  file.path(output_dir, requested_output_files)
)$size <= 0)) {
  stop("At least one requested output file is empty.")
}


###############################################################################
## 14. PRINT A CONCISE ANALYSIS SUMMARY AND RESTORE OPTIONS
###############################################################################

cat("\nSurvey-weighted UCP analysis completed successfully.\n\n")
cat("Sample tracking:\n")
print(sample_tracking)

cat("\nWeighted adjusted logistic regression results:\n")
print(
  table4_weighted |>
    select(
      analysis_period,
      predictor,
      odds_ratio_ci,
      p_value_display
    )
)

cat("\nFiles saved in:\n")
cat(output_dir, "\n")

options(
  survey.lonely.psu = old_lonely_psu_option,
  survey.adjust.domain.lonely = old_domain_lonely_option
)
