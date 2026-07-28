############################################################
# BRFSS Cancer Pain + Material Hardship Analysis
# Years: 2022, 2023, 2024 only
# No gt package needed
############################################################

# -----------------------------
# 1. Install/load packages
# -----------------------------


##  "haven", "dplyr", "tidyr", "purrr", "readr",
##  "ggplot2", "broom", "stringr", "tibble"

# -----------------------------
# 2. File paths
# -----------------------------

xpt_paths <- c(
  "2022" = "/home/theo/Documents/UCP/2022.XPT",
  "2023" = "/home/theo/Documents/UCP/2023.XPT",
  "2024" = "/home/theo/Documents/UCP/2024.XPT"
)

print(file.exists(xpt_paths))

if (any(!file.exists(xpt_paths))) {
  stop("One or more XPT files do not exist. Check the printed TRUE/FALSE list above.")
}

output_dir <- "/home/theo/Documents/UCP/brfss_cancer_pain_outputs_2022_2024"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


# -----------------------------
# 3. Required variables
# -----------------------------

required_vars <- c(
  "CSRVPAIN",
  "CSRVCTL2",
  "SDHBILLS",
  "SDHTRNSP",
  "SDHUTILS",
  "_AGEG5YR",
  "_SEX",
  "_INCOMG1"
)

age_labels <- c(
  "18-24", "25-29", "30-34", "35-39", "40-44",
  "45-49", "50-54", "55-59", "60-64", "65-69",
  "70-74", "75-79", "80+"
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

hardship_vars <- c(
  "bills" = "Difficulty paying bills",
  "transport" = "Transportation insecurity",
  "utility" = "Utility insecurity"
)

hardship_terms <- c(
  "bills_yes" = "Difficulty paying bills",
  "transport_yes" = "Transportation insecurity",
  "utility_yes" = "Utility insecurity"
)


# -----------------------------
# 4. Clean one year of data
# -----------------------------

process_year <- function(year, path) {
  
  message("Reading BRFSS ", year, "...")
  
  raw <- haven::read_xpt(path)
  
  names(raw) <- toupper(names(raw))
  
  missing_vars <- setdiff(required_vars, names(raw))
  
  if (length(missing_vars) > 0) {
    stop(
      "The following required variables are missing from ",
      year, ": ",
      paste(missing_vars, collapse = ", ")
    )
  }
  
  n_total <- nrow(raw)
  
  n_cancer_pain_yes_raw <- raw %>%
    filter(as.numeric(.data[["CSRVPAIN"]]) == 1) %>%
    nrow()
  
  cleaned <- raw %>%
    transmute(
      year = as.integer(year),
      CSRVPAIN = as.numeric(.data[["CSRVPAIN"]]),
      CSRVCTL2 = as.numeric(.data[["CSRVCTL2"]]),
      SDHBILLS = as.numeric(.data[["SDHBILLS"]]),
      SDHTRNSP = as.numeric(.data[["SDHTRNSP"]]),
      SDHUTILS = as.numeric(.data[["SDHUTILS"]]),
      `_AGEG5YR` = as.numeric(.data[["_AGEG5YR"]]),
      `_SEX` = as.numeric(.data[["_SEX"]]),
      `_INCOMG1` = as.numeric(.data[["_INCOMG1"]])
    ) %>%
    
    # Keep only respondents with current cancer-related pain
    filter(CSRVPAIN == 1) %>%
    
    # Omit don't know, not sure, refused, missing, and invalid codes
    filter(
      CSRVCTL2 %in% c(1, 2, 3, 4),
      SDHBILLS %in% c(1, 2),
      SDHTRNSP %in% c(1, 2),
      SDHUTILS %in% c(1, 2),
      `_AGEG5YR` %in% 1:13,
      `_SEX` %in% c(1, 2),
      `_INCOMG1` %in% 1:7
    ) %>%
    
    mutate(
      cancer_pain = "Yes",
      
      pain_under_control = if_else(CSRVCTL2 %in% c(1, 2), "Yes", "No"),
      
      pain_control = if_else(
        CSRVCTL2 %in% c(1, 2),
        "Controlled",
        "Uncontrolled"
      ),
      
      pain_control = factor(
        pain_control,
        levels = c("Controlled", "Uncontrolled")
      ),
      
      uncontrolled_pain = if_else(CSRVCTL2 %in% c(3, 4), 1L, 0L),
      
      bills = if_else(SDHBILLS == 1, "Yes", "No"),
      bills = factor(bills, levels = c("No", "Yes")),
      bills_yes = if_else(SDHBILLS == 1, 1L, 0L),
      
      transport = if_else(SDHTRNSP == 1, "Yes", "No"),
      transport = factor(transport, levels = c("No", "Yes")),
      transport_yes = if_else(SDHTRNSP == 1, 1L, 0L),
      
      utility = if_else(SDHUTILS == 1, "Yes", "No"),
      utility = factor(utility, levels = c("No", "Yes")),
      utility_yes = if_else(SDHUTILS == 1, 1L, 0L),
      
      age_group = age_labels[as.character(`_AGEG5YR`)],
      age_group = factor(age_group, levels = age_labels),
      
      sex = if_else(`_SEX` == 1, "Male", "Female"),
      sex = factor(sex, levels = c("Male", "Female")),
      
      income_group = income_labels[as.character(`_INCOMG1`)],
      income_group = factor(income_group, levels = income_labels)
    ) %>%
    
    select(
      year,
      CSRVPAIN,
      CSRVCTL2,
      SDHBILLS,
      SDHTRNSP,
      SDHUTILS,
      `_AGEG5YR`,
      `_SEX`,
      `_INCOMG1`,
      cancer_pain,
      pain_under_control,
      pain_control,
      uncontrolled_pain,
      bills,
      bills_yes,
      transport,
      transport_yes,
      utility,
      utility_yes,
      age_group,
      sex,
      income_group
    )
  
  readr::write_csv(
    cleaned,
    file.path(output_dir, paste0("brfss_cancer_pain_analytic_", year, ".csv"))
  )
  
  tracking <- tibble(
    year = as.integer(year),
    total_brfss_rows = n_total,
    cancer_pain_yes_before_cleaning = n_cancer_pain_yes_raw,
    analytic_complete_case_n = nrow(cleaned)
  )
  
  return(list(data = cleaned, tracking = tracking))
}


# -----------------------------
# 5. Run cleaning for 2022-2024
# -----------------------------

processed <- purrr::imap(
  xpt_paths,
  ~ process_year(year = .y, path = .x)
)

analysis_all <- purrr::map_dfr(processed, "data")

sample_tracking <- purrr::map_dfr(processed, "tracking")

readr::write_csv(
  analysis_all,
  file.path(output_dir, "brfss_cancer_pain_analytic_2022_2024.csv")
)

readr::write_csv(
  sample_tracking,
  file.path(output_dir, "sample_tracking_2022_2024.csv")
)


# -----------------------------
# 6. Descriptive table
# -----------------------------

make_descriptive_table <- function(df, year_label) {
  
  count_one_variable <- function(var_name, label) {
    
    df %>%
      count(category = as.character(.data[[var_name]]), name = "n") %>%
      mutate(
        year = year_label,
        variable = label,
        percent = round(100 * n / sum(n), 1),
        n_percent = paste0(n, " (", percent, "%)")
      ) %>%
      select(year, variable, category, n, percent, n_percent)
  }
  
  bind_rows(
    count_one_variable("pain_control", "Pain control"),
    count_one_variable("bills", "Difficulty paying bills"),
    count_one_variable("transport", "Transportation insecurity"),
    count_one_variable("utility", "Utility insecurity"),
    count_one_variable("sex", "Sex"),
    count_one_variable("age_group", "Age group"),
    count_one_variable("income_group", "Income group")
  )
}

datasets_for_tables <- split(analysis_all, analysis_all$year)
datasets_for_tables[["Pooled 2022-2024"]] <- analysis_all

descriptive_table <- purrr::imap_dfr(
  datasets_for_tables,
  ~ make_descriptive_table(.x, .y)
)

readr::write_csv(
  descriptive_table,
  file.path(output_dir, "table1_descriptive_statistics_2022_2024.csv")
)


# -----------------------------
# 7. Chi-square tests
# -----------------------------

safe_chisq <- function(tab) {
  tryCatch(
    suppressWarnings(chisq.test(tab, correct = TRUE)),
    error = function(e) NULL
  )
}

make_chi_square_table <- function(df, year_label) {
  
  purrr::map_dfr(names(hardship_vars), function(v) {
    
    hardship_status <- factor(df[[v]], levels = c("No", "Yes"))
    pain_status <- factor(df$pain_control, levels = c("Controlled", "Uncontrolled"))
    
    tab <- table(hardship_status, pain_status)
    
    test <- safe_chisq(tab)
    
    no_uncontrolled <- tab["No", "Uncontrolled"]
    no_total <- sum(tab["No", ])
    
    yes_uncontrolled <- tab["Yes", "Uncontrolled"]
    yes_total <- sum(tab["Yes", ])
    
    no_percent <- if (no_total > 0) {
      100 * no_uncontrolled / no_total
    } else {
      NA_real_
    }
    
    yes_percent <- if (yes_total > 0) {
      100 * yes_uncontrolled / yes_total
    } else {
      NA_real_
    }
    
    chi_square_value <- if (is.null(test)) NA_real_ else unname(test$statistic)
    df_value <- if (is.null(test)) NA_real_ else unname(test$parameter)
    p_value_value <- if (is.null(test)) NA_real_ else test$p.value
    low_expected_value <- if (is.null(test)) NA else any(test$expected < 5)
    
    tibble(
      year = year_label,
      hardship = hardship_vars[[v]],
      uncontrolled_among_no = paste0(
        no_uncontrolled, "/", no_total,
        " (", round(no_percent, 1), "%)"
      ),
      uncontrolled_among_yes = paste0(
        yes_uncontrolled, "/", yes_total,
        " (", round(yes_percent, 1), "%)"
      ),
      chi_square = chi_square_value,
      df = df_value,
      p_value = p_value_value,
      low_expected_cell = low_expected_value
    )
  })
}

chi_square_table <- purrr::imap_dfr(
  datasets_for_tables,
  ~ make_chi_square_table(.x, .y)
)

chi_square_table_print <- chi_square_table %>%
  mutate(
    chi_square = round(chi_square, 3),
    p_value = round(p_value, 4)
  )

readr::write_csv(
  chi_square_table_print,
  file.path(output_dir, "table2_chi_square_results_2022_2024.csv")
)


# -----------------------------
# 8. Logistic regression
# -----------------------------

run_logistic_model <- function(df, year_label, adjusted = FALSE) {
  
  if (nrow(df) < 10 || length(unique(df$uncontrolled_pain)) < 2) {
    return(tibble())
  }
  
  df2 <- df %>%
    mutate(year_factor = factor(year))
  
  if (adjusted == FALSE) {
    
    model_formula <- uncontrolled_pain ~ bills_yes + transport_yes + utility_yes
    model_label <- "Hardship-only"
    
  } else {
    
    if (n_distinct(df2$year) > 1) {
      model_formula <- uncontrolled_pain ~ bills_yes + transport_yes + utility_yes +
        age_group + sex + income_group + year_factor
      
      model_label <- "Adjusted for age, sex, income, and year"
    } else {
      model_formula <- uncontrolled_pain ~ bills_yes + transport_yes + utility_yes +
        age_group + sex + income_group
      
      model_label <- "Adjusted for age, sex, and income"
    }
  }
  
  model <- tryCatch(
    glm(
      model_formula,
      data = df2,
      family = binomial(),
      control = glm.control(maxit = 100)
    ),
    error = function(e) NULL
  )
  
  if (is.null(model)) {
    return(tibble())
  }
  
  coef_table <- coef(summary(model)) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    as_tibble() %>%
    rename(
      estimate = Estimate,
      std_error = `Std. Error`,
      statistic = `z value`,
      p_value = `Pr(>|z|)`
    ) %>%
    filter(term %in% names(hardship_terms)) %>%
    mutate(
      year = year_label,
      model = model_label,
      hardship = hardship_terms[term],
      odds_ratio = exp(estimate),
      ci_low = exp(estimate - 1.96 * std_error),
      ci_high = exp(estimate + 1.96 * std_error)
    ) %>%
    select(
      year,
      model,
      hardship,
      odds_ratio,
      ci_low,
      ci_high,
      p_value,
      estimate,
      std_error,
      statistic
    )
  
  return(coef_table)
}

logistic_hardship_only_table <- purrr::imap_dfr(
  datasets_for_tables,
  ~ run_logistic_model(.x, .y, adjusted = FALSE)
)

logistic_adjusted_table <- purrr::imap_dfr(
  datasets_for_tables,
  ~ run_logistic_model(.x, .y, adjusted = TRUE)
)

logistic_hardship_only_print <- logistic_hardship_only_table %>%
  mutate(
    odds_ratio = round(odds_ratio, 2),
    ci_low = round(ci_low, 2),
    ci_high = round(ci_high, 2),
    p_value = round(p_value, 4)
  )

logistic_adjusted_print <- logistic_adjusted_table %>%
  mutate(
    odds_ratio = round(odds_ratio, 2),
    ci_low = round(ci_low, 2),
    ci_high = round(ci_high, 2),
    p_value = round(p_value, 4)
  )

readr::write_csv(
  logistic_hardship_only_print,
  file.path(output_dir, "table3a_logistic_hardship_only_2022_2024.csv")
)

readr::write_csv(
  logistic_adjusted_print,
  file.path(output_dir, "table3b_logistic_adjusted_2022_2024.csv")
)


# -----------------------------
# 9. Figure 1:
# Percent uncontrolled pain by hardship
# -----------------------------

hardship_percent <- analysis_all %>%
  select(year, uncontrolled_pain, bills, transport, utility) %>%
  pivot_longer(
    cols = c(bills, transport, utility),
    names_to = "hardship_var",
    values_to = "reported_hardship"
  ) %>%
  mutate(
    hardship = recode(
      hardship_var,
      "bills" = "Bills",
      "transport" = "Transportation",
      "utility" = "Utility"
    )
  ) %>%
  group_by(year, hardship, reported_hardship) %>%
  summarise(
    n = n(),
    uncontrolled_n = sum(uncontrolled_pain),
    percent_uncontrolled = 100 * mean(uncontrolled_pain),
    .groups = "drop"
  )

readr::write_csv(
  hardship_percent,
  file.path(output_dir, "figure1_percent_data_2022_2024.csv")
)

fig1 <- ggplot(
  hardship_percent,
  aes(x = reported_hardship, y = percent_uncontrolled)
) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        round(percent_uncontrolled, 1),
        "%\n",
        uncontrolled_n,
        "/",
        n
      )
    ),
    vjust = -0.2,
    size = 3
  ) +
  facet_grid(year ~ hardship) +
  labs(
    title = "Percent With Uncontrolled Cancer Pain by Material Hardship",
    subtitle = "BRFSS 2022-2024, complete-case cancer-pain respondents",
    x = "Reported hardship",
    y = "Percent with uncontrolled pain",
    caption = "Figure 1. Unweighted complete-case percentages among respondents reporting current cancer-related pain."
  ) +
  ylim(0, max(hardship_percent$percent_uncontrolled, na.rm = TRUE) * 1.2) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(output_dir, "figure1_percent_uncontrolled_by_hardship_2022_2024.png"),
  fig1,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  file.path(output_dir, "figure1_percent_uncontrolled_by_hardship_2022_2024.pdf"),
  fig1,
  width = 12,
  height = 7
)


# Save one Figure 1 per year
for (y in sort(unique(hardship_percent$year))) {
  
  fig1_y <- hardship_percent %>%
    filter(year == y) %>%
    ggplot(aes(x = reported_hardship, y = percent_uncontrolled)) +
    geom_col() +
    geom_text(
      aes(
        label = paste0(
          round(percent_uncontrolled, 1),
          "%\n",
          uncontrolled_n,
          "/",
          n
        )
      ),
      vjust = -0.2,
      size = 3
    ) +
    facet_wrap(~ hardship) +
    labs(
      title = paste0("Percent With Uncontrolled Cancer Pain by Material Hardship, ", y),
      x = "Reported hardship",
      y = "Percent with uncontrolled pain",
      caption = paste0(
        "Figure 1, ",
        y,
        ". Unweighted complete-case percentages among respondents reporting current cancer-related pain."
      )
    ) +
    ylim(0, max(hardship_percent$percent_uncontrolled, na.rm = TRUE) * 1.2) +
    theme_minimal(base_size = 12)
  
  ggsave(
    file.path(output_dir, paste0("figure1_percent_uncontrolled_by_hardship_", y, ".png")),
    fig1_y,
    width = 10,
    height = 5,
    dpi = 300
  )
}


# -----------------------------
# 10. Figure 2:
# Adjusted logistic regression odds ratios
# -----------------------------

or_plot_data <- logistic_adjusted_table %>%
  filter(
    is.finite(odds_ratio),
    is.finite(ci_low),
    is.finite(ci_high),
    odds_ratio > 0,
    ci_low > 0,
    ci_high > 0
  ) %>%
  mutate(
    hardship = factor(
      hardship,
      levels = c(
        "Difficulty paying bills",
        "Transportation insecurity",
        "Utility insecurity"
      )
    )
  )

fig2 <- ggplot(
  or_plot_data,
  aes(x = odds_ratio, y = hardship)
) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_segment(
    aes(x = ci_low, xend = ci_high, y = hardship, yend = hardship)
  ) +
  geom_point(size = 2) +
  geom_text(
    aes(label = round(odds_ratio, 2)),
    nudge_y = 0.18,
    size = 3,
    check_overlap = TRUE
  ) +
  scale_x_log10() +
  facet_wrap(~ year) +
  labs(
    title = "Adjusted Odds Ratios for Uncontrolled Cancer Pain",
    subtitle = "Hardship predictors adjusted for age, sex, income, and year in the pooled model",
    x = "Odds ratio, log scale",
    y = "Material hardship",
    caption = "Figure 2. Odds ratios greater than 1 indicate higher odds of uncontrolled cancer pain."
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(output_dir, "figure2_adjusted_odds_ratios_2022_2024.png"),
  fig2,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  file.path(output_dir, "figure2_adjusted_odds_ratios_2022_2024.pdf"),
  fig2,
  width = 12,
  height = 7
)


# -----------------------------
# 11. Supplemental Figure:
# Hardship-only odds ratios
# -----------------------------

or_plot_data_unadjusted <- logistic_hardship_only_table %>%
  filter(
    is.finite(odds_ratio),
    is.finite(ci_low),
    is.finite(ci_high),
    odds_ratio > 0,
    ci_low > 0,
    ci_high > 0
  ) %>%
  mutate(
    hardship = factor(
      hardship,
      levels = c(
        "Difficulty paying bills",
        "Transportation insecurity",
        "Utility insecurity"
      )
    )
  )

fig2_unadjusted <- ggplot(
  or_plot_data_unadjusted,
  aes(x = odds_ratio, y = hardship)
) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_segment(
    aes(x = ci_low, xend = ci_high, y = hardship, yend = hardship)
  ) +
  geom_point(size = 2) +
  geom_text(
    aes(label = round(odds_ratio, 2)),
    nudge_y = 0.18,
    size = 3,
    check_overlap = TRUE
  ) +
  scale_x_log10() +
  facet_wrap(~ year) +
  labs(
    title = "Hardship-Only Odds Ratios for Uncontrolled Cancer Pain",
    subtitle = "Unadjusted logistic regression models",
    x = "Odds ratio, log scale",
    y = "Material hardship",
    caption = "Supplemental Figure. Odds ratios greater than 1 indicate higher odds of uncontrolled cancer pain."
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(output_dir, "figure2_supplement_hardship_only_odds_ratios_2022_2024.png"),
  fig2_unadjusted,
  width = 12,
  height = 7,
  dpi = 300
)


# -----------------------------
# 12. Print final outputs
# -----------------------------

cat("\nAnalysis complete.\n")
cat("Files saved to:\n")
cat(output_dir, "\n\n")

cat("Sample tracking:\n")
print(sample_tracking)

cat("\nChi-square results:\n")
print(chi_square_table_print)

cat("\nAdjusted logistic regression results:\n")
print(logistic_adjusted_print)

cat("\nHardship-only logistic regression results:\n")
print(logistic_hardship_only_print)

