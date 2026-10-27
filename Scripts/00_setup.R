# Shared paths, labels, and small helpers. Only manuscript-listed add-on packages.
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
script_dir <- dirname(normalizePath(script_file, winslash = "/"))
root <- dirname(script_dir)
project <- dirname(root)
local_library <- Sys.getenv("UCP_R_LIBRARY",file.path(project,"New Stats Oct 2026","R_library"))
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))
suppressPackageStartupMessages(library(haven))
suppressPackageStartupMessages(library(survey))
options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)
for (folder in c("Validation", "Figures", "Tables + Supplementaries", "Manuscript"))
  dir.create(file.path(root, folder), showWarnings = FALSE)
save_csv <- function(x, name, folder = "Validation")
  write.csv(x, file.path(root, folder, paste0(name, ".csv")), row.names = FALSE, na = "")
hardships <- c("bills", "transport", "utility")
demographic <- c("age", "sex", "race")
socioeconomic <- c(demographic, "income", "education", "employment")
access <- c(socioeconomic, "insured", "MEDCOST1")
health <- c(access, "mobility", "other_disability", "poor_health", "depression", "comorbidity")
labels <- c(uncontrolled = "Uncontrolled pain", bills = "Difficulty paying bills",
  transport = "Transportation insecurity", utility = "Utility insecurity",
  age = "Age in years", sex = "Sex", race = "Race and ethnicity",
  income = "Annual household income", education = "Education", employment = "Employment",
  insured = "Health insurance", MEDCOST1 = "Unable to afford a doctor visit",
  mobility = "Mobility limitation", other_disability = "Other disability",
  poor_health = "General health", depression = "Depression history", comorbidity = "Comorbidity count")
state_names <- c(`9` = "Connecticut", `10` = "Delaware", `13` = "Georgia", `15` = "Hawaii",
  `16` = "Idaho", `18` = "Indiana", `25` = "Massachusetts", `29` = "Missouri",
  `31` = "Nebraska", `34` = "New Jersey", `49` = "Utah", `50` = "Vermont",
  `55` = "Wisconsin", `56` = "Wyoming", `66` = "Guam", `72` = "Puerto Rico")
yes_no <- function(x) ifelse(x %in% 1:2, as.numeric(x == 1), NA_real_)
valid <- function(x, codes) ifelse(x %in% codes, x, NA_real_)
warnings_seen <- character()
quiet <- function(expr) withCallingHandlers(expr, warning = function(w) {
  warnings_seen <<- unique(c(warnings_seen, conditionMessage(w)))
  if (grepl("has only one PSU", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
})
# Logit CIs use the domain design degrees of freedom.
prop <- function(design, indicator) {
  stopifnot(length(indicator) == nrow(design$variables), !anyNA(indicator))
  design$variables$indicator <- as.numeric(indicator)
  estimate <- quiet(svymean(~indicator, design))
  p <- as.numeric(coef(estimate)); n <- length(indicator); count <- sum(indicator)
  ci <- c(NA_real_, NA_real_)
  if (count > 0 && count < n && degf(design) > 0)
    ci <- plogis(qlogis(p) + c(-1, 1) * qt(.975, degf(design)) *
                   as.numeric(SE(estimate)) / (p * (1 - p)))
  data.frame(n = n, count = count, weighted_N = sum(weights(design) * indicator),
    percent = 100 * p, lower = 100 * ci[1], upper = 100 * ci[2])
}
ci_text <- function(x, lo, hi, digits = 2)
  sprintf(paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)"), x, lo, hi)
p_text <- function(p) ifelse(p < .001, "<.001", sub("^0", "", sprintf("%.3f", p)))
number <- function(x) format(round(x), big.mark = ",", trim = TRUE, scientific = FALSE)
