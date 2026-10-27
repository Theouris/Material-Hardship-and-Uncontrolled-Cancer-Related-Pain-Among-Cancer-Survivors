# Rscript --vanilla FINAL/Scripts/run_all.R
script_file <- sub("^--file=", "", grep("^--file=",commandArgs(),value=TRUE)[1])
script_dir <- dirname(normalizePath(script_file,winslash="/"))
for (file in c("01_prepare_data.R","02_analyse.R","03_export.R","04_validate.R")) {
  cat("\nRunning",file,"\n")
  source(file.path(script_dir,file))
}
