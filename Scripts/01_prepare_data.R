# Read the four original XPT files and define the survey design before domains.
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE)[1]))), "00_setup.R"))
data_dir <- Sys.getenv("BRFSS_DATA_DIR", "C:/Users/Theo/Documents/research/datasets/BRFSS")
years <- 2022:2025
# Intersections of main-file Pain Management and Social Determinants coverage.
states <- list(`2022`=c(9,10,16,18,29,34,49,50,55,72), `2023`=c(18,34),
  `2024`=c(10,18,25), `2025`=c(13,15,31,56,66))
variables <- c("_STATE","QSTVER","_LLCPWT","_STSTR","_PSU","CSRVPAIN","CSRVCTL2",
  "SDHBILLS","SDHTRNSP","SDHUTILS","_AGEG5YR","_SEX","_RACEGR4","_RACEGR3",
  "_INCOMG1","EDUCA","EMPLOY1","PRIMINSR","PRIMINS1","PRIMINS2","MEDCOST1",
  "GENHLTH","DIFFWALK","DEAF","BLIND","DECIDE","DIFFDRES","DIFFALON","ADDEPEV3",
  "CVDINFR4","CVDCRHD4","CVDSTRK3","CHCCOPD3","CHCKDNY2","DIABETE4","ASTHMA3",
  "CHCSCNC1","CHCOCNC1","_AGE80","CSRVTRT3","CNCRAGE","CNCRTYP2","CNCRDIFF")
parts <- list(); manifest <- list(); dictionary <- list()
for (year in years) {
  path <- file.path(data_dir, paste0("LLCP", year, ".XPT"))
  stopifnot(file.exists(path))
  cat("Reading", path, "\n")
  x <- as.data.frame(read_xpt(path, col_select = dplyr::any_of(variables)))
  dictionary[[as.character(year)]] <- lapply(x, function(z) list(label=attr(z,"label"), values=attr(z,"labels")))
  x <- as.data.frame(lapply(x, as.numeric), check.names = FALSE)
  manifest[[as.character(year)]] <- data.frame(year=year, file=normalizePath(path,winslash="/"),
    bytes=file.info(path)$size, records=nrow(x), md5=unname(tools::md5sum(path)))
  x$source_row <- seq_len(nrow(x))
  for (v in setdiff(variables, names(x))) x[[v]] <- NA_real_
  x$insurance_code <- x[[c("PRIMINSR","PRIMINS1","PRIMINS2","PRIMINS2")[year-2021]]]
  x$race_code <- x[[if (year==2022) "_RACEGR4" else "_RACEGR3"]]
  x$year <- year
  parts[[as.character(year)]] <- x[x[["_STATE"]] %in% states[[as.character(year)]],
    c(variables,"source_row","insurance_code","race_code","year")]
}
d <- do.call(rbind, parts); rownames(d) <- NULL; rm(parts,x)
d$bills <- yes_no(d$SDHBILLS); d$transport <- yes_no(d$SDHTRNSP); d$utility <- yes_no(d$SDHUTILS)
d$uncontrolled <- ifelse(d$CSRVCTL2 %in% 1:4, as.numeric(d$CSRVCTL2 %in% 3:4), NA_real_)
d$age <- cut(valid(d[["_AGEG5YR"]],1:13),c(0,5,9,13),labels=c("18-44","45-64","65+"))
d$income <- cut(valid(d[["_INCOMG1"]],1:7),c(0,2,4,7),labels=c("<$25,000","$25,000-$49,999","$50,000+"))
d$sex <- factor(valid(d[["_SEX"]],1:2),levels=1:2,labels=c("Male","Female"))
d$race <- factor(ifelse(d$race_code %in% c(3,4),3,valid(d$race_code,1:5)),levels=c(1,2,5,3),
  labels=c("White, non-Hispanic","Black, non-Hispanic","Hispanic","Other/multiracial, non-Hispanic"))
d$education <- cut(valid(d$EDUCA,1:6),c(0,4,5,6),labels=c("High school or less","Some college","College graduate"))
d$employment <- factor(ifelse(d$EMPLOY1 %in% 1:2,1,ifelse(d$EMPLOY1 %in% 3:6,2,valid(d$EMPLOY1,7:8))),
  levels=c(1,2,7,8),labels=c("Employed","Other/not employed","Retired","Unable to work"))
d$insured <- ifelse(d$insurance_code %in% 1:10,1,ifelse(d$insurance_code==88,0,NA_real_))
d$insurance_type <- factor(ifelse(d$insurance_code %in% 1:2,"Private",ifelse(d$insurance_code %in% 3:4,
  "Medicare/Medigap",ifelse(d$insurance_code %in% 5:10,"Other public",ifelse(d$insurance_code==88,"None",NA_character_)))) ,
  levels=c("Private","Medicare/Medigap","Other public","None"))
d$MEDCOST1 <- yes_no(d$MEDCOST1); d$mobility <- yes_no(d$DIFFWALK)
disability <- sapply(d[c("DEAF","BLIND","DECIDE","DIFFDRES","DIFFALON")],yes_no)
d$other_disability <- ifelse(rowSums(disability==1,na.rm=TRUE)>0,1,ifelse(rowSums(is.na(disability))==0,0,NA_real_))
d$poor_health <- ifelse(d$GENHLTH %in% 1:5,as.numeric(d$GENHLTH>=4),NA_real_)
d$depression <- yes_no(d$ADDEPEV3)
heart <- ifelse(d$CVDINFR4==1 | d$CVDCRHD4==1,1,ifelse(d$CVDINFR4==2 & d$CVDCRHD4==2,0,NA_real_))
conditions <- cbind(heart,yes_no(d$CVDSTRK3),yes_no(d$CHCCOPD3),yes_no(d$CHCKDNY2),
  ifelse(d$DIABETE4 %in% 1:4,as.numeric(d$DIABETE4==1),NA_real_),yes_no(d$ASTHMA3))
d$comorbidity <- cut(rowSums(conditions),c(-1,0,1,6),labels=c("0","1","2+"))
d$design_ok <- is.finite(d[["_LLCPWT"]]) & d[["_LLCPWT"]]>0 & !is.na(d[["_STSTR"]]) & !is.na(d[["_PSU"]])
d$history <- d$CHCSCNC1 %in% 1 | d$CHCOCNC1 %in% 1
d$current_pain <- d$CSRVPAIN %in% 1
d$final_case <- d$history & d$current_pain & complete.cases(d[c("uncontrolled",hardships,health)])
d$hardship_count <- rowSums(d[hardships]); d$hardship_group <- factor(d$hardship_count,levels=0:3)
d$income_missing <- factor(ifelse(is.na(d$income),"Missing",as.character(d$income)),levels=c(levels(d$income),"Missing"))
d$jurisdiction <- factor(d[["_STATE"]],levels=as.numeric(names(state_names)),labels=state_names)
d$year_factor <- factor(d$year)
d$active_treatment <- ifelse(!(d$year %in% 2022:2024 & d[["_STATE"]]==18) & d$CSRVTRT3 %in% 1:5,
  as.numeric(d$CSRVTRT3==1),NA_real_)
elapsed <- ifelse(d[["_AGE80"]]<80 & d$CNCRAGE %in% 0:97 & d$CNCRAGE<=d[["_AGE80"]],d[["_AGE80"]]-d$CNCRAGE,NA_real_)
d$since_diagnosis <- cut(elapsed,c(-1,4,9,100),labels=c("<5 years","5-9 years","10+ years"))
d$skin_only <- d$CHCSCNC1 %in% 1 & d$CHCOCNC1 %in% 2
d$nmsc_only <- d$skin_only & d$CNCRDIFF %in% 1 & d$CNCRTYP2 %in% 22
# CDC CNCRTYP2 valid responses extend through 30; 23 is skin, unknown subtype.
d$cancer_type <- factor(ifelse(d$CNCRTYP2 %in% 1:30,
  ifelse(d$CNCRTYP2==5,"Breast",ifelse(d$CNCRTYP2==20,"Prostate",
  ifelse(d$CNCRTYP2 %in% c(16,22,23),"Skin","Other"))),NA_character_),
  levels=c("Other","Breast","Prostate","Skin"))
d$pain4 <- factor(valid(d$CSRVCTL2,1:4),levels=1:4,labels=c("Controlled with treatment",
  "Controlled without treatment","Uncontrolled with treatment","Uncontrolled without treatment"))
manifest <- do.call(rbind,manifest); save_csv(manifest,"raw_data_manifest")
saveRDS(dictionary,file.path(root,"Validation","raw_variable_dictionary.rds"))
keep <- rep(TRUE,nrow(d)); counts <- c(sum(manifest$records),nrow(d))
for (flag in list(d$design_ok,d$history,d$current_pain,!is.na(d$uncontrolled),complete.cases(d[hardships]),d$final_case)) {
  keep <- keep & flag; counts <- c(counts,sum(keep))
}
flow <- data.frame(step=c("Source records","Common-module jurisdictions","Valid survey design","Cancer history",
  "Current cancer-related pain","Valid pain-control response","Complete hardship responses","Complete primary covariates"),
  retained_n=counts,excluded_n=c(NA,-diff(counts)))
save_csv(flow,"participant_flow")
d$pooled_weight <- d[["_LLCPWT"]]/4
d$stratum <- as.integer(interaction(d$year,d[["_STATE"]],d[["_STSTR"]],drop=TRUE))
d$psu <- as.integer(interaction(d$year,d[["_STATE"]],d[["_PSU"]],drop=TRUE))
full <- svydesign(ids=~psu,strata=~stratum,weights=~pooled_weight,data=d[d$design_ok,],nest=FALSE)
saveRDS(full,file.path(root,"Validation","full_design.rds"))
primary <- subset(full,final_case)
save_csv(primary$variables[c("year","_STATE","source_row")],"final_cohort_keys")
cat("Prepared primary sample:",nrow(primary$variables),"; events:",sum(primary$variables$uncontrolled),"\n")
