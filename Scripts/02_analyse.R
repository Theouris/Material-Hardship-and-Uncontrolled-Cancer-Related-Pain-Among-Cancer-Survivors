# Ordinary survey models and reviewer analyses; no custom estimation algorithms.
source(file.path(dirname(normalizePath(sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]))),"00_setup.R"))
full <- readRDS(file.path(root,"Validation","full_design.rds"))
primary <- subset(full,final_case); d <- primary$variables
eligible <- subset(full,history & current_pain)
model_rows <- list(); models <- list()
fit <- function(design, exposure, covariates, name, response="uncontrolled") {
  design$variables <- droplevels(design$variables)
  formula <- reformulate(c(exposure,covariates),response=response)
  stopifnot(all(complete.cases(design$variables[all.vars(formula)])))
  m <- quiet(svyglm(formula,design,family=quasibinomial(),control=glm.control(epsilon=1e-12)))
  a <- quiet(summary(m)$coefficients); critical <- qt(.975,m$df.residual)
  z <- data.frame(model=name,term=rownames(a),log_OR=a[,1],SE=a[,2],OR=exp(a[,1]),
    lower=exp(a[,1]-critical*a[,2]),upper=exp(a[,1]+critical*a[,2]),
    n=nobs(m),events=sum(design$variables[[response]]),parameters=m$rank,
    design_df=degf(design),residual_df=m$df.residual,converged=m$converged,
    formula=paste(deparse(formula),collapse=""),row.names=NULL)
  stopifnot(m$converged,m$df.residual>0,all(is.finite(z$OR)))
  model_rows[[name]] <<- z; models[[name]] <<- m
  invisible(m)
}
cat("Fitting crude, intermediate, sequential, primary, and separate models\n")
for (v in hardships) fit(primary,v,character(),paste("Crude",v))
fit(primary,hardships,c("age","sex","income"),"Age sex income")
blocks <- list("Joint hardships only"=character(),"Demographic"=demographic,
  "Socioeconomic"=socioeconomic,"Access"=access,"Mobility"=c(access,"mobility"),"Primary"=health)
for (name in names(blocks)) fit(primary,hardships,blocks[[name]],name)
for (v in hardships) fit(primary,v,health,paste("Separate",v))
fit(primary,"hardship_group",health,"Cumulative categories")
fit(primary,"hardship_count",health,"Cumulative linear")

cat("Fitting missing-data, cancer-covariate, and geographic sensitivity models\n")
sensitivities <- list("Without doctor cost barrier"=setdiff(health,"MEDCOST1"),
  "Without function measures"=setdiff(health,c("mobility","other_disability")),
  "Insurance type"=c(setdiff(health,"insured"),"insurance_type"),
  "Jurisdiction adjustment"=c(health,"jurisdiction"))
for (name in names(sensitivities)) fit(primary,hardships,sensitivities[[name]],name)
for (year in 2022:2025) fit(primary[d$year!=year,],hardships,health,paste("Omit",year))
income_covariates <- replace(health,health=="income","income_missing")
income_domain <- subset(full,history & current_pain & !is.na(uncontrolled) & !is.na(bills) & !is.na(transport) & !is.na(utility))
income_domain <- income_domain[complete.cases(income_domain$variables[income_covariates]),]
fit(income_domain,hardships,income_covariates,"Missing income retained")
cancer <- primary[complete.cases(d[c("active_treatment","since_diagnosis","cancer_type")]),]
fit(cancer,hardships,health,"Cancer subset baseline")
fit(cancer,hardships,c(health,"active_treatment","since_diagnosis","cancer_type"),"Cancer covariates added")
# Two alternative targets test the sensitivity to recurring jurisdictions and year mixture.
year_n <- table(full$variables$year)
repetitions <- table(unique(full$variables[c("year","_STATE")])[["_STATE"]])
for (name in c("Year sample share","Jurisdiction repetition")) {
  z <- full$variables
  z$alternative_weight <- if (name=="Year sample share") z[["_LLCPWT"]]*as.numeric(year_n[as.character(z$year)])/sum(year_n)
    else z[["_LLCPWT"]]/as.numeric(repetitions[as.character(z[["_STATE"]])])
  alternate <- svydesign(ids=~psu,strata=~stratum,weights=~alternative_weight,data=z)
  fit(subset(alternate,final_case),hardships,health,name)
}
# Year appears only in this exploratory composition-interaction model.
interaction_model <- fit(primary,hardships,c(health,"year_factor",paste0(hardships,":year_factor")),"Year interactions")
interaction_rows <- list()
for (v in c(hardships,"All hardships")) {
  terms <- paste0(if (v=="All hardships") hardships else v,":year_factor")
  test <- quiet(regTermTest(interaction_model,reformulate(terms),method="Wald"))
  interaction_rows[[v]] <- data.frame(hardship=v,F=as.numeric(test$Ftest),df1=test$df,df2=test$ddf,P=as.numeric(test$p))
}
save_csv(do.call(rbind,interaction_rows),"interaction_tests")
source(file.path(script_dir,"multinomial.R"))
coefficients <- do.call(rbind,model_rows); rownames(coefficients) <- NULL
save_csv(coefficients,"model_coefficients")

cat("Estimating descriptive results and available-case comparisons\n")
overall <- prop(primary,d$uncontrolled==1); save_csv(overall,"overall_prevalence")
descriptive <- list()
for (v in c(hardships,health)) {
  x <- d[[v]]; levels_x <- if(is.factor(x)) levels(x) else sort(unique(x))
  for (level in levels_x) {
    group <- x==level
    all <- prop(primary,group); pain <- prop(primary[group,],d$uncontrolled[group]==1)
    descriptive[[length(descriptive)+1]] <- data.frame(variable=v,category=as.character(level),all,
      pain_n=pain$count,group_n=pain$n,pain_percent=pain$percent,pain_lower=pain$lower,pain_upper=pain$upper)
  }
}
descriptive <- do.call(rbind,descriptive); save_csv(descriptive,"descriptive_results")
tests <- lapply(hardships,function(v) {
  t <- quiet(svychisq(reformulate(c(v,"uncontrolled")),primary,statistic="F"))
  data.frame(variable=v,F=as.numeric(t$statistic),df1=t$parameter[1],df2=t$parameter[2],P=t$p.value)
})
save_csv(do.call(rbind,tests),"rao_scott_tests")
missing <- data.frame(variable=c("uncontrolled",hardships,health),n=nrow(eligible$variables),
  missing_n=sapply(eligible$variables[c("uncontrolled",hardships,health)],function(x)sum(is.na(x))))
save_csv(missing,"missingness")
included <- list()
comparison <- subset(eligible,!is.na(uncontrolled) & !is.na(bills) & !is.na(transport) & !is.na(utility))
for (v in c("uncontrolled",hardships,health)) {
  x <- comparison$variables[[v]]; lev <- if(is.factor(x)) levels(x) else sort(unique(x[!is.na(x)]))
  for (level in lev) for (in_sample in c(TRUE,FALSE)) {
    keep <- comparison$variables$final_case==in_sample & !is.na(x)
    a <- prop(comparison[keep,],x[keep]==level)
    included[[length(included)+1]] <- data.frame(variable=v,category=as.character(level),included=in_sample,a)
  }
}
save_csv(do.call(rbind,included),"included_excluded")
overlap <- list(); correlation <- cov2cor(as.matrix(quiet(svyvar(reformulate(hardships),primary))))
for (pair in combn(hardships,2,simplify=FALSE)) for (a in 0:1) for (b in 0:1) {
  overlap[[length(overlap)+1]] <- data.frame(first=pair[1],second=pair[2],first_yes=a,second_yes=b,
    phi=correlation[pair[1],pair[2]],prop(primary,d[[pair[1]]]==a & d[[pair[2]]]==b))
}
save_csv(do.call(rbind,overlap),"hardship_overlap")
# Generalized VIF from the coefficient correlation matrix, excluding the intercept.
V <- vcov(models$Primary)[-1,-1]; R <- cov2cor(V)
assignment <- attr(model.matrix(models$Primary),"assign")[-1]
terms <- attr(terms(models$Primary),"term.labels")
logdet <- function(x) as.numeric(determinant(x,logarithm=TRUE)$modulus)
gvif <- lapply(seq_along(terms),function(i) {
  idx <- which(assignment==i); other <- setdiff(seq_len(nrow(R)),idx)
  value <- exp(logdet(R[idx,idx,drop=FALSE])+logdet(R[other,other,drop=FALSE])-logdet(R))
  data.frame(term=terms[i],df=length(idx),GVIF=value,scaled_GVIF=value^(1/(2*length(idx))))
})
save_csv(do.call(rbind,gvif),"collinearity")
cumulative <- lapply(0:3,function(k) {
  keep <- d$hardship_count==k; a <- prop(primary,keep); b <- prop(primary[keep,],d$uncontrolled[keep]==1)
  data.frame(hardships=k,a,pain_n=b$count,pain_percent=b$percent,pain_lower=b$lower,pain_upper=b$upper)
})
save_csv(do.call(rbind,cumulative),"cumulative_hardship")
pain4 <- list()
for (v in c("Overall",hardships)) for (a in if(v=="Overall") 0 else 0:1) {
  keep <- if(v=="Overall") rep(TRUE,nrow(d)) else d[[v]]==a
  for (code in 1:4) pain4[[length(pain4)+1]] <- data.frame(variable=v,hardship=a,pain_code=code,
    category=levels(d$pain4)[code],prop(primary[keep,],d$CSRVCTL2[keep]==code))
}
save_csv(do.call(rbind,pain4),"four_pain_categories")
coverage <- unique(full$variables[c("year","_STATE")]); coverage <- coverage[order(coverage$year,coverage[["_STATE"]]),]
coverage$jurisdiction <- state_names[as.character(coverage[["_STATE"]])]
coverage$dataset <- paste0("LLCP",coverage$year); coverage$person_weight <- "_LLCPWT"
coverage$pain_social <- "Main file"; coverage$treatment <- ifelse(coverage$year %in% 2022:2024 & coverage[["_STATE"]]==18,"Not in public module","Main file")
coverage$source <- paste0("https://www.cdc.gov/brfss/questionnaires/modules/category",coverage$year,".htm")
coverage$primary_n <- coverage$events <- numeric(nrow(coverage))
for(i in seq_len(nrow(coverage))) {
  keep <- d$year==coverage$year[i] & d[["_STATE"]]==coverage[["_STATE"]][i]
  coverage$primary_n[i] <- sum(keep); coverage$events[i] <- sum(d$uncontrolled[keep])
}
save_csv(coverage,"jurisdiction_coverage")
denominators <- list()
for (year in 2022:2025) for(v in hardships) for(a in 0:1) {
  keep <- d$year==year & d[[v]]==a
  denominators[[length(denominators)+1]] <- data.frame(year=year,hardship=v,status=a,n=sum(keep),events=sum(d$uncontrolled[keep]))
}
save_csv(do.call(rbind,denominators),"annual_group_denominators")
eligibility <- data.frame(measure=c("Skin cancer only","Identified single nonmelanoma skin cancer only"),
  n=c(sum(d$skin_only),sum(d$nmsc_only)))
save_csv(eligibility,"skin_cancer_eligibility")
mapping <- list()
for(v in hardships) {
  raw <- eligible$variables[[c(bills="SDHBILLS",transport="SDHTRNSP",utility="SDHUTILS")[[v]]]]
  mapped <- eligible$variables[[v]]
  tab <- as.data.frame(table(raw=raw,derived=mapped,useNA="ifany"))
  mapping[[v]] <- data.frame(variable=v,tab[tab$Freq>0,])
}
save_csv(do.call(rbind,mapping),"hardship_coding_audit")
cells <- list()
for(v in c(hardships,health)) {
  tab <- as.data.frame(table(year=d$year,category=d[[v]],uncontrolled=d$uncontrolled))
  cells[[v]] <- data.frame(variable=v,tab)
}
save_csv(do.call(rbind,cells),"annual_outcome_cells")
availability <- data.frame(variable=c("cancer_type","active_treatment","since_diagnosis"),
  available_n=sapply(d[c("cancer_type","active_treatment","since_diagnosis")],function(x)sum(!is.na(x))),
  missing_n=sapply(d[c("cancer_type","active_treatment","since_diagnosis")],function(x)sum(is.na(x))))
save_csv(availability,"cancer_covariate_availability")
writeLines(warnings_seen,file.path(root,"Validation","analysis_warnings.txt"))
saveRDS(models,file.path(root,"Validation","fitted_models.rds"))
writeLines(capture.output(sessionInfo()),file.path(root,"Validation","session_info.txt"))
cat("Completed",length(models),"survey models\n")
