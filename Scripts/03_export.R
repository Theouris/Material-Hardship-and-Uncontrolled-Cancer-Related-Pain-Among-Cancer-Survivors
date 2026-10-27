# Format all publication tables and create title-only vector and raster figures.
source(file.path(dirname(normalizePath(sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]))),"00_setup.R"))
suppressPackageStartupMessages(library(ggplot2))
read_result <- function(name) read.csv(file.path(root,"Validation",paste0(name,".csv")),check.names=FALSE,stringsAsFactors=FALSE)
c <- read_result("model_coefficients"); desc <- read_result("descriptive_results")
manifest <- list()
publish <- function(table,id,title) {
  save_csv(table,id,"Tables + Supplementaries")
  manifest[[length(manifest)+1]] <<- data.frame(id=id,title=title,rows=nrow(table),columns=ncol(table))
}
or_text <- function(z) ci_text(z$OR,z$lower,z$upper)
pct_text <- function(z) ci_text(z$percent,z$lower,z$upper,1)
one <- function(model,term) {
  z <- c[c$model==model & c$term==term,]
  if(nrow(z)==0) return("")
  or_text(z)
}
group_label <- function(v,level) {
  if(v=="poor_health") return(ifelse(level=="1","Fair or poor","Good to excellent"))
  if(v=="insured") return(ifelse(level=="1","Insured","Uninsured"))
  if(v %in% c(hardships,"MEDCOST1","mobility","other_disability","depression","uncontrolled"))
    return(ifelse(level=="1","Yes","No"))
  level
}
z <- desc[desc$variable %in% c(hardships,"MEDCOST1") & desc$category=="1",]
publish(data.frame(Measure=labels[z$variable],`Sample n`=number(z$count),`Weighted N`=number(z$weighted_N),
  `Weighted % (95% CI)`=pct_text(z),check.names=FALSE),"Table_1",
  "Table 1. Material hardship and cost-related doctor-visit barriers in 1,382 respondents")
z <- desc[desc$variable %in% health,]
publish(data.frame(Characteristic=ifelse(duplicated(z$variable),"",labels[z$variable]),
  Category=mapply(group_label,z$variable,z$category),`All respondents n; weighted % (95% CI)`=paste0(number(z$count),"; ",pct_text(z)),
  `Uncontrolled pain n/N; weighted % (95% CI)`=paste0(z$pain_n,"/",z$group_n,"; ",ci_text(z$pain_percent,z$pain_lower,z$pain_upper,1)),
  check.names=FALSE),"Table_2","Table 2. Sample characteristics and within-category prevalence of uncontrolled pain")
z <- desc[desc$variable %in% hardships,]; tests <- read_result("rao_scott_tests"); idx <- match(z$variable,tests$variable)
first <- !duplicated(z$variable)
publish(data.frame(Hardship=ifelse(first,labels[z$variable],""),Status=ifelse(z$category=="1","Yes","No"),
  `Uncontrolled n / group N`=paste(z$pain_n,z$group_n,sep=" / "),
  `Weighted % (95% CI)`=ci_text(z$pain_percent,z$pain_lower,z$pain_upper,1),
  `Rao-Scott F (df1, df2)`=ifelse(first,sprintf("%.2f (%g, %g)",tests$F[idx],tests$df1[idx],tests$df2[idx]),""),
  P=ifelse(first,p_text(tests$P[idx]),""),check.names=FALSE),"Table_3",
  "Table 3. Pooled uncontrolled-pain prevalence by hardship status")
primary <- c[c$model=="Primary",]; n <- primary$n[1]; events <- primary$events[1]
publish(data.frame(Hardship=labels[hardships],`Crude OR (95% CI)`=sapply(hardships,function(v)one(paste("Crude",v),v)),
  `Age sex income aOR (95% CI)`=sapply(hardships,function(v)one("Age sex income",v)),
  `Primary aOR (95% CI)`=sapply(hardships,function(v)one("Primary",v)),check.names=FALSE),"Table_4",
  paste0("Table 4. Crude and joint adjusted odds ratios in ",number(n)," respondents with ",events," uncontrolled-pain events"))
full <- readRDS(file.path(root,"Validation","full_design.rds")); d <- subset(full,final_case)$variables
term_details <- function(term) {
  if(term=="(Intercept)") return(c("Intercept","Baseline odds"))
  if(term %in% names(labels)) return(c(labels[[term]],if(term=="poor_health")"Fair/poor vs good to excellent" else if(term=="insured")"Insured vs uninsured" else "Yes vs no"))
  for(v in health[order(nchar(health),decreasing=TRUE)]) if(startsWith(term,v))
    return(c(labels[[v]],paste(substring(term,nchar(v)+1),"vs",levels(d[[v]])[1])))
  c(term,"")
}
details <- t(sapply(primary$term,term_details))
publish(data.frame(Predictor=ifelse(duplicated(details[,1]),"",details[,1]),Comparison=details[,2],
  `aOR or baseline odds (95% CI)`=or_text(primary),check.names=FALSE),"Table_S1",
  paste0("Supplementary Table S1. Full primary model in ",number(n)," respondents with ",events," events"))
coverage <- read_result("jurisdiction_coverage")
coverage_table <- coverage[c("year","jurisdiction","dataset","person_weight","pain_social","treatment")]
names(coverage_table) <- c("Year","Jurisdiction","Dataset","Person weight","Pain and social needs","Course of treatment")
publish(coverage_table,"Table_S2",
  "Supplementary Table S2. Main-file module coverage and applicable person weights")
z <- read_result("missingness")
publish(data.frame(Variable=labels[z$variable],`Eligible N`=z$n,`Missing n`=z$missing_n,
  `Missing %`=sprintf("%.1f",100*z$missing_n/z$n),check.names=FALSE),"Table_S3",
  "Supplementary Table S3. Missing responses among 1,965 respondents with current cancer-related pain")
z <- read_result("included_excluded"); a <- z[z$included,]; b <- z[!z$included,]
stopifnot(identical(a$variable,b$variable),identical(a$category,b$category))
publish(data.frame(Characteristic=ifelse(duplicated(a$variable),"",labels[a$variable]),
  Category=mapply(group_label,a$variable,a$category),
  `Included n/N; weighted % (95% CI)`=paste0(a$count,"/",a$n,"; ",pct_text(a)),
  `Excluded n/N; weighted % (95% CI)`=paste0(b$count,"/",b$n,"; ",pct_text(b)),check.names=FALSE),"Table_S4",
  "Supplementary Table S4. Available-case comparison of 1,382 included and 465 excluded respondents")
wide_models <- function(names) {
  rows <- lapply(names,function(name) {
    z <- c[c$model==name,]
    data.frame(Model=name,`n / events`=paste(z$n[1],z$events[1],sep=" / "),
      `Bills OR (95% CI)`=one(name,"bills"),`Transport OR (95% CI)`=one(name,"transport"),
      `Utility OR (95% CI)`=one(name,"utility"),check.names=FALSE)
  }); do.call(rbind,rows)
}
publish(wide_models(c("Joint hardships only","Demographic","Socioeconomic","Access","Mobility","Primary",
  paste("Separate",hardships))),"Table_S5","Supplementary Table S5. Sequential joint and separate-hardship models")
z <- read_result("hardship_overlap")
publish(data.frame(Pair=paste(labels[z$first],labels[z$second],sep=" and "),
  `First / second hardship`=paste(ifelse(z$first_yes==1,"Yes","No"),ifelse(z$second_yes==1,"Yes","No"),sep=" / "),
  n=z$count,`Weighted % (95% CI)`=pct_text(z),`Weighted phi`=sprintf("%.3f",z$phi),check.names=FALSE),
  "Table_S6","Supplementary Table S6. Pairwise hardship overlap and weighted binary correlations")
z <- read_result("collinearity")
publish(data.frame(Predictor=labels[z$term],`Term df`=z$df,GVIF=sprintf("%.3f",z$GVIF),
  `GVIF power 1/(2 df)`=sprintf("%.3f",z$scaled_GVIF),check.names=FALSE),"Table_S7",
  "Supplementary Table S7. Collinearity diagnostics for the primary joint model")
z <- read_result("cumulative_hardship")
publish(data.frame(Hardships=z$hardships,`Sample n`=z$count,`Weighted % (95% CI)`=pct_text(z),
  `Uncontrolled n/N`=paste(z$pain_n,z$count,sep="/"),
  `Pain weighted % (95% CI)`=ci_text(z$pain_percent,z$pain_lower,z$pain_upper,1),
  `Primary adjusted OR (95% CI)`=c("1.00 (reference)",sapply(1:3,function(k)one("Cumulative categories",paste0("hardship_group",k)))),
  check.names=FALSE),"Table_S8","Supplementary Table S8. Cumulative hardship and uncontrolled pain")
sens <- c("Without doctor cost barrier","Without function measures","Insurance type","Jurisdiction adjustment",
  paste("Omit",2022:2025),"Year sample share","Jurisdiction repetition","Missing income retained",
  "Cancer subset baseline","Cancer covariates added")
publish(wide_models(sens),"Table_S9","Supplementary Table S9. Sensitivity analyses with sample sizes and events")
z <- read_result("interaction_tests")
publish(data.frame(Test=ifelse(z$hardship %in% hardships,labels[z$hardship],z$hardship),
  `Wald F`=sprintf("%.3f",z$F),`Numerator df`=z$df1,`Denominator df`=z$df2,P=p_text(z$P),check.names=FALSE),
  "Table_S10","Supplementary Table S10. Exploratory hardship-by-year interaction tests")
z <- read_result("four_pain_categories")
groups <- ifelse(z$variable=="Overall","Overall",paste0(labels[z$variable],": ",ifelse(z$hardship==1,"Yes","No")))
four_table <- data.frame(Group=unique(groups),check.names=FALSE)
for(code in 1:4) {
  a <- z[z$pain_code==code,]
  four_table[[paste0(a$category[1],"\nn/N; weighted % (95% CI)")]] <- paste0(a$count,"/",a$n,"; ",pct_text(a))
}
publish(four_table,"Table_S11",
  "Supplementary Table S11. Four pain-control categories overall and by hardship")
z <- read_result("multinomial_coefficients"); z <- z[z$term %in% hardships,]
publish(data.frame(Outcome=ifelse(duplicated(z$outcome),"",z$outcome),Hardship=labels[z$term],
  `Category n / total N`=paste(z$category_n,z$n,sep=" / "),`Adjusted RRR (95% CI)`=ci_text(z$RRR,z$lower,z$upper),check.names=FALSE),
  "Table_S12","Supplementary Table S12. Multinomial contrasts with controlled pain as the reference")
z <- read_result("annual_group_denominators")
publish(data.frame(Year=z$year,Hardship=labels[z$hardship],Status=ifelse(z$status==1,"Yes","No"),
  `Group N`=z$n,`Uncontrolled n`=z$events,check.names=FALSE),"Table_S13",
  "Supplementary Table S13. Unweighted annual group denominators for the pooled analysis")
z <- c[!duplicated(c$model),]
publish(data.frame(Model=z$model,N=z$n,Events=z$events,Parameters=z$parameters,`Design df`=z$design_df,
  `Residual df`=z$residual_df,Converged=z$converged,check.names=FALSE),"Table_S14",
  "Supplementary Table S14. Logistic-model sample sizes and survey design diagnostics")
z <- read_result("hardship_coding_audit")
publish(data.frame(Hardship=labels[z$variable],`Original code`=ifelse(is.na(z$raw),"Missing",z$raw),
  `Derived code (0=no 1=yes)`=ifelse(is.na(z$derived),"Missing",z$derived),N=z$Freq,check.names=FALSE),
  "Table_S15","Supplementary Table S15. Original and derived hardship codes in the current-pain domain")
save_csv(do.call(rbind,manifest),"publication_manifest")

# Figures contain a title and chart labels only. The manuscript supplies all methods.
save_plot <- function(plot,id,width=8,height=4.6) {
  ggsave(file.path(root,"Figures",paste0(id,".pdf")),plot,width=width,height=height,device="pdf")
  ggsave(file.path(root,"Figures",paste0(id,".png")),plot,width=width,height=height,dpi=400,bg="white")
  ggsave(file.path(root,"Figures",paste0(id,".tiff")),plot,width=width,height=height,dpi=600,compression="lzw",bg="white")
}
forest <- function(names,title,id) {
  z <- do.call(rbind,lapply(names,function(name)c[c$model==name & c$term %in% hardships,]))
  z$hardship <- factor(labels[z$term],levels=rev(labels[hardships]))
  z$estimate <- or_text(z)
  plot <- ggplot(z,aes(x=OR,y=hardship))+geom_vline(xintercept=1,linetype=2,color="grey50")+
    geom_errorbar(aes(xmin=lower,xmax=upper),orientation="y",width=.12,linewidth=.7)+geom_point(size=3,color="#1C4C6B")+
    geom_text(aes(x=10,label=estimate),hjust=0,size=3.8)+
    scale_x_log10(breaks=c(.25,.5,1,2,4,8))+coord_cartesian(xlim=c(.25,8),clip="off")+
    labs(title=title,x="Odds ratio (95% confidence interval)",y=NULL)+theme_classic(base_size=12)+
    theme(plot.title=element_text(size=13,face="bold",margin=margin(b=20)),axis.text.y=element_text(color="black"),
      plot.margin=margin(15,125,12,12))
  save_plot(plot,id)
}
forest(paste("Crude",hardships),"Figure 2. Crude odds ratios for uncontrolled cancer-related pain","Figure_2")
forest("Age sex income","Figure 3. Joint odds ratios adjusted for age sex and income","Figure_3")
forest("Primary","Figure 4. Primary expanded joint adjusted odds ratios","Figure_4")
flow <- read_result("participant_flow")
png(file.path(root,"Figures","Figure_1.png"),width=2400,height=3000,res=350)
draw_flow <- function() {
  par(mar=c(0,0,2,0)); plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  title(main="Figure 1. Participant flow for pooled BRFSS 2022-2025",cex.main=.95,line=.5)
  yy <- seq(.94,.08,length.out=nrow(flow))
  for(i in seq_len(nrow(flow))) {
    rect(.06,yy[i]-.035,.72,yy[i]+.035,col="#F2F5F7",border="#7B8D99")
    text(.39,yy[i],paste0(flow$step[i],"\nn = ",number(flow$retained_n[i])),cex=.9)
    if(i<nrow(flow)) {
      arrows(.39,yy[i]-.035,.39,yy[i+1]+.038,length=.07)
      if(flow$excluded_n[i+1]>0) text(.77,mean(yy[c(i,i+1)]),paste0("Excluded\nn = ",number(flow$excluded_n[i+1])),cex=.8,adj=0)
    }
  }
}
draw_flow(); dev.off()
pdf(file.path(root,"Figures","Figure_1.pdf"),width=7,height=8.8); draw_flow(); dev.off()
tiff(file.path(root,"Figures","Figure_1.tiff"),width=7,height=8.8,units="in",res=600,compression="lzw");draw_flow();dev.off()
draw_dag <- function() {
  par(mar=c(0,0,3,0));plot.new();plot.window(xlim=c(0,1),ylim=c(0,1))
  title(main="Supplementary Figure S1. Hypothesized forward pathways",cex.main=.95,line=1)
  boxes <- list(c(.5,.88),c(.15,.53),c(.5,.53),c(.85,.53))
  txt <- c("Preexisting demographic\nsocioeconomic and\nhealth factors","Material hardship","Cost-related barriers\nto doctor visits","Pain control")
  edge <- function(i,j) {
    a<-boxes[[i]];b<-boxes[[j]];delta<-b-a;t<-max(abs(delta/c(.145,.07)))
    arrows(a[1]+delta[1]/t,a[2]+delta[2]/t,b[1]-delta[1]/t,b[2]-delta[2]/t,length=.08,col="#556873")
  }
  for(pair in list(c(1,2),c(1,3),c(1,4),c(2,3),c(3,4))) edge(pair[1],pair[2])
  segments(.15,.465,.15,.32,col="#556873");segments(.15,.32,.85,.32,col="#556873")
  arrows(.85,.32,.85,.465,length=.08,col="#556873")
  for(i in 1:4) { a<-boxes[[i]];rect(a[1]-.14,a[2]-.065,a[1]+.14,a[2]+.065,col="#F2F5F7",border="#7B8D99");text(a[1],a[2],txt[i],cex=.82) }
}
png(file.path(root,"Figures","Figure_S1.png"),width=3200,height=2200,res=400);draw_dag();dev.off()
pdf(file.path(root,"Figures","Figure_S1.pdf"),width=9,height=6);draw_dag();dev.off()
tiff(file.path(root,"Figures","Figure_S1.tiff"),width=9,height=6,units="in",res=600,compression="lzw");draw_dag();dev.off()
cat("Exported",length(manifest),"tables and five figures\n")
