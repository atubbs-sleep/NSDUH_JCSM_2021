####BZRA Analysis
###Load Packages
library(tidyverse)
library(ggpubr)
library(survey)
library(broom)
library(CBPS)
library(cobalt)
library(gtsummary)
library(WeightIt)
###Load the data
bzra <- read_rds("c:/users/atala/sync/research/projects/to publish/nsduh/bzra.rdata")
bzra.data <- bzra %>% dplyr::select("ID" = QUESTID2, "Year" = year, "Weight4" = ANALWC4, "PSU" = verep, "Strata" = vestr,
                    "Age" = CATAG6, "Race" = NEWRACE2, "Health" = HEALTH2, "Marital" = irmarit,
                    "Sex" = irsex, "Income" = income,"K6Score" = K6SCMAX,"WHODAS" = WHODASC2,
                    "PYRZolpidem" = zolppdapyu,"PYREszopiclone" = eszopdapyu,"PYRZaleplon" = zalepdapyu,
                    "PYRTriazolam" = triapdapyu,"PYRTemazepam" = temapdapyu,"PYRFlurazepam" = flurpdapyu,
                    "PYRSuicIdea" = mhsuithk, "PYRSuicPlan" = mhsuipln, "PYRSuicAttempt" = mhsuitry,
                    "PYRDepression" = AMDEY2_U,"PYRIllicit" = illyr)
                    
### Recode Variables
bzra.data$ID <- as_factor(bzra.data$ID)
bzra.data$Year <- as_factor(bzra.data$Year)
bzra.data$PYRIllicit <- factor(bzra.data$PYRIllicit, levels = c(0,1), labels = c("No","Yes"))
bzra.data <- bzra.data %>% filter(Age!=1)
bzra.data$Age <- factor(bzra.data$Age, levels = c(2,3,4,5,6),
                        labels = c("18-25","26-34","35-49","50-64",">65"))
bzra.data$Race <- factor(bzra.data$Race, levels = c(1,2,7,3,4,5,6),
                         labels = c("White","Black","Hispanic","Native American","Pacific Islander","Asian","Other"))
bzra.data$Health <- factor(bzra.data$Health, levels = c(1,2,3,4), exclude = c(-9,"."),
                           labels = c("Excellent","Very Good","Good","Fair/Poor"))
bzra.data$Marital <- factor(bzra.data$Marital, levels = c(4,1,2,3), exclude = c(-9, 99),
                            labels = c("Never Married","Married","Widowed","Divorced or Separated"))
bzra.data$Sex <- factor(bzra.data$Sex, levels = c(1,2), labels = c("Male","Female"))
bzra.data$Income <- factor(bzra.data$Income, levels = c(4,3,2,1), 
                           labels = c(">75K","50-75K","20-50K","<20K"))
bzra.data$PYRSuicIdea <- factor(bzra.data$PYRSuicIdea, levels = c(0,1),labels = c("No","Yes"))
bzra.data$PYRSuicPlan <- factor(bzra.data$PYRSuicPlan, levels = c(0,1),labels = c("No","Yes"))
bzra.data$PYRSuicAttempt <- factor(bzra.data$PYRSuicAttempt, levels = c(0,1),labels = c("No","Yes"))
bzra.data <- mutate(bzra.data, "Zdrug" = case_when(PYRZolpidem==1~1,PYREszopiclone==1~1,PYRZaleplon==1~1,
                                                  PYRZolpidem==0&PYREszopiclone==0&PYRZaleplon==0~0,
                                                  PYRZolpidem==-9|PYREszopiclone==-9|PYRZaleplon==-9~-9))
bzra.data <- mutate(bzra.data, "SedBenzo" = case_when(PYRTriazolam==1~1,PYRTemazepam==1~1,PYRFlurazepam==1~1,
                                                      PYRTriazolam==0&PYRTemazepam==0&PYRFlurazepam==0~0,
                                                      PYRTriazolam==-9|PYRTemazepam==-9|PYRFlurazepam==-9~-9))
bzra.data$PYRDepression <- factor(bzra.data$PYRDepression, levels=c(0,1),exclude=c(".",-9),
                                  labels = c("No","Yes"))
bzra.data <- bzra.data %>% filter(is.na(Zdrug)==F)
bzra.data <- mutate(bzra.data, PYRSuicPlan = if_else(PYRSuicIdea=="No","No",as.character(PYRSuicPlan)),
                    PYRSuicAttempt = if_else(PYRSuicIdea=="No","No",as.character(PYRSuicAttempt)),
                    PYRSuicAttempt = if_else(PYRSuicPlan=="No","No",as.character(PYRSuicAttempt)))
bzra.data$PYRSuicPlan <- factor(bzra.data$PYRSuicPlan, levels = c("No","Yes"))
bzra.data$PYRSuicAttempt <- factor(bzra.data$PYRSuicAttempt, levels = c("No","Yes"))
bzra.data <- bzra.data %>% filter(is.na(PYRSuicIdea)==F,is.na(PYRSuicPlan)==F,is.na(PYRSuicAttempt)==F)
bzra.data <- bzra.data %>% mutate("Sedatives" = factor(case_when(SedBenzo==1 & Zdrug==1 ~ "SedBenzo + Zdrug",
                                                          SedBenzo==1 & Zdrug ==0 ~ "SedBenzo", 
                                                          SedBenzo==0 & Zdrug==1~"Zdrug",
                                                          SedBenzo==0&Zdrug==0~"None"),
                                                       levels = c("None","Zdrug","SedBenzo","SedBenzo + Zdrug")))
### Population Weighted Summary Statistics ###
## Form survey design object on all data
bzra.design <-  svydesign(id= ~PSU, strata = ~Strata, weights=~Weight4, data = bzra.data,nest=T)

# Sample Summary Table
tbl_merge(tab_spanner = c("Overall","Suicidal Ideation","Suicide Planning","Suicide Attempt"),list(
  tbl_svysummary(bzra.design,include=c("Age","Sex","Race","Year","Sedatives"),missing="no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n_unweighted} ({p}%)")),
  tbl_svysummary(bzra.design,by="PYRSuicIdea",include=c("PYRSuicIdea","Age","Sex","Race","Year","Sedatives"),missing="no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n_unweighted} ({p}%)")) %>%
    add_p(test = list(all_continuous() ~ "svy.t.test",all_categorical() ~ "svy.chisq.test")),
  tbl_svysummary(bzra.design,by="PYRSuicPlan",include=c("PYRSuicPlan","Age","Sex","Race","Year","Sedatives"),missing="no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n_unweighted} ({p}%)")) %>%
    add_p(test = list(all_continuous() ~ "svy.t.test",all_categorical() ~ "svy.chisq.test")),
  tbl_svysummary(bzra.design,by="PYRSuicAttempt",include=c("PYRSuicAttempt","Age","Sex","Race","Year","Sedatives"),missing="no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n_unweighted} ({p}%)")) %>%
    add_p(test = list(all_continuous() ~ "svy.t.test",all_categorical() ~ "svy.chisq.test"))
))

## Prevalence of Sleep Aid use by STB
# Create Table
subs <- c("Zdrug","SedBenzo")
suic <- c("PYRSuicIdea","PYRSuicPlan","PYRSuicAttempt")
demotable <- tibble("Index" = 1:4)
for(jdx in 1:length(suic)){
  temptable2 <- tibble()
  for(idx in 1:length(subs)){
    temptable <- svyby(as.formula(paste("~",suic[jdx],sep="")),as.formula(paste("~",subs[idx],sep="")),bzra.design,svymean,vartype="ci",na.rm=T) %>%
      gather(key="Demo",value="Value",-1) %>% separate(Demo,into=c("Demo","Level"),sep=suic[jdx]) %>%
      spread(key="Demo",value="Value") %>%
      dplyr::select(Level,"Mean" = V1, "Lower" = ci_l.,"Upper" = ci_u.) %>%
      mutate("Mean" = round(Mean*100,2),"Lower"=round(Lower*100,2),"Upper"=round(Upper*100,2),
             "95% CI" = paste("[",Lower,", ",Upper,"]",sep="")) %>% filter(Level=="Yes") %>% dplyr::select(!Level)
    temptable <- crossing("Demo" = suic[jdx], "Subs" = subs[idx],temptable)
    temptable2 <- bind_rows(temptable2,temptable)
    print(paste("Completed",subs[idx],"for",suic[jdx]))
  }
  demotable <- bind_cols(demotable, temptable2)
}
#write_csv(demotable,"c:/users/atala/sync/research/projects/to publish/nsduh/zdrugs and suicide/table1.csv")
# Barplot
ggarrange(nrow=3,ncol=1,labels="AUTO",common.legend = T,
ggplot(svyby(~PYRSuicIdea,~Sedatives,bzra.design,svymean,na.rm=T,vartype="ci")%>%
         mutate("Sedatives" = factor(Sedatives,levels = c("None","Zdrug","SedBenzo","SedBenzo + Zdrug"))), 
       aes(x=Sedatives,y=PYRSuicIdeaYes))+
  geom_col(fill="Grey",color="Black")+
  geom_errorbar(aes(ymin=ci_l.PYRSuicIdeaYes,ymax=ci_u.PYRSuicIdeaYes),width=.5)+
  scale_y_continuous(labels=scales::percent)+theme_pubr()+xlab("")+
  ylab("% Suicidal Ideation"),
ggplot(svyby(~PYRSuicPlan,~Sedatives,bzra.design,svymean,na.rm=T,vartype="ci")%>%
         mutate("Sedatives" = factor(Sedatives,levels = c("None","Zdrug","SedBenzo","SedBenzo + Zdrug"))), 
       aes(x=Sedatives,y=PYRSuicPlanYes))+
  geom_col(fill="Grey",color="Black")+
  geom_errorbar(aes(ymin=ci_l.PYRSuicPlanYes,ymax=ci_u.PYRSuicPlanYes),width=.5)+
  scale_y_continuous(labels=scales::percent)+theme_pubr()+xlab("")+
  ylab("% Suicide Planning"),
ggplot(svyby(~PYRSuicAttempt,~Sedatives,bzra.design,svymean,na.rm=T,vartype="ci")%>%
         mutate("Sedatives" = factor(Sedatives,levels = c("None","Zdrug","SedBenzo","SedBenzo + Zdrug"))), 
       aes(x=Sedatives,y=PYRSuicAttemptYes))+
  geom_col(fill="Grey",color="Black")+
  geom_errorbar(aes(ymin=ci_l.PYRSuicAttemptYes,ymax=ci_u.PYRSuicAttemptYes),width=.5)+
  scale_y_continuous(labels=scales::percent)+theme_pubr()+xlab("")+
  ylab("% Suicide Attempt")
)

### Generate Propensity Score Weighting variables for Zdrugs and Sedative Benzodiazepines
### Propensity score matching for Zdrugs
set.seed(627)
zdrug.nsduh.scores1 <- weightit(Zdrug~Age+Race+Sex+Income,bzra.data,
                               method="gbm",stabilize=T,s.weights = "Weight4",estimand = "ATE",stop.method="es.mean")
zdrug.nsduh.scores2 <- weightit(Zdrug~Age+Race+Sex+Income+PYRDepression+PYRIllicit+K6Score+WHODAS,bzra.data,
                         method="gbm",stabilize=T,s.weights = "Weight4",estimand = "ATE",stop.method="es.mean")
bzra.data$Zweights1 <- zdrug.nsduh.scores1$weights
bzra.data$Zweights2 <- zdrug.nsduh.scores2$weights
# Make survey design object using propensity scores weighted on Z-drug use
zdrug.nsduh.design1 <-  svydesign(id= ~PSU, strata = ~Strata, weights=~Zweights1, data = bzra.data,nest=T)
zdrug.nsduh.design2 <-  svydesign(id= ~PSU, strata = ~Strata, weights=~Zweights2, data = bzra.data,nest=T)
### Propensity score matching for SedBenzos
sedbenzo.nsduh.scores1 <- weightit(SedBenzo~Age+Race+Sex+Income,bzra.data,
                            method="gbm",stabilize=T,s.weights = "Weight4",estimand = "ATE",stop.method="es.mean")
sedbenzo.nsduh.scores2 <- weightit(SedBenzo~Age+Race+Sex+Income+PYRDepression+PYRIllicit+K6Score+WHODAS,bzra.data,
                            method="gbm",stabilize=T,s.weights = "Weight4",estimand = "ATE",stop.method="es.mean")
bzra.data$SBweights1 <- sedbenzo.nsduh.scores1$weights
bzra.data$SBweights2 <- sedbenzo.nsduh.scores2$weights
# Make survey design object using propensity scores weighted on Z-drug use
sedbenzo.nsduh.design1 <-  svydesign(id= ~PSU, strata = ~Strata, weights=~SBweights1, data = bzra.data,nest=T)
sedbenzo.nsduh.design2 <-  svydesign(id= ~PSU, strata = ~Strata, weights=~SBweights2, data = bzra.data,nest=T)

### Modeling using the weighted data

##Associations of Zdrugs with STBs
# Create models
stbs = c("PYRSuicIdea","PYRSuicPlan","PYRSuicAttempt")
covariates = c("+Age+Race+Sex+Income","+Age+Race+Sex+Income+PYRDepression+PYRIllicit+K6Score+WHODAS")
modelnames = c("Model 1","Model 2")
zdrug.nsduh.design <- list(zdrug.nsduh.design1,zdrug.nsduh.design2)
finalmodels = list()
for(idx in 1:length(stbs)){
  stbsmodels <- list()
  for(jdx in 1:length(covariates)){
    stbsmodels[[jdx]] = svyglm(as.formula(paste(stbs[idx],"~Zdrug",covariates[jdx],sep="")),
                               design = zdrug.nsduh.design[[jdx]],family="quasibinomial")
    print(paste("Completed",stbs[idx],"for",modelnames[jdx]))
  }
  finalmodels[[idx]] <- stbsmodels
}
# Summarize Models
zdrug.model.sum <- tibble()
for(idx in 1:length(stbs)){
  interimtable2 <- tibble()
  for(jdx in 1:length(covariates)){
     interimtable <- tibble("STBs" = stbs[idx],
                            "Model" = modelnames[jdx],
                            "Predictors" = c("Z-Drug use"),
                            "Estimate" = round(exp(summary(finalmodels[[idx]][[jdx]])$coefficients[2,1]),2),
                            "Lower" = unname(round(exp(confint(finalmodels[[idx]][[jdx]],"Zdrug")),2)[,1]),
                            "Upper" = unname(round(exp(confint(finalmodels[[idx]][[jdx]],"Zdrug")),2)[,2]),
                            "95% CI" = c(paste("[",unname(round(exp(confint(finalmodels[[idx]][[jdx]],"Zdrug")),2)[,1]),
                                              ", ",unname(round(exp(confint(finalmodels[[idx]][[jdx]],"Zdrug")),2)[,2]),
                                              "]",sep="")),
                            "p-value" = round(summary(finalmodels[[idx]][[jdx]])$coefficients[2,4],4))
     interimtable2 <- bind_rows(interimtable2, interimtable)
    print(paste("Summarized",stbs[idx]))
  }
  zdrug.model.sum <- bind_rows(zdrug.model.sum,interimtable2)
}

## Associations of SedBenzos with STBs
# Create models
stbs = c("PYRSuicIdea","PYRSuicPlan","PYRSuicAttempt")
covariates = c("+Age+Race+Sex+Income","+Age+Race+Sex+Income+PYRDepression+PYRIllicit+K6Score+WHODAS")
modelnames = c("Model 1","Model 2")
sedbenzo.nsduh.design <- list(sedbenzo.nsduh.design1,sedbenzo.nsduh.design2)
finalmodels = list()
for(idx in 1:length(stbs)){
  stbsmodels <- list()
  for(jdx in 1:length(covariates)){
    stbsmodels[[jdx]] = svyglm(as.formula(paste(stbs[idx],"~SedBenzo",covariates[jdx],sep="")),
                               design = sedbenzo.nsduh.design[[jdx]],family="quasibinomial")
    print(paste("Completed",stbs[idx],"for",modelnames[jdx]))
  }
  finalmodels[[idx]] <- stbsmodels
}
# Summarize Models
sedbenzo.model.sum <- tibble()
for(idx in 1:length(stbs)){
  interimtable2 <- tibble()
  for(jdx in 1:length(covariates)){
    interimtable <- tibble("STBs" = stbs[idx],
                           "Model" = modelnames[jdx],
                           "Predictors" = c("SedBenzo Use"),
                           "Estimate" = round(exp(summary(finalmodels[[idx]][[jdx]])$coefficients[2,1]),2),
                           "Lower" = unname(round(exp(confint(finalmodels[[idx]][[jdx]],"SedBenzo")),2)[,1]),
                           "Upper" = unname(round(exp(confint(finalmodels[[idx]][[jdx]],"SedBenzo")),2)[,2]),
                           "95% CI" = c(paste("[",unname(round(exp(confint(finalmodels[[idx]][[jdx]],"SedBenzo")),2)[,1]),
                                              ", ",unname(round(exp(confint(finalmodels[[idx]][[jdx]],"SedBenzo")),2)[,2]),
                                              "]",sep="")),
                           "p-value" = round(summary(finalmodels[[idx]][[jdx]])$coefficients[2,4],4))
    interimtable2 <- bind_rows(interimtable2, interimtable)
    print(paste("Summarized",stbs[idx]))
  }
  sedbenzo.model.sum <- bind_rows(sedbenzo.model.sum,interimtable2)
}
study1.sum <- bind_rows(zdrug.model.sum,sedbenzo.model.sum)
#write_csv(study1.sum,"c:/users/atala/sync/research/projects/to publish/NSDUH/zdrugs and suicide/table2.csv")

### NHANES COMPARISON ANALYSIS

### Load NHANES Data
nhanes <- read_rds("c:/users/atala/sync/research/projects/to publish/nsduh/zdrugs and suicide/data/nhanes.rds")
nhanes <- filter(nhanes, is.na(SI)==F) %>% filter(is.na(SleepDuration)==F)
### Recode Variables
nhanes$ID <- factor(nhanes$ID)
nhanes$Sex <- factor(nhanes$Sex, levels = c("Male", "Female"))
nhanes$Age <- factor(nhanes$Age, levels = c("18-24","25-34","35-44","45-54","55-64","65-74","75-85"))
nhanes <- mutate(nhanes, "SleepDuration2" = case_when(SleepDuration<7~"Short",SleepDuration>=7&SleepDuration<9~"Recommended",SleepDuration>=9~"Long"))
nhanes$SleepDuration2 <- factor(nhanes$SleepDuration2, levels = c("Recommended", "Short","Long"))
nhanes$Race <- factor(nhanes$Race, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Mexican Hispanic","Other Hispanic","Other Race"))
nhanes <- mutate(nhanes, "Sedatives" = factor(case_when(Zdrug=="No" & Trazodone=="No"~"None",
                                                 Zdrug=="Yes" & Trazodone=="No"~"Zdrug",
                                                 Zdrug=="No" & Trazodone=="Yes"~"Trazodone",
                                                 Zdrug=="Yes" & Trazodone=="Yes"~"Zdrug + Trazodone"),
                                              levels = c("None","Zdrug","Trazodone","Zdrug + Trazodone")))

###Survey Design object
nhanes.design <- svydesign(ids=~PSU, strata = ~Strata, weights=~Weights,data=nhanes,nest=T)

tbl_merge(tab_spanner = c("Overall","Suicidal Ideation"),list(
  tbl_svysummary(nhanes.design,include=c("Age","Sex","Race"),missing="no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n_unweighted} ({p}%)")),
  tbl_svysummary(nhanes.design,by="SI",include=c("SI","Age","Sex","Race"),missing="no",
                 statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n_unweighted} ({p}%)")) %>%
    add_p(test = list(all_continuous() ~ "svy.t.test",all_categorical() ~ "svy.chisq.test"))
))

#Figure
ggarrange(nrow=2,ncol=1,labels="AUTO",
  ggplot(svyby(~SI,~Zdrug,nhanes.design,svymean,na.rm=T,vartype="ci") %>% 
           mutate("Zdrug" = factor(Zdrug, levels = c("No","Yes"),labels=c("No Z-Drug Use","Z-Drug Use"))),
         aes(x=Zdrug,y=SIYes))+
    geom_col(fill="Grey",color="Black")+
    geom_errorbar(aes(ymin=ci_l.SIYes,ymax=ci_u.SIYes),width=.5)+
    scale_y_continuous(labels=scales::percent)+theme_pubr()+xlab("")+
    ylab("% Suicidal Ideation"),
  ggplot(svyby(~SI,~Trazodone,nhanes.design,svymean,na.rm=T,vartype="ci") %>% 
           mutate("Trazodone" = factor(Trazodone, levels = c("No","Yes"),
                                       labels=c("No Trazodone Use","Trazodone Use"))),
         aes(x=Trazodone,y=SIYes))+
    geom_col(fill="Grey",color="Black")+
    geom_errorbar(aes(ymin=ci_l.SIYes,ymax=ci_u.SIYes),width=.5)+
    scale_y_continuous(labels=scales::percent)+theme_pubr()+xlab("")+
    ylab("% Suicidal Ideation")
)

### Duration differences between SI categories
svyby(~ZdrugDays,~SI,nhanes.design,svymean,na.rm=T)
svyby(~TrazodoneDays,~SI,nhanes.design,svymean,na.rm=T)
# Prevalence of SI among categories of sleep aid use
drugs <- c("Zdrug","Trazodone")
sleepaid.nhanes <- tibble()
for(idx in 1:length(drugs)){
  temptable <- svyby(~SI,as.formula(paste("~",drugs[idx],sep="")),nhanes.design,svymean,vartype="ci",na.rm=T) %>% 
    transmute("Substance" = drugs[idx],"Level" = c("No","Yes"), "Prevalence" = round(SIYes*100,2), 
              "95% CI" = paste("[", round(ci_l.SIYes*100,2), ", ",round(ci_u.SIYes*100,2),"]",sep=""))
  sleepaid.nhanes <- bind_rows(sleepaid.nhanes,temptable)
}
#write_csv(sleepaid.nhanes, "c:/users/atala/sync/research/projects/to publish/nsduh/zdrugs and suicide/table3.csv")

### Propensity Score Weighting
##  Zdrugs
set.seed(627)
zdrug.nhanes.scores1 <- weightit(Zdrug~Age+Race+Sex,nhanes,
                                 method="gbm",stabilize=T,s.weights = "Weights",estimand = "ATE",stop.method="es.mean")
zdrug.nhanes.scores2 <- weightit(Zdrug~Age+Race+Sex+SleepDuration2+SSRI+SNRI+AtypicalAP+MoodStabilizer,nhanes,
                                method="gbm",stabilize=T,s.weights = "Weights",estimand = "ATE",stop.method="es.mean")
nhanes$Zweights1 <- zdrug.nhanes.scores1$weights
nhanes$Zweights2 <- zdrug.nhanes.scores2$weights
# Make survey design object using propensity scores weighted on Z-drug use
zdrug.nhanes.design1 <-  svydesign(id= ~PSU, strata = ~Strata, weights=~Zweights1, data = nhanes,nest=T)
zdrug.nhanes.design2 <-  svydesign(id= ~PSU, strata = ~Strata, weights=~Zweights2, data = nhanes,nest=T)

### Trazodone
trazodone.nhanes.scores1 <- weightit(Trazodone~Age+Race+Sex,nhanes,
                                    method="gbm",stabilize=T,s.weights = "Weights",estimand = "ATE",stop.method="es.mean")
trazodone.nhanes.scores2 <- weightit(Trazodone~Age+Race+Sex+SleepDuration2+SSRI+SNRI+AtypicalAP+MoodStabilizer,nhanes,
                            method="gbm",stabilize=T,s.weights = "Weights",estimand = "ATE",stop.method="es.mean")
nhanes$Tweights1 <- trazodone.nhanes.scores1$weights
nhanes$Tweights2 <- trazodone.nhanes.scores2$weights
# Make survey design object using propensity scores weighted on Trazodone use
trazodone.nhanes.design1 <-  svydesign(id= ~PSU, strata = ~Strata, weights=~Tweights1, data = nhanes,nest=T)
trazodone.nhanes.design2 <-  svydesign(id= ~PSU, strata = ~Strata, weights=~Tweights2, data = nhanes,nest=T)
## Modeling Associations between Zdrugs/Trazodone and SI
zdrugs.nhanes.model1 <- svyglm(SI~Zdrug+Age+Race+Sex,zdrug.nhanes.design1,family="quasibinomial")
zdrugs.nhanes.model2 <- svyglm(SI~Zdrug+Age+Race+Sex+SleepDuration2+SSRI+SNRI+AtypicalAP+MoodStabilizer,zdrug.nhanes.design2,family="quasibinomial")
trazodone.nhanes.model1 <- svyglm(SI~Trazodone+Age+Race+Sex,trazodone.nhanes.design1,family="quasibinomial")
trazodone.nhanes.model2 <- svyglm(SI~Trazodone+Age+Race+Sex+SleepDuration2+SSRI+SNRI+AtypicalAP+MoodStabilizer,trazodone.nhanes.design2,family="quasibinomial")

nhanes.sum <- tibble("Outcome" = "Suicidal Ideation",
                     "Model" = c("Model 1","Model 2","Model 1","Model 2"),
                     "Predictors" = c("Zdrugs","Zdrugs","Trazodone","Trazodone"),
                     "Estimate" = c(round(exp(summary(zdrugs.nhanes.model1)$coefficients[2,1]),2),
                                    round(exp(summary(zdrugs.nhanes.model2)$coefficients[2,1]),2),
                                    round(exp(summary(trazodone.nhanes.model1)$coefficients[2,1]),2),
                                    round(exp(summary(trazodone.nhanes.model2)$coefficients[2,1]),2)),
                     "95% CI" = c(paste("[", unname(round(exp(confint(zdrugs.nhanes.model1,"ZdrugYes")[1]),2)),
                                        ", ",unname(round(exp(confint(zdrugs.nhanes.model1,"ZdrugYes")[2]),2)),
                                        "]",sep=""),
                                  paste("[", unname(round(exp(confint(zdrugs.nhanes.model2,"ZdrugYes")[1]),2)),
                                        ", ",unname(round(exp(confint(zdrugs.nhanes.model2,"ZdrugYes")[2]),2)),
                                        "]",sep=""),
                                  paste("[", unname(round(exp(confint(trazodone.nhanes.model1,"TrazodoneYes")[1]),2)),
                                        ", ",unname(round(exp(confint(trazodone.nhanes.model1,"TrazodoneYes")[2]),2)),
                                        "]",sep=""),
                                  paste("[", unname(round(exp(confint(trazodone.nhanes.model2,"TrazodoneYes")[1]),2)),
                                        ", ",unname(round(exp(confint(trazodone.nhanes.model2,"TrazodoneYes")[2]),2)),
                                        "]",sep="")),
                     "p-value" = c(round(summary(zdrugs.nhanes.model1)$coefficients[2,4],4),
                                   round(summary(zdrugs.nhanes.model2)$coefficients[2,4],4),
                                   round(summary(trazodone.nhanes.model1)$coefficients[2,4],4),
                                   round(summary(trazodone.nhanes.model2)$coefficients[2,4],4)))
#write_csv(nhanes.sum, "c:/users/atala/sync/research/projects/to publish/nsduh/zdrugs and suicide/table4.csv")

### Table reporting the results of the propensity weighting
listweights <- list(zdrug.nsduh.scores1,zdrug.nsduh.scores2,sedbenzo.nsduh.scores1,sedbenzo.nsduh.scores2,
                    zdrug.nhanes.scores1,zdrug.nhanes.scores2,trazodone.nhanes.scores1,trazodone.nhanes.scores2)
balance.sum.table <- tibble()
for(idx in 1:length(listweights)){
  temptable <- tibble("Dataset" = as.character(listweights[[idx]]$call$data),
                      "Exposure" = as.character(listweights[[idx]]$call$formula[[2]]),
                      "MeanWeights" = round(mean(listweights[[idx]]$weights),4),
                      "SDWeights" = round(sd(listweights[[idx]]$weights),2),
                      "RangeWeights" = paste("[",round(range(listweights[[idx]]$weights)[1],2)," - ",
                                             round(range(listweights[[idx]]$weights)[2],2),"]",sep=""),
                      "MaxUnadjStdDiff" = round(max(abs(bal.tab(listweights[[idx]],stats="m",un=T)$Balance$Diff.Un[-1])),2),
                      "MaxAdjStdDiff" = round(max(abs(bal.tab(listweights[[idx]],stats="m")$Balance$Diff.Adj[-1])),2),
                      "MaxUnadjKS" = round(max(abs(bal.tab(listweights[[idx]],stats="ks",un=T)$Balance$KS.Un[-1])),2),
                      "MaxAdjKS" = round(max(abs(bal.tab(listweights[[idx]],stats="ks")$Balance$KS.Adj[-1])),2))
  balance.sum.table <- bind_rows(balance.sum.table,temptable)
  print("Completed",idx)
}
#write_csv(balance.sum.table, "c:/users/atala/sync/research/projects/to publish/nsduh/zdrugs and suicide/tablemethods.csv")










