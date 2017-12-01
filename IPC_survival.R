setwd("~/Documents/Burdenko")

library(MASS)
library(survival)
library(survminer)
library(extrafont)

#font_import()
#loadfonts(device="mac")       #Register fonts for Windows bitmap output
#fonts() 

cns<-read.csv("info.csv", header = TRUE, sep = ",", dec=",")
cns$days_in_hospital<-as.numeric(cns$days_in_hospital)
cns$age<-as.numeric(cns$age)
cns$CHARLSON_FIRST<-as.integer(cns$CHARLSON_FIRST)

#__________________Survival curves__________

fit<-survfit(Surv(cns$days_in_hospital, cns$outcome_death) ~ cns$intestinal_dysfunction, data=cns)

ggsurvplot(fit, 
           break.time.by = 15,
           conf.int = TRUE,
           ggtheme = theme_bw(base_family = "Times New Roman"),
           font.x =  12,
           font.y = 12,
           font.tickslab = 10,
           pval = TRUE,
           legend = "bottom", 
           legend.title = "Intestinal dysfunction",
           legend.labs = c("Negative", "Positive"),
           xlab = "Days in hospital", ylab = "Proportion alive",
           risk.table=TRUE, risk.table.title="",risk.table.col = "intestinal_dysfunction", 
           risk.table.fontsize = 3.5,
           surv.median.line = "hv")


#_________________Cox regression__________

fit = coxph(Surv(cns$days_in_hospital, cns$outcome_death) ~ + cns$infection_CNS +
              cns$infection_bloodstream +
              cns$infection_respiratory +
              cns$infection_urinary +
              cns$infection_wound +
              cns$infection_other +
              cns$intestinal_dysfunction +
              cns$disease_type_congenital_disorder +
              cns$disease_type_other +
              cns$disease_type_trauma +
              cns$disease_type_tumor +
              cns$CHARLSON_FIRST +
              cns$age +
              cns$gender_M +
              cns$ot_craniotomy_count_binary +
              cns$ot_endonasal_count_binary +
              cns$ot_endovascular_count_bi_binary +
              cns$ot_device_count_binary +
              cns$ot_other_count_binary, data=cns)

summary(fit)

pvals = coef(summary(fit))[,5]
pvals = as.data.frame(pvals)

hr <- round(coef(summary(fit))[,2],6)
hr = as.data.frame(hr)
CI <- round(exp(confint(fit)), 2)
colnames(CI) <- c("Lower", "Higher")

cox = cbind(hr, CI, pvals)
write.table(cox, 'cox.csv', sep = ',')


