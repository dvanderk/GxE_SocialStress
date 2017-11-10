# FINAL PROJECT - Genes and Physiology
# Dee van der Knaap 

library(broom)
library(party)

#import data
cry <- read.csv("~/Desktop/Organized/Wellesley 2017-2018/Classes - Fall/STAT260/Project/R_and_Data/DNACry.csv")

#summary
summary(cry[cry$Audio=="ic",])
summary(cry[cry$Audio=="mc",])
summary(cry[cry$Audio=="wc",])

#make plots
par(mfrow=c(3,1))
# rs53576 is a silent G to A change in the oxytocin receptor (OXTR) gene. 
# Studies have demonstrated that individuals with the G allele are more empathetic, 
#feel less lonely, employ more sensitive parenting techniques, and have lower rates of autism.
plot(cry$NoseT ~ cry$rs53576) 
plot(cry$HR ~ cry$rs53576)

# Study found homozygous for the G allele (GG genotype) areless sensitive to early adversities 
# in the social environment, such ashaving depressed and anxious mothers, compared with A 
# carriers,and individuals with the G allele in this region (compared with theA allele) show more 
# empathy.
plot(cry$NoseT ~ cry$rs2254298) 
plot(cry$HR ~ cry$rs2254298)
#length(cry$rs2254298[cry$rs2254298 == "AA"]) #checking AA because boxplot looks funny

# 5-HTTLPR (serotonin-transporter-linked polymorphic region) is a degenerate repeat 
# polymorphic region in SLC6A4, the gene that codes for the serotonin transporter. 
# It has been extensively investigated in connection with the behavioral, psychiatric, 
# pharmacogenetic aspects of neuropsychiatric disorders.
plot(cry$NoseT ~ cry$X5HTTLPR) 
plot(cry$HR ~ cry$X5HTTLPR)

#Tree Models - This gives us nothing (one node, no significant differences)
NoseTree <- ctree(NoseT ~ as.factor(rs53576) + as.factor(rs2254298) + as.factor(X5HTTLPR), data = cry)
plot(NoseTree, type = "simple")

#Linear Models
#Without Interactions
plot(cry$HR ~ cry$NoseT, col = c(cry$X5HTTLPR, cry$rs2254298, cry$rs53576))
fitNoseT <- lm(NoseT ~ rs53576 + rs2254298 + X5HTTLPR, data=cry)
summary(fitNoseT)
fitHR <- lm(HR ~ rs53576 + rs2254298 + X5HTTLPR, data=cry)
summary(fitHR)

#With Interactions
#run interactions with variables we think are important in this dataset - oxytocin genes
fitNoseTInteract <- lm(NoseT ~ rs53576 + rs2254298 + rs53576*rs2254298, data=cry)
summary(fitNoseTInteract)
glance(fitNoseTInteract)$p.value
fitHRInteract <- lm(HR ~ rs53576 + rs2254298 + rs53576*rs2254298, data=cry)
summary(fitHRInteract)
glance(fitHRInteract)$p.value

#Now use multiple comparisons on the interaction model
noseT_pvals <- as.vector(summary(fitNoseTInteract)$coefficients[,4]) 
p.adjust(noseT_pvals, method = "bonferroni") < 0.05
HR_pvals <-  as.vector(summary(fitHRInteract)$coefficients[,4]) 
p.adjust(HR_pvals, method = "bonferroni")





