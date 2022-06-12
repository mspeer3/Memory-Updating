

##Load packages
library(rstatix)

##Import data
me1_data <- read.csv(file = "Mem_emotion_exp1.csv", header = TRUE, stringsAsFactors = FALSE)


#Feeling change
me1_anova_fc <- lm(FeelChange~Group,data=me1_data)
me1_anova_fc_res <- Anova(me1_anova_fc,type=2)
Anova(me1_anova_fc,type=2)
eta_squared(me1_anova_fc_res)

me1_fcg_wide <- spread(me1_data, Group, FeelChange)
t.test(me1_fcg_wide$Positive, me1_fcg_wide$Negative, paired=FALSE, var.equal=TRUE)
t.test(me1_fcg_wide$Positive, me1_fcg_wide$Neutral, paired=FALSE, var.equal=TRUE)
t.test(me1_fcg_wide$Positive, me1_fcg_wide$Distraction, paired=FALSE, var.equal=TRUE)
t.test(me1_fcg_wide$Negative, me1_fcg_wide$Neutral, paired=FALSE, var.equal=TRUE)
t.test(me1_fcg_wide$Negative, me1_fcg_wide$Distraction, paired=FALSE, var.equal=TRUE)
t.test(me1_fcg_wide$Neutral, me1_fcg_wide$Distraction, paired=FALSE, var.equal=TRUE)
cohens_d(me1_data, FeelChange~Group, paired=FALSE)


##Positive content change
me1_anova_pcc <- lm(PosConChange~Group,data=me1_data)
me1_anova_pcc_res <- Anova(me1_anova_pcc,type=2)
Anova(me1_anova_pcc,type=2)
eta_squared(me1_anova_pcc_res)


##Dissimilarity
me1_anova_diss <- lm(Dissimilarity~Group,data=me1_data)
me1_anova_diss_res <- Anova(me1_anova_diss,type=2)
Anova(me1_anova_diss,type=2)
eta_squared(me1_anova_diss_res)


##Positive group: Correlations between changes in feeling and memory content
me1_data_posgrp <- subset(me1_data, Group=="Positive")
cor.test(me1_data_posgrp$FeelChange, me1_data_posgrp$PosConChange, method="pearson")
cor.test(me1_data_posgrp$FeelChange, me1_data_posgrp$Dissimilarity, method="pearson")


##Elaboration - positivity
me1_anova_elab_pos <- lm(Elab_pos~Group,data=me1_data)
me1_anova_elab_pos_res <- Anova(me1_anova_elab_pos,type=2)
Anova(me1_anova_elab_pos,type=2)
eta_squared(me1_anova_elab_pos_res)

me1_elab_pos_wide <- spread(me1_data, Group, Elab_pos)
t.test(me1_elab_pos_wide$Positive, me1_elab_pos_wide$Negative, paired=FALSE, var.equal=TRUE)
t.test(me1_elab_pos_wide$Positive, me1_elab_pos_wide$Neutral, paired=FALSE, var.equal=TRUE)
t.test(me1_elab_pos_wide$Negative, me1_elab_pos_wide$Neutral, paired=FALSE, var.equal=TRUE)


##Elaboration - meaning
me1_anova_elab_meaning <- lm(Elab_meaning~Group,data=me1_data)
me1_anova_elab_meaning_res <- Anova(me1_anova_elab_meaning,type=2)
Anova(me1_anova_elab_meaning,type=2)
eta_squared(me1_anova_elab_meaning_res)

me1_elab_meaning_wide <- spread(me1_data, Group, Elab_meaning)
t.test(me1_elab_meaning_wide$Positive, me1_elab_meaning_wide$Negative, paired=FALSE, var.equal=TRUE)
t.test(me1_elab_meaning_wide$Positive, me1_elab_meaning_wide$Neutral, paired=FALSE, var.equal=TRUE)
t.test(me1_elab_meaning_wide$Negative, me1_elab_meaning_wide$Neutral, paired=FALSE, var.equal=TRUE)


##Elaboration - future-oriented
me1_anova_elab_future <- lm(Elab_future~Group,data=me1_data)
me1_anova_elab_future_res <- Anova(me1_anova_elab_future,type=2)
Anova(me1_anova_elab_future,type=2)
eta_squared(me1_anova_elab_future_res)

me1_elab_future_wide <- spread(me1_data, Group, Elab_future)
t.test(me1_elab_future_wide$Positive, me1_elab_future_wide$Negative, paired=FALSE, var.equal=TRUE)
t.test(me1_elab_future_wide$Positive, me1_elab_future_wide$Neutral, paired=FALSE, var.equal=TRUE)
t.test(me1_elab_future_wide$Negative, me1_elab_future_wide$Neutral, paired=FALSE, var.equal=TRUE)
cohens_d(me1_data, Elab_future~Group, paired=FALSE)


##Positive group: Correlations between future-oriented elaborations and memory change
me1_data_posgrp <- subset(me1_data, Group=="Positive")
cor.test(me1_data_posgrp$Elab_future, me1_data_posgrp$FeelChange, method="pearson")
cor.test(me1_data_posgrp$Elab_future, me1_data_posgrp$PosConChange, method="pearson")


