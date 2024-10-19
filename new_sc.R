library(sjPlot)
library(lme4)
library(dplyr)
library(nlme)
# library(ggplot)
library(ggplot2)
library(lmerTest)
# library(EMAtools)
# install.packages(“EMAtools”)

lme.dscore<-function(mod,data,type){
  if (type=="lme4") {
    mod1<-lmerTest::lmer(mod,data=data)
    eff<-cbind(summary(mod1)$coefficients[,4],summary(mod1)$coefficients[,3])
  }

  if (type=="nlme") {
    eff=cbind(summary(mod)$tTable[,4],summary(mod)$fixDF$terms)
  }

  colnames(eff)<-c("t","df")
  eff<-as.data.frame(eff)
  eff$d<-(2*eff$t)/sqrt(eff$df)
  eff<-eff[-1,]
  return(eff)
}


# df_i <- read.csv("D:/icmi_ff-main/icmi_ff-main/individual_group_sclag.csv")
df_i <- read.csv("D:/icmi_ff-main/icmi_ff-main/PO_individual_group_sclag_3.csv")
pa <- df_i %>% mutate(
  diagnosisPerson = factor(diagnosisPerson, levels = c("TH", "HL")),
)
head(pa)
model <- lmer(Social_contact_ratio ~ diagnosisPerson   * Group_Size+ (1 | person) + (1|group), data = pa)
# model <- lmer(Social_contact_ratio ~ diagnosisPerson * HL_ratio  + (1 | person) + (1|group), data = pa)
# model <- lmer(Social_contact_ratio ~ diagnosisPerson * Homophily_degree  + (1 | person) + (1|group), data = pa)
# model <- lmer(Social_contact_ratio ~ diagnosisPerson * HL_ratio * Group_Size + (1 | person) + (1|group), data = pa)
# model <- lmer(Social_contact_ratio ~ diagnosisPerson * Homophily_degree * Group_Size + (1 | person) + (1|group), data = pa)
lme.dscore(model,data = pa,type='lme4')
tab_model(model,show.stat = T,show.se=T,show.r2=F,dv.labels ="Time in Social Contact")


######################################################################################################
#social contact

#read in dataframe
# df<-read.csv("D:/icmi_ff-main/icmi_ff-main/PO_individual_group_sclag_3.csv")
df<-read.csv("D:/icmi_ff-main/icmi_ff-main/final_social_contact_ratios_with_diagnosis.csv")
# df_ds_p<-read.csv("D:/icmi_ff-main/icmi_ff-main/grouped_final_social_contact_ratios_with_diagnosis_copy.csv")
# df_ds_po<-read.csv("D:/icmi_ff-main/icmi_ff-main/PO_grouped_final_social_contact_ratios_with_diagnosis_copy.csv")
# df_k<-read.csv("D:/icmi_ff-main/icmi_ff-main/kmeans_final_social_contact_ratios_with_diagnosis_copy.csv")


head(df)

pa <- df %>% mutate(
  diagnosisPerson1 = factor(diagnosisPerson1, levels = c("HL", "TH")),
  diagnosisPerson2 = factor(diagnosisPerson2, levels = c("HL", "TH")),
  # Date = as.Date(Date, "%y/%m/%d"),  # Adjust format string as needed
  dyad = paste(Subject, Partner),
)
head(pa)

# Define the function
create_pa <- function(df) {
  pa <- df %>% 
    mutate(
      diagnosisPerson1 = factor(diagnosisPerson1, levels = c("TH", "HL")),
      diagnosisPerson2 = factor(diagnosisPerson2, levels = c("TH", "HL")),
      # Uncomment and adjust the following line if Date conversion is needed
      # Date = as.Date(Date, "%y/%m/%d"),  # Adjust format string as needed
      dyad = paste(Subject, Partner),
      Homophily = recode(Pair,
        "concordant" = "same",
        "diff" = "Discordant"
      )
    ) %>% 
    select(-Pair)  # Remove the original Pair column
  return(pa)
}

pa <- create_pa(df)
# pa_ds_p <- create_pa(df_ds_p)
# pa_ds_po <- create_pa(df_ds_po)
# pa_k <- create_pa(df_k)
# pa_i <- create_pa(df_i)





#aggregated dx*pair social contact analyses

# No grouping

sc<-lmer(scLag~diagnosisPerson1*Homophily+(1|Subject),pa)
# + date as a random factor
# sc <- lmer(scLag ~ diagnosisPerson1 * Homophily + (1 | Date) + (1 | Subject), pa)

# Add Group
# as fixed factor
# sc <- lmer(scLag ~ diagnosisPerson1 * Homophily * Group + (1 | Subject), pa)

# sc <- lmer(scLag ~ diagnosisPerson1 * Homophily * Group + (1 | Subject) + (1 | Date), pa)

# as random factor
# sc <- lmer(scLag ~ diagnosisPerson1 * Homophily + (1 | Subject) + (1 | Group), data = pa)

# how Homophily and group interact
# sc <- lmer(scLag ~ Homophily * Group + (1 | Subject), data = pa)


# sc <- lmer(scLag ~ Homophily * Group + (1 | Subject)  + (1 | Date) , data = pa)
# sc <- lmer(scLag ~ diagnosisPerson1 * Homophily + (Homophily | Group)  + (1 | Date) , data = pa)



# # with diagnosisPerson1
# # baseline
# model <- lmer(scLag ~ diagnosisPerson1 * Homophily + (1 | Subject), data = pa)

# # ds with p
# model1_ds_p <- lmer(scLag ~ diagnosisPerson1 * Homophily * Group + (1 | Subject), data = pa_ds_p)
# model2_ds_p <- lmer(scLag ~ diagnosisPerson1 * Homophily + (1 | Subject) + (1 | Group), data = pa_ds_p)

# # ds with po
# model1_ds_po <- lmer(scLag ~ diagnosisPerson1 * Homophily * Group + (1 | Subject), data = pa_ds_po)
# model2_ds_po <- lmer(scLag ~ diagnosisPerson1 * Homophily + (1 | Subject) + (1 | Group), data = pa_ds_po)

# # K means
# model1_k<- lmer(scLag ~ diagnosisPerson1 * Homophily * Group + (1 | Subject), data = pa_k)
# model2_k <- lmer(scLag ~ diagnosisPerson1 * Homophily + (1 | Subject) + (1 | Group), data = pa_k)



# # Baseline Model Without diagnosisPerson1
# model_no_diag <- lmer(scLag ~ Homophily + (1 | Subject), data = pa_ds_p)

# # ds with p Models Without diagnosisPerson1
# model1_ds_p_no_diag <- lmer(scLag ~ Homophily * Group + (1 | Subject), data = pa_ds_p)
# model2_ds_p_no_diag <- lmer(scLag ~ Homophily + (1 | Subject) + (1 | Group), data = pa_ds_p)

# # ds with po Models Without diagnosisPerson1
# model1_ds_po_no_diag <- lmer(scLag ~ Homophily * Group + (1 | Subject), data = pa_ds_po)
# model2_ds_po_no_diag <- lmer(scLag ~ Homophily + (1 | Subject) + (1 | Group), data = pa_ds_po)

# # K-means Models Without diagnosisPerson1
# model1_k_no_diag <- lmer(scLag ~ Homophily * Group + (1 | Subject), data = pa_k)
# model2_k_no_diag <- lmer(scLag ~ Homophily + (1 | Subject) + (1 | Group), data = pa_k)


# sc <- model
lme.dscore(sc,data = pa,type='lme4')
tab_model(sc,show.stat = T,show.se=T,show.r2=F,dv.labels ="Time in Social Contact")

# summary(model2_ds_p_no_diag)
# summary(model2_ds_po_no_diag)
# summary(model2_k_no_diag)

# AIC(model, model1_ds_p, model2_ds_p, model1_ds_po, model2_ds_po, model1_k, model2_k)
# BIC(model, model1_ds_p, model2_ds_p, model1_ds_po, model2_ds_po, model1_k, model2_k)

# anova(model1, model2)
# anova(model1, model3)

# summary(model)$varcor
# summary(model1_ds_p)$varcor
# summary(model2_ds_p)$varcor
# summary(model1_ds_po)$varcor
# summary(model2_ds_po)$varcor
# summary(model1_k)$varcor
# summary(model2_k)$varcor


# aic_values <- c(
#   AIC(model_no_diag),
#   AIC(model1_ds_p_no_diag),
#   AIC(model2_ds_p_no_diag),
#   AIC(model1_ds_po_no_diag),
#   AIC(model2_ds_po_no_diag),
#   AIC(model1_k_no_diag),
#   AIC(model2_k_no_diag)
# )

# bic_values <- c(
#   BIC(model_no_diag),
#   BIC(model1_ds_p_no_diag),
#   BIC(model2_ds_p_no_diag),
#   BIC(model1_ds_po_no_diag),
#   BIC(model2_ds_po_no_diag),
#   BIC(model1_k_no_diag),
#   BIC(model2_k_no_diag)
# )

# # Create a data frame to summarize the results
# comparison_df <- data.frame(
#   Model = c("Baseline", "DS with p - Model 1", "DS with p - Model 2", 
#             "DS with po - Model 1", "DS with po - Model 2", 
#             "K Means - Model 1", "K Means - Model 2"),
#   AIC = aic_values,
#   BIC = bic_values
# )

# print(comparison_df)


# # Read the CSV file into a dataframe
# df_3 <-read.csv("D:/icmi_ff-main/icmi_ff-main/3_group.csv")

# # Rename the columns
# colnames(df_3)[colnames(df_3) == "homophily_degree"] <- "Homophily"
# colnames(df_3)[colnames(df_3) == "sclag"] <- "scLag"
# colnames(df_3)[colnames(df_3) == "group"] <- "Group"

# # head(df_3)
# sc <- lmer(scLag ~ Homophily + (1 | day) + (1 | Group), data = df_3)
# lme.dscore(sc,data = df_3,type='lme4')
# tab_model(sc,show.stat = T,show.se=T,show.r2=F,dv.labels ="Time in Social Contact")

# # Extract residuals
# residuals_model <- resid(model)

# # Q-Q plot
# ggplot(data = data.frame(residuals_model), aes(sample = residuals_model)) + 
#   stat_qq() + 
#   stat_qq_line() +
#   ggtitle("Q-Q Plot of Residuals for Baseline Model")

# summary(model2_ds_p_no_diag)
# summary(model2_ds_po_no_diag)
