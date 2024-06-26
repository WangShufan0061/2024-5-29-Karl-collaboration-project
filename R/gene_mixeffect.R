# This is a function to build, select and illustrate the mixed-effect models for cleaned gene data
# Input:
#   The cleaned gene dataset
# Outputs:
#   A list of tables and figures:
#     me1: The summary of model me1
#     me2: The summary of model me2
#     me3: The summary of model me3
#     me4: The summary of model me4
#     me5: The summary of model me5
#     anova: The AIC and Likelihood ratio test results of 5 models, which is conducted with anova() function in R
#     coe_final: The coefficients and intercepts of our final model for different names
#     final model: A plot illustrate the final models

gene_mixeffect <- function(gene_cleaned){

# normalize the variables conc and gene_expression
gene_cleaned$conc <- scale(gene_cleaned$conc)[,1]
gene_cleaned$gene_expression <- scale(gene_cleaned$gene_expression)[,1]

# different mix effect model
me1<-lmer(gene_expression~conc+cell_line+(1|name),data = gene_cleaned)
me2<-lmer(gene_expression~treatment+conc+cell_line+(1|name),data = gene_cleaned)
me3<-lmer(gene_expression~treatment*conc+cell_line+(1|name),data = gene_cleaned)
me4<-lmer(gene_expression~treatment*conc+(1+conc|name),data = gene_cleaned)
me5<-lmer(gene_expression~treatment*conc+cell_line+(1+conc|name),data = gene_cleaned)

# compare model using anova. According to the AIC and likelihood ratio test results, we choose me4 as our final model
model_anova<-tibble(
  model = c("me1","me2","me3","me4","me5"),
  AIC = anova(me1,me2,me3,me4,me5)$AIC,
  pvalue = anova(me1,me2,me3,me4,me5)$`Pr(>Chisq)`
)|>
  gt()|>
  fmt_number(
    columns = c(AIC),
    decimals = 3,
    use_seps = FALSE
  )

# The prediction of our final model
me4_pre = predict(me4)

# extract the intercepts and coefficients of our final model
model_coef<- tibble(
  name = c("GL-cDZ","GL-cwN","GL-kYH","GL-MFA","GL-rjS","GL-XIb","GL-Xik","GL-ZHw"),
  Intercept = coef(me4)$name[,1],
  Placebo = coef(me4)$name[,2],
  conc = coef(me4)$name[,3],
  `Placebo:conc` =  coef(me4)$name[,4]
)|>
  gt()|>
  fmt_number(
    columns = c(Intercept,Placebo,conc,`Placebo:conc`),
    decimals = 3,
    use_seps = FALSE
  )|>
  tab_spanner(label = "Coefficients",
              columns = c(Placebo,conc,`Placebo:conc`))


# illustrate the final model
p3<-ggplot(gene_cleaned, aes(conc, gene_expression,col=name))+
  facet_grid(~cell_line)+
  geom_line(aes(y=me4_pre), linewidth=0.8)+
  geom_point(alpha = 0.5)+
  ylab("Gene Expression (normalized)")+
  xlab("Growth Factor (normalized)")+
  ggtitle("Illustration of the final model")+
  theme_bw()

#save the plots
ggsave(here::here("figs/final models.png"), p3,width = 9,height = 6,units = "in")

#save the tables
model_anova|>gtsave("tabs/model_AIC_pvalue.png")
model_coef|>gtsave("tabs/final_coef.png")

result_list <- list('me1'=summary(me1),
                    'me2'=summary(me2),
                    'me3'=summary(me3),
                    'me4'=summary(me4),
                    'me5'=summary(me5),
                    'anova'=model_anova,
                    'coe_final' = model_coef,
                    'final_model'=p3)
return(result_list)
}
# pacman::p_load(lme4,merTools,caret,tidyverse,gt)
# temp<-gene_mixeffect(gene_cleaned)
# temp$me1
# temp$rmse_rsquare
# temp$anova
# temp$residual_vs_fitted
# temp$QQplot
