
gene_mixeffect <- function(gene_clean){
gene_clean$conc <- scale(gene_clean$conc)[,1]
gene_clean$gene_expression <- scale(gene_clean$gene_expression)[,1]
# different mix effect model
me1<-lmer(gene_expression~conc+cell_line+(1|name),data = gene_clean)
me2<-lmer(gene_expression~treatment+conc+cell_line+(1|name),data = gene_clean)
me3<-lmer(gene_expression~treatment*conc+cell_line+(1|name),data = gene_clean)
me4<-lmer(gene_expression~treatment*conc+(1+conc+treatment|name),data = gene_clean)
me5<-lmer(gene_expression~treatment*conc+cell_line+(1+conc+treatment|name),data = gene_clean)
# model prediction
me1_pre = predict(me1)
me2_pre = predict(me2)
me3_pre = predict(me3)
me4_pre = predict(me4)
me5_pre = predict(me5)

pre_df <-tibble(
  truth = gene_clean$gene_expression,
  me1_pre = me1_pre,
  me2_pre = me2_pre,
  me3_pre = me3_pre,
  me4_pre = me4_pre,
  me5_pre = me5_pre
  #me4_res = residuals(me4)
)
# # calculate rmse and r square
# rmse1 =  sqrt(mean((pre_df$truth - pre_df$me1_pre)^2))
# R_square1 = cor(pre_df$truth , pre_df$me1_pre)^2
#
# rmse2 =  sqrt(mean((pre_df$truth - pre_df$me2_pre)^2))
# R_square2 = cor(pre_df$truth , pre_df$me2_pre)^2
#
# rmse3 =  sqrt(mean((pre_df$truth - pre_df$me3_pre)^2))
# R_square3 = cor(pre_df$truth , pre_df$me3_pre)^2
#
# rmse4 =  sqrt(mean((pre_df$truth - pre_df$me4_pre)^2))
# R_square4 = cor(pre_df$truth , pre_df$me4_pre)^2
#
#
# model_performance <- tibble(
#   model = c("me1","me2","me3","me4"),
#   RMSE = c(rmse1,rmse2,rmse3,rmse4),
#   R_square = c(R_square1,R_square2,R_square3,R_square4)
# )|>gt()
# compare model using anova

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

# We choose me4 as our final model

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
# residual vs predicted plot
# p1<-pre_df|>
#   ggplot()+
#     geom_point(aes(x=me4_pre, y= me4_res))+
#   geom_hline(yintercept = 0, colour = "red", linetype = 2)+
#   ggtitle("Residual vs Fitted plot")+
#   theme_bw()

#QQ plot
# p2<-ggplot(pre_df,aes(sample=me4_res)) + stat_qq() + stat_qq_line()


p3<-ggplot(gene_clean, aes(conc, gene_expression,col=name))+
  facet_grid(~cell_line)+
  geom_line(aes(y=me4_pre), size=0.8)+
  geom_point(alpha = 0.5)+
  ylab("Gene Expression (normalized)")+
  xlab("Growth Factor (normalized)")+
  ggtitle("Illustration of the final model")+
  theme_bw()


result_list <- list('me1'=summary(me1),
                    'me2'=summary(me2),
                    'me3'=summary(me3),
                    'me4'=summary(me4),
                    'me5'=summary(me5),
                   # 'rmse_rsquare'=model_performance,
                    'anova'=model_anova,
                    #'anova_24'=anova(me2,me4),
                    #'residual_vs_fitted'=p1,
                    #'QQplot'=p2,
                    'coe_final' = model_coef,
                    'final_model'=p3)
return(result_list)
}
# #pacman::p_load(lme4,merTools,caret,tidyverse,gt)
# temp<-gene_mixeffect(gene_clean)
# temp$me1
# temp$rmse_rsquare
# temp$anova
# temp$residual_vs_fitted
# temp$QQplot
