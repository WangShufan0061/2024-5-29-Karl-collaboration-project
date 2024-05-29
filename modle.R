pacman::p_load(lme4,merTools,caret)
model <-lmer(gene_expression~treatment+conc+cell_line+(1|name),data = gene_clean)
model
lm <-lm(gene_expression~treatment+conc+cell_line,data = gene_clean)
rf<- randomForest(gene_expression~treatment+conc+cell_line+name, data=gene_clean)



plotREsim(REsim(model))

predict_no_re = predict((model), re.form=NA)

predict_with_re = predict(model)
predict_rf = predict(rf)


gene_ex<-tibble(
  truth = gene_clean$gene_expression,
  lm = predict_no_re,
  lm_re = predict_with_re,
  rf = predict_rf
)
mean((gene_ex$truth - gene_ex$lm)^2)# lm residuals
mean((gene_ex$truth - gene_ex$lm_re)^2)# mix-effect residuals 目前最好！
mean((gene_ex$truth - gene_ex$rf)^2)# rf residuals





