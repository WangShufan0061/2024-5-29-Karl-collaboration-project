EDA_gene_raw <-function(gene_file){
  gene_df <-read_xlsx(gene_file)
  gene_summary <- skimr::skim(gene_df)
  check_missing <- naniar::vis_miss(gene_df)
  cell_line_unique <- gene_df|>group_by(cell_line)|>count()
  treatment_unique <- gene_df|>group_by(treatment)|>count()
  name_unique  <- gene_df|>group_by(name)|>count()
  conc_histgram <-
    ggplot(gene_df, aes(conc))+
    geom_histogram(col="black",fill="orange")+
    xlab("growth factor")+
    ggtitle("Histogram of the growth factor ")+
    theme_bw()
  expression_histgram <-
    ggplot(gene_df, aes(gene_expression))+
    geom_histogram(col="black",fill="orange")+
    ggtitle("Histgram of gene_expression")+
    theme_bw()
  result_list <- list('summary'=gene_summary,
                      'missing_plot'=check_missing,
                      'cell_line_count'=cell_line_unique,
                      'treatment_count'=treatment_unique,
                      'name_count'=name_unique,
                      'conc_histgram'=conc_histgram,
                      'expression_histgram'=expression_histgram)
  return(result_list)
}
# pacman::p_load(tidyverse,targets,readxl)
# tar_load(gene_file)
# temp<-EDA_gene_raw(gene_file)
# temp$summary
# temp$cell_line_count
# temp$treatment_count
# temp$name_count
# temp$missing_plot
# temp$conc_histgram
# temp$expression_histgram
