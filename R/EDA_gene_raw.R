# This is a function to conduct the exploratory data analysis for gene dataset
# Input:
#   The path of the raw dataset
# Outputs:
#   A list:
#     summary: The summary of the raw data
#     missing_plot: A plot illustrates the missing values in dataset
#     cell_line_count: A table of the number of observations in different cell lines
#     treatment_count: A table of the number of observations in different treatments
#     name_count: A table of the number of observations in different namess
#     conc_histgram: A histogram of concentration of growth factors
#     expression_histgram: A histogram of gene expressions

EDA_gene_raw <-function(gene_file){
  # read the raw data
  gene_df <-read_xlsx(gene_file)
  # get the summary of the raw data
  gene_summary <- skimr::skim(gene_df)
  # check the missing values in the raw data
  check_missing <- naniar::vis_miss(gene_df)
  # count the number of observations in different cell lines
  cell_line_unique <- gene_df|>group_by(cell_line)|>count()
  # count the number of observations in different treatments
  treatment_unique <- gene_df|>group_by(treatment)|>count()
  # count the number of observations in different names
  name_unique  <- gene_df|>group_by(name)|>count()
  # draw a histogram of concentration of growth factors
  conc_histgram <-
    ggplot(gene_df, aes(conc))+
    geom_histogram(col="black",fill="orange")+
    xlab("growth factor")+
    ggtitle("Histogram of the growth factor ")+
    theme_bw()
  # draw a histogram of gene expressions
  expression_histgram <-
    ggplot(gene_df, aes(gene_expression))+
    geom_histogram(col="black",fill="orange")+
    ggtitle("Histgram of gene_expression")+
    theme_bw()
  # store the results in a list
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
