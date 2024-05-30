# This is a function to conduct the exploratory data analysis for gene dataset
# Input:
#   The cleaned gene dataset
# Outputs:
#   A list of tables and figures:
#     mean_GeneExp: A mean table of gene expressions with different concentrations of growth factors
#     sd_GeneExp: A standard deviation of gene expressions with different concentrations of growth factors
#     boxplot_Gene_vs_Conc: A box plot of gene expression vs conc with different treatments
#     scatter_Gene_vs_Conc_wild: A scatter plot of gene expression vs conc with different treatments, for cells with "Wild-type".
#     scatter_Gene_vs_Conc_cell: A scatter plot of gene expression vs conc with different treatments, for cells with "Cell-type 101".

EDA_gene_cleaned <- function(gene_clean){
  # set the values in the tables with 3 decimals
  options(digits = 3)

  # Get mean expression table
  mean_tab <-gene_clean|>
    group_by(as.factor(conc),treatment)|>
    summarize(formatC(mean(gene_expression), digits = 3, format = "f"))
  mean_tab<-
    mean_tab |>
    group_by(`as.factor(conc)`) |>
    spread(`as.factor(conc)`, `formatC(mean(gene_expression), digits = 3, format = "f")`)|>
    gt()|>
    cols_label(treatment = "Treatments")|>
    tab_spanner(
      label = "Growth factor",
      columns = c(`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`))|>
    tab_header(
      title = "Mean values of gene expression",
      subtitle = "under different growth factors and treatments"
    )

  # Get standard derivation gene expression table
  sd_tab <- gene_clean|>
    group_by(as.factor(conc),treatment)|>
    summarize(formatC(sd(gene_expression), digits = 3, format = "f"))
  sd_tab<-
    sd_tab  %>%
    group_by(`as.factor(conc)`) %>%
    spread(`as.factor(conc)`, `formatC(sd(gene_expression), digits = 3, format = "f")`)|>
    gt()|>
    cols_label(treatment = "Treatments")|>
    tab_spanner(
      label = "Growth factor",
      columns = c(`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`))|>
    tab_header(
      title = "Standard deviations of gene expression",
      subtitle = "under different growth factors and treatments"
    )

  # box plot of gene expression vs conc with different treatment
  box1<-gene_clean|>
    ggplot(aes(as.factor(conc),gene_expression,fill=treatment))+
    geom_boxplot()+
    xlab("Growth Factor")+
    ylab("Gene Expression")+
    theme(legend.position = "bottom")+
    ggtitle("A box plot of gene expression vs growth factor with different treatments")+
    theme_bw()

  # A scatter plot of gene expression vs conc with different treatments, for cells with "Wild-type".
  scatter1<-gene_clean|>
    filter(cell_line=="Wild-type")|>
    ggplot(aes(conc, gene_expression, col=treatment,shape=name))+
    geom_point()+
    xlab("Growth factor")+
    ylab("Gene expression")+
    ggtitle("Wild-type")+
    theme_bw()

  #A scatter plot of gene expression vs conc with different treatments, for cells with "Cell-type 101".
  scatter2<-gene_clean|>
    filter(cell_line!="Wild-type")|>
    ggplot(aes(conc, gene_expression,shape=name,col=treatment))+
    geom_point()+
    xlab("Growth factor")+
    ylab("Gene expression")+
    ggtitle("Cell-type 101")+
    theme_bw()

  # store the results in a list
  results<-list(
    "mean_GeneExp"=mean_tab,
    "sd_GeneExp"=sd_tab,
    "boxplot_Gene_vs_Conc"=box1,
    "scatter_Gene_vs_Conc_wild"=scatter1,
    "scatter_Gene_vs_Conc_cell"=scatter2
  )
}

# pacman::p_load(tidyverse,targets, gt)
# tar_load(gene_clean)
# temp<-EDA_gene_cleaned(gene_clean)
# temp$mean_GeneExp
# temp$sd_GeneExp
# temp$boxplot_Gene_vs_Conc
# temp$scatter_Gene_vs_Conc_wild
# temp$scatter_Gene_vs_Conc_cell
