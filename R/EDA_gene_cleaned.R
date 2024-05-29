EDA_gene_cleaned <- function(gene_clean){
  options(digits = 3)
  ## Get mean expression table
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
  ## Get standard derivation gene expression table
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
  # scatter plot of gene expression vs conc with different treatment, name and cell line
  scatter1<-gene_clean|>
    filter(cell_line=="Wild-type")|>
    ggplot(aes(conc, gene_expression, col=treatment,shape=name))+
    geom_point()+
    xlab("Growth factor")+
    ylab("Gene expression")+
    ggtitle("Wild-type")+
    theme_bw()
  scatter2<-gene_clean|>
    filter(cell_line!="Wild-type")|>
    ggplot(aes(conc, gene_expression,shape=name,col=treatment))+
    geom_point()+
    xlab("Growth factor")+
    ylab("Gene expression")+
    ggtitle("Cell-type 101")+
    theme_bw()

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
