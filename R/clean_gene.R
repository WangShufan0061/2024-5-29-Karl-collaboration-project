# This is a function to clean the raw gene data
# Inputs:
#   The path of the raw data file
# Outputs:
#   A tibble of cleaned gene dataset

clean_gene <- function(gene_file){
  # read the raw data
  gene_df <-read_xlsx(gene_file)
  # clean the cell line
  gene_df$cell_line[gene_df$cell_line=="WILD-TYPE"] <-"Wild-type"
  gene_df$cell_line[gene_df$cell_line=="CELL-TYPE 101"] <-"Cell-type 101"
  # clean the treatment
  gene_df$treatment[gene_df$treatment=="activating factor 42"] <- "Activating factor 42"
  gene_df$treatment[gene_df$treatment=="placebo"] <- "Placebo"
  # clean the name
  gene_df$name[gene_df$name=="Gl-Cwn"] <- "GL-cwN"
  gene_df$name[gene_df$name=="Gl-Rjs"] <- "GL-rjS"
  gene_df$name[gene_df$name=="Gl-Xib"] <- "GL-XIb"
  gene_df$name[gene_df$name=="Gl-Zhw"] <- "GL-ZHw"
  # return the cleaned data
  return(gene_df)
}
 # pacman::p_load(tidyverse,targets,readxl)
 # tar_load(gene_file)
# temp<-clean_gene(gene_file)
#
# temp |> group_by(cell_line)|>count()
# temp |> group_by(treatment)|>count()
# temp |> group_by(name)|>count()
