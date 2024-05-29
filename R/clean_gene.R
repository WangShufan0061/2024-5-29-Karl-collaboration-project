clean_gene <- function(gene_file){
  gene_df <-read_xlsx(gene_file)

  gene_df$cell_line[gene_df$cell_line=="WILD-TYPE"] <-"Wild-type"
  gene_df$cell_line[gene_df$cell_line=="CELL-TYPE 101"] <-"Cell-type 101"

  gene_df$treatment[gene_df$treatment=="activating factor 42"] <- "Activating factor 42"
  gene_df$treatment[gene_df$treatment=="placebo"] <- "Placebo"

  gene_df$name[gene_df$name=="Gl-Cwn"] <- "GL-cwN"
  gene_df$name[gene_df$name=="Gl-Rjs"] <- "GL-rjS"
  gene_df$name[gene_df$name=="Gl-Xib"] <- "GL-XIb"
  gene_df$name[gene_df$name=="Gl-Zhw"] <- "GL-ZHw"

  return(gene_df)
}
 # pacman::p_load(tidyverse,targets,readxl)
 # tar_load(gene_file)
# temp<-clean_gene(gene_file)
#
# temp |> group_by(cell_line)|>count()
# temp |> group_by(treatment)|>count()
# temp |> group_by(name)|>count()
