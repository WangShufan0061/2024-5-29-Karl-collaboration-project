
# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tidyverse","readxl","gt","pwr","patchwork","ggrepel","lme4","ggpubr","ggrepel","showtext"), # Packages that your targets need for their tasks.
   format = "rds", # Optionally set the default storage format. qs is fast.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

list(
  tar_file(gene_file,"raw-data/2024-3-4-WIF-tis4d-raw.xlsx"),
  tar_target(gene_raw_EDA, EDA_gene_raw(gene_file)),
  tar_target(gene_clean, clean_gene(gene_file)),
  tar_target(gene_clean_EDA, EDA_gene_cleaned(gene_clean)),
  tar_target(sample_size,gene_samplesize(0.1,0.9,0.05,5)),
  tar_target(gene_models,gene_mixeffect(gene_clean)),
  tar_quarto(readme,"README-R.qmd")
)
