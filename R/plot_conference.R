
plot_conference<- function(gene_clean){
#remove the "GL - "in the variable "name"
gene_clean$name <- str_remove(gene_clean$name, "GL-")
#select the data that need to add labels
wild_type_labels = gene_clean|>filter(cell_line=="Wild-type" & conc==10)
cell_type_labels = gene_clean|>filter(cell_line!="Wild-type" & conc==10)

## Add font
# font_add(
#   family = "times",
#   regular = here::here(
#     "resources","Times New Roman.ttf"
#   )
# )

# Set running
# showtext_auto()
# showtext_opts(dpi=500)#change the font size (use showtext and geom_label simultanously will make the output's font size really small )
# plot cell_line = wild-type
p1<-gene_clean|>
  filter(cell_line=="Wild-type")|>
  ggplot(aes(conc, gene_expression, fill=treatment))+
  geom_point(shape = 21,color = "black",size = 3,stroke = 1)+
  scale_fill_manual(name= "Treatement",values=c("#78A8D1", "#D5BF98"))+
  geom_label_repel(aes(label = name),#add in-figure labels
                   fill = c("#D5BF98","#D5BF98","#78A8D1","#78A8D1"),
                   color = 'black',
                   size = 3.5,
                   data= wild_type_labels,
                   #max.overlaps	=1,
                   xlim = c(10.3, 13),#position
                   direction     = "y",#alligement
                   #family = "times"#font type
                   )+
  scale_x_continuous(breaks = gene_clean$conc[c(T,F,F)])+# specifiy the x-axis
  expand_limits(x=c(0,11))+
  labs(title = "Wild-type",
       x = "\u03bcg/ml",
       #x = expression(mu*"g/ml"),
       y = "Gene Expression")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size=20)
        #text = element_text(family = "times")
        )


p2<-gene_clean|>
  filter(cell_line!="Wild-type")|>
  ggplot(aes(conc, gene_expression, fill=treatment))+
  geom_point(shape = 21,color = "black",size = 3,stroke = 1)+
  scale_fill_manual(name= "Treatement",values=c("#78A8D1", "#D5BF98"))+
  geom_label_repel(aes(label = name),
                   fill = c("#D5BF98","#D5BF98","#78A8D1","#78A8D1"),
                   color = 'black',
                   size = 3.5,
                   data= cell_type_labels,
                   #max.overlaps	=1,
                   xlim = c(10.2, 12),
                   direction     = "y"
                   #family = "times"
  )+
  scale_x_continuous(breaks = gene_clean$conc[c(T,F,F)])+
  expand_limits(x=c(0,11))+
  labs(title = "Cell-type 101",
       x = "\u03bcg/ml",
       #x = expression(mu*"g/ml"),
       y = "Gene Expression")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size=20)
        #text = element_text(family = "times")
        )

# p3<-(p1|p2)+
#   plot_layout(guides = "collect") & theme(legend.position = 'bottom', legend.text = element_text(size=15))&
#   plot_annotation(
#     tag_levels = "A"
#   )



p3<-ggarrange(p1,p2,
          labels = c("A", "B"),
          ncol = 2, nrow = 1,common.legend = TRUE, legend="bottom")





#ggsave(here::here("figs/2024-04-03_Karl_Conference_figure.tiff"),width = 9,height = 6, units = "in",dpi = 500)
#showtext_end()

p3
}
# pacman::p_load(tidyverse,targets,patchwork,hrbrthemes,ggrepel,showtext,ggpubr)
# tar_load(gene_clean)
# plot_conference(gene_clean)

