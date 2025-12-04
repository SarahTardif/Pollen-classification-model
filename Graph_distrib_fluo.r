library(dplyr)
library(tidyr)
library(ggplot2)

### Figure principale avec les espèces pour genres principaux ###
## charger les données 
trainset<-read.csv('./inputs_outputs/trainset_species_V2.csv',h=T)
trainset$taxon<-trainset$Class
trainset$taxon<-as.factor(trainset$taxon)
trainset$Class<-NULL

# Sélectionner les variables d'intérêt
df_plot <- trainset %>%
  select(taxon, Violet610_A, PB450_A, FITC_A, SSC_A, FSC_A) %>%
  pivot_longer(cols = -taxon, names_to = "variable", values_to = "value")

# avec données log transformées 
df_plot_norm <- df_plot %>%
  mutate(value_log = log10(value + 1))  # +1 pour éviter log(0)

df_plot_norm$Genus <- sub("_.*", "", df_plot_norm$taxon)
df_plot_norm$Genus<-as.factor(df_plot_norm$Genus)
df_plot_norm <- df_plot_norm %>%
  filter(Genus %in% c("Alnus", "Betula", "Corylus", "Pinus","Quercus","Salix"))

maingraph<-ggplot(df_plot_norm, aes(x = taxon, y = value_log, fill = Genus)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~variable, scales = "free_y",ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15),
      axis.text.y = element_text(size =15),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.text  = element_text(size = 15), 
      legend.title = element_text(size = 15),
      strip.text   = element_text(size = 15),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 20)) +
  labs(y = "Log10(Intensity + 1)", x = "Species")+
  scale_fill_viridis_d(option = "viridis")
maingraph

ggsave("./inputs_outputs/distrib_fluo_mainfig.png", maingraph, width = 15, height = 15, dpi = 300)



### Figure pour tous les genres ###
## charger les données 
trainset<-read.csv('./inputs_outputs/trainset_genus_V2.csv',h=T)
trainset$taxon<-trainset$Class
trainset$taxon<-as.factor(trainset$taxon)
trainset$Class<-NULL

# Sélectionner les variables d'intérêt
df_plot <- trainset %>%
  select(taxon, Violet610_A, PB450_A, FITC_A, SSC_A, FSC_A) %>%
  pivot_longer(cols = -taxon, names_to = "variable", values_to = "value")

# avec données log transformées 
df_plot_norm <- df_plot %>%
  mutate(value_log = log10(value + 1))  # +1 pour éviter log(0)

graphgenus<-ggplot(df_plot_norm, aes(x = forcats::fct_relevel(taxon, "OTHER", after = Inf), y = value_log)) + 
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~variable, scales = "free_y",ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15),
      axis.text.y = element_text(size =15),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      strip.text   = element_text(size = 13),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 20)) +
  labs(y = "Log10(Intensity + 1)", x = "Genus")
graphgenus

ggsave("./inputs_outputs/distrib_fluo_totgenus.png", graphgenus, width = 15, height = 15, dpi = 300)


### Figure pour toutes les espèces ###
## charger les données 
trainset<-read.csv('./inputs_outputs/trainset_species_V2.csv',h=T)
trainset$taxon<-trainset$Class
trainset$taxon<-as.factor(trainset$taxon)

trainset$Class<-NULL

# Sélectionner les variables d'intérêt
df_plot <- trainset %>%
  select(taxon, Violet610_A, PB450_A, FITC_A, SSC_A, FSC_A) %>%
  pivot_longer(cols = -taxon, names_to = "variable", values_to = "value")

# avec données log transformées 
df_plot_norm <- df_plot %>%
  mutate(value_log = log10(value + 1))  # +1 pour éviter log(0)

df_plot_norm$genus <- sub("_.*", "", df_plot_norm$taxon)
df_plot_norm$genus<-as.factor(df_plot_norm$genus)

graphsp<-ggplot(df_plot_norm, aes(x = forcats::fct_relevel(taxon, "OTHER", after = Inf), y = value_log, fill=genus)) + 
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~variable, scales = "free_y",ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
      axis.text.y = element_text(size =15),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15), 
      strip.text   = element_text(size = 13),
      legend.position="none") +
  labs(y = "Log10(Intensity + 1)", x = "Species")
graphsp

ggsave("./inputs_outputs/distrib_fluo_totspecies.png", graphsp, width = 15, height = 15, dpi = 300)
