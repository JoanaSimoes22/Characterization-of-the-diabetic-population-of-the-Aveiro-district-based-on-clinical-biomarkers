# 0 - define the work directory

setwd("~/R studio_files/Biological_Age")

# 1 - check if i have the packages that i will need-----------------------------------------------------------------------------------------

library(BioAge)
library(devtools)
library(tidyverse)
library(reshape2)
library(usethis)
library(RColorBrewer)

# retrieve the citation of the used packages
citation("BioAge")
citation("devtools")
citation("tidyverse")
citation("reshape2")
citation("usethis")

# 2 - Upload the file with the clinical data necessary for the analysis-----------------------------------------------------------------------------------------
DATA = reatidyverseDATA = readxl::read_excel("Base_IB.xlsx", col_names = TRUE)

# 3 - Make the changes needed in the file-----------------------------------------------------------------------------------------
  # Convert the gender; create the ln(crp); create the ln(creat); convert to the right units; convert ID to sample_ID 
DATA %>% 
  mutate(gender = str_replace(gender,"0","2"),
         gender = as.numeric(gender),
         lncrp = log(crp+1),
         lncreat = log(creat+1),
         albumin_gL = albumin*10,
         glucose_mmol = glucose/18,
         creat_umol = creat*88.4,
         lncreat_umol = log(creat_umol+1)) %>% 
  rename(sampleID = ID)-> DATA

write.csv(DATA,"Final_Data_IB.csv") # write a file with the new information

nrow(read.csv("Final_Data_IB.csv")) -> totcount # total number of individuals of the database

# 2 - Calculate the Biological Age with KDM Method using NHANES III as training set-----------------------------------------------------------------------------------------
  # 2.1 - Necessary biomarkers for KDM method
biomarkers = c("albumin","lymph","mcv","glucose","rdw","lncreat","lncrp","alp","wbc","totchol","hba1c","uap")

  # 2.2 - Remove the useless biomarkers for this method and create a file with data for KDM method
DATA %>% 
  select(-c("albumin_gL", "creat", "creat_umol", "lncreat_umol", "glucose_mmol","crp")) -> DATA_kdm

  # 2.3 -  Train the algorithm with NHANES III
train = kdm_calc(NHANES3, biomarkers = c("albumin","lymph","mcv","glucose","rdw","lncreat","lncrp","alp","wbc","totchol","hba1c","uap"))
                 
  # 2.4 - Calculate the IB of my dataset using the trained algorithm
kdm = kdm_calc(DATA_kdm,biomarkers = c("albumin","lymph","mcv","glucose","rdw","lncreat","lncrp","alp","wbc","totchol","hba1c","uap"),
                                fit = train$fit,
                                s_ba2 = train$fit$s_ba2)

  # 2.5 - Extract KDM dataset
data_kdm = kdm$data

# 3 - Calculate the Biological Age with PhenoAge algorithm using NHANES III as training set-----------------------------------------------------------------------------------------
  # 3.1 - Necessary biomarkers for PhenoAge algorithm
biomarkers = c("albumin_gL","lymph","mcv","glucose_mmol","rdw","lncreat_umol","lncrp","alp","wbc")

  # 3.2 - Remove the useless biomarkers for this method and create a file with data for PhenoAge method
DATA %>% 
  select(-c("albumin", "lncreat", "creat", "creat_umol", "glucose","crp", "totchol", "hba1c", "uap")) -> DATA_phenoage

  # 3.3 - Train the algorithm with NHANES III
train = phenoage_calc(NHANES3, biomarkers = c("albumin_gL","lymph","mcv","glucose_mmol","rdw","lncreat_umol","lncrp","alp","wbc"))

  # 3.4 - Calculate the IB of my dataset using the trained algorithm
phenoage = phenoage_calc(DATA_phenoage, biomarkers = c("albumin_gL","lymph","mcv","glucose_mmol","rdw","lncreat_umol","lncrp","alp","wbc"),
                               fit = train$fit)

  # 3.5 - Extract phenoage dataset
data_phenoage = phenoage$data

# 4 - Calculate the Biological Age with HD Method using NHANES III as training set-----------------------------------------------------------------------------------------
  # 4.1 - Necessary biomarkers for HD method
biomarkers = c("albumin_gL","lymph","mcv","glucose_mmol","rdw","lncreat","lncrp","alp","wbc","totchol","hba1c","uap")

  # 4.2 - Remove the useless biomarkers for this method and create a file with data for HD method
DATA %>% 
  select(-c("albumin", "creat", "lncreat_umol","creat_umol", "glucose","crp")) -> DATA_hd

  # 4.3 - Train the algorithm with NHANES III and calculate the IB of my dataset using the trained algorithm
hd = hd_calc(DATA_hd, NHANES3, biomarkers=c("albumin_gL","lymph","mcv","glucose_mmol","rdw","lncreat","lncrp","alp","wbc","totchol","hba1c","uap"))

  # 4.4 - Extract HD dataset
data_hd = hd$data

# 5 - Merge the results of KDM, PhenoAge and HD method
bioage_data = merge(hd$data, kdm$data) %>% merge(., phenoage$data)

write.csv(bioage_data,"bioage_data.csv") # write a file with the biological age data 

# 6 - Other things that will be needed-----------------------------------------------------------------------------------------
  # 6.1 - Create hba1cclasses, genderclass, glicclasses, ageclasses and remove the HD method
bioage_data %>% 
  mutate(
    hba1cclass = case_when( # hba1cclass
      hba1c <5.7 ~ "ND",
      hba1c >=5.7 & hba1c <6.5 ~ "PD", 
      hba1c >=6.5 ~ "D",
      TRUE ~ "erro"),
    name_gender = case_when( # genderclass
      gender == 1 ~ "Masculino",
      gender == 2 ~ "Feminino"),
    glicclass = case_when ( # glicclass
      glucose <70 ~ "H",
      glucose >=70 & glucose < 100 ~ "ND",
      glucose >=100 & glucose <126 ~ "PD",
      glucose >=126 ~ "D",
      TRUE ~ "erro"),
    ageclass = case_when( # ageclass
      age>=18 & age<40 ~ "[18,40[",
      age>=40 & age<60 ~ "[40,60[",
      age>=60 & age<80 ~ "[60,80[",
      age>=80 & age<=100 ~ "[80,100]",
      TRUE ~ "erro"), 
    municipio = str_sub(sampleID, 1, 2)) %>% 
  select(-hd, -hd_log) -> FINAL_DATA # remove HD method 

write_csv(FINAL_DATA, "final_data.csv") # write a file with the biological age data with the modifications

  # 6.2 - Remove other municipality ("outros") just to keep the Aveiro district data-----------------------------------------------------------------------------------------
DATA_AV <- FINAL_DATA %>% 
  filter(municipio != "ou")

write.csv(DATA_AV,"AV_final_data.csv") # write a file to save the biological age data of the Aveiro district

  # 6.3 Separate the Biological age data by method
DATA_AV %>% 
  pivot_longer(cols = c("kdm", "phenoage"),
               names_to = "methods", values_to = "bioage") -> av_sep_methods

write.csv(av_sep_methods,"av_final_data_sep_methods.csv") # write a file to save the biological age data of the Aveiro district by method

  # 6.4 - Separate the Biological age data by advance method (accelerated aging)
DATA_AV %>% 
  pivot_longer(cols = c("kdm_advance", "phenoage_advance"),
               names_to = "methods_advanced", values_to = "accel_bioage") -> av_sep_advanced_methods

write.csv(av_sep_advanced_methods,"av_final_data_sep_advanced_methods.csv") # write a file to save the biological age data of the Aveiro district by advance method

# 7 - Graphics for the descritive analysis-----------------------------------------------------------------------------------------
# 7.1 - Total number of individuals of the Aveiro District
nrow(read.csv("AV_final_data.csv")) -> av_totcount

# 7.2 - Population analysis of the individuals by gender (av_gender_distribution)-----------------------------------------------------------------------------------------
  # gender colours used (can be changed)
    #009ACD --> deepskyblue3
    #FF6EB4--> hotpink1

  # count individuals by gender and create percentage labels
av_gender_counts <- DATA_AV %>% 
  count(name_gender) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  arrange(name_gender)

av_gender_counts <- av_gender_counts %>% 
  mutate(label = paste0(n, " (", sprintf("%.2f", percent), "%)"))

  # av_gender_distribution plot 
png("av_gender_distribution.png", width = 1400, height = 1000)
ggplot(av_gender_counts, aes(x = name_gender, y = n, fill = name_gender)) +
  geom_bar(width = 0.6, stat = "identity") +
  geom_text(aes(label = label), vjust = -0.5, hjust = 0.5, size = 7) +
  labs(title = "Distribuição da população geral por Género Biológico",
       x = "Género", y = "Número de indivíduos", fill = "Género Biológico") +
  scale_fill_manual(values = c("Feminino"= "hotpink1", "Masculino"= "deepskyblue"), labels = c("Feminino", "Masculino"))+
  scale_x_discrete(expand = expansion(mult = c(0, 0.25)), labels = c("Masculino", "Feminino"))+
  scale_y_continuous(limits = c(0,90), breaks = seq(0, 90, by = 5), expand = expansion(mult = c(0, 0.05)))+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face = "bold", size = 30),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 22),
        legend.text = element_text (colour = "black", size = 18),
        axis.title.x=element_blank(),
        axis.title.y=element_text(face ="bold", colour = "black", size = 24),
        axis.text.x=element_blank(),
        axis.text.y=element_text(colour = "black", size = 20),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = "black", linewidth = 0.75),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.3 - Population analysis of the individuals by ageclass (av_ageclass_distribution)-----------------------------------------------------------------------------------------
  # count individuals by ageclass, create percentage labels and define the order of the ageclass
av_ageclass_counts <- DATA_AV %>%
  count(ageclass) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  arrange(ageclass)

av_ageclass_counts <- av_ageclass_counts %>% 
  mutate(label = paste0(n, " (", sprintf("%.2f", percent), "%)"))

av_ageclass_counts$ageclass <- factor(av_ageclass_counts$ageclass, levels = c("[18,40[", "[40,60[", "[60,80[", "[80,100]"))

  # av_ageclass_distribution plot
png("av_ageclass_distribution.png", width = 1400, height = 1000)
ggplot(av_ageclass_counts, aes(x = ageclass, y = n, fill = ageclass)) +
  geom_bar(width = 0.8, stat = "identity") +
  geom_text(aes(label = label), hjust = -0.2, vjust = 0.5, size = 7) +
  labs(title = "Distribuição da população geral por Faixa Etária segundo a Idade Cronológica", x = "Faixa Etária", y = "Nº de indivíduos", fill = "Faixa Etária") +
  scale_fill_brewer(palette = "Set1")+
  scale_x_discrete(expand = expansion(mult = c(0, 0.05)))+
  scale_y_continuous(limits = c(0, 70), breaks= seq(0, 70, by = 5), expand = expansion(mult = c(0, 0.05)))+
  coord_flip()+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", size = 22),
        legend.text = element_text (colour = "black", size = 18),
        axis.title.x = element_text(face ="bold", colour = "black", size = 24),
        axis.title.y = element_text(face ="bold", colour = "black", size = 24),
        axis.text.x = element_text(colour = "black", size = 20),
        axis.text.y = element_text(colour = "black", size = 20),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.75),
        axis.ticks.y = element_line(color = "black", linewidth = 0.75),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.4 - Population analysis of the individuals by municipality (av_mun_distribution)-----------------------------------------------------------------------------------------
  # count individuals by municipality, create percentage labels, define the order of the municipality, define a set of colours
av_mun_counts <- DATA_AV %>%
  count(municipio) %>%
  mutate(percent = n / sum(n) * 100) %>% 
  arrange(municipio)

av_mun_counts <- av_mun_counts %>% 
  mutate(label = paste0(n, " (", sprintf("%.2f", percent), "%)"))
  
ordem_av_municipios <- av_mun_counts %>%
  arrange(desc(n)) %>% 
  pull(municipio)

mun_colors <- adjustcolor(rainbow(13), alpha.f = 0.6)

  # av_mun_distribution plot
png("av_mun_distribution.png", width = 1400, height = 1000)
ggplot(av_mun_counts, aes(x = factor(municipio, levels = ordem_av_municipios), y = n, fill = municipio)) +
  geom_bar(width = 0.8, stat = "identity") +
  geom_text(aes(x = factor(municipio, levels = ordem_av_municipios), y = n, label = label), hjust = -0.2, vjust = 0.5, size = 7) +
  labs(title="Distribuição da população geral no Distrito de Aveiro", 
       x = "Municípios", y = "Nº de Indivíduos")+
  scale_fill_manual(values = mun_colors)+
  scale_x_discrete(expand = expansion(mult = c(0,0.05)),
                   labels= c("ag" = "Águeda", "al"="Albergaria-a-Velha", "an"= "Anadia", 
                             "av" = "Aveiro", "et"= "Estarreja", "il"="Ílhavo", "mu"="Murtosa", 
                             "oa"= "Oliveira de Azeméis", "ob"= "Oliveira do Bairro", "ov"= "Ovar", 
                             "se"="Sever do Vouga","va"= "Vagos"))+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100,by = 5), expand = expansion(mult = c(0,0.12)))+
  coord_flip()+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5, face = "bold", size = 30),
        legend.position = "none", 
        axis.title.x = element_text(face ="bold", colour = "black", size = 24),
        axis.title.y = element_text(face ="bold", colour = "black", size = 24),
        axis.text.x = element_text(colour = "black", size = 20),
        axis.text.y = element_text(colour = "black", size = 20), 
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"),
        axis.ticks.x = element_line(color = "black", linewidth = 0.75),
        axis.ticks.y = element_line(color = "black", linewidth = 0.75),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.5 - Population analysis of the individuals by zone and municipality of the district (3 zones)-----------------------------------------------------------------------------------------
  # create zones and integrate the municipality
zona_norte <- c("ov", "oa")
zona_centro <- c("mu", "ag", "al", "av", "et", "il", "se")
zona_sul <- c("an", "ob", "va")

  # count individuals by zone and municipality, create percentage labels and labels to zones
av_mun_zone_counts <- DATA_AV %>%
  mutate(zona = case_when(
    municipio %in% zona_norte ~ "Zona Norte",
    municipio %in% zona_centro ~ "Zona Centro",
    municipio %in% zona_sul ~ "Zona Sul",
    TRUE ~ NA_character_))

av_mun_zone_counts <- av_mun_zone_counts %>%
  group_by(zona, municipio) %>%
  summarise(n_individuos = n()) %>%
  ungroup()

av_mun_zone_counts <- av_mun_zone_counts %>% 
  mutate(total = sum(n_individuos),
         perc = n_individuos / total * 100,
         label = paste0(n_individuos , "\n (", sprintf("%.2f", perc), "%)"))

av_mun_zone_counts <- av_mun_zone_counts %>%
  mutate(zona = factor(zona, levels = c("Zona Norte", "Zona Centro", "Zona Sul")))

  # mun_zone_distribution plot 
png("mun_zone_distribution.png", width = 1400, height = 1000)
ggplot(av_mun_zone_counts, aes(x = municipio, y = n_individuos, fill = zona)) +
  geom_bar(stat = "identity", width = 0.95) +
  geom_text(aes(label = label), vjust = -0.5, size = 6) +
  scale_fill_manual(values = c("Zona Norte" = "forestgreen", "Zona Centro" = "deepskyblue4", "Zona Sul" = "brown4"))+
  facet_wrap(~ zona, scales = "free") +                  
  scale_x_discrete(labels= c("ag" = "Águeda", "al" = "Albergaria-a-Velha", "an" = "Anadia", "av" = "Aveiro", 
                             "et" = "Estarreja", "il" = "Ílhavo", "mu" = "Murtosa", "oa"= "Oliveira de Azeméis",
                             "ob"= "Oliveira do Bairro", "ov" = "Ovar", "se" = "Sever do Vouga", "va" = "Vagos"),
                   expand = expansion(mult = c(0,0.1)))+
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))+
  labs(title = "Distribuição da população geral no Distrito de Aveiro por Zona", x = "Município", y = "Número de indivíduos") +
  theme_minimal() +
  theme(plot.title=element_text(hjust = 0.5, face = "bold", size = 30),
        strip.text = element_text(face = "bold", size = 24), 
        legend.position = "none", 
        axis.title.x=element_text(face ="bold", colour = "black", size = 22),
        axis.title.y=element_text(face ="bold", colour = "black", size = 22),
        axis.text.x=element_text(colour = "black", size = 20, angle = 45, hjust = 1),
        axis.text.y=element_text(colour = "black", size = 20),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"),
        axis.ticks.x = element_line(color = "black", linewidth = 0.75),
        axis.ticks.y = element_line(color = "black", linewidth = 0.75),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.6 - Population analysis of the individuals by gender, ageclass and municipality (av_age_gender_mun_distribution)-----------------------------------------------------------------------------------------
# count individuals by gender/ageclass/municipality, create percentage labels and define the order of the minucipality, ageclass and gender
av_age_gender_mun_counts <- DATA_AV %>% 
  count(municipio, name_gender, ageclass)

av_municipio_totals <- av_age_gender_mun_counts %>% 
  group_by(municipio) %>% 
  summarise (total = sum (n)) %>% 
  mutate(nome_municipio = recode(municipio, "ag" = "Águeda", "al"="Albergaria-a-Velha", "an"= "Anadia", 
                                 "av" = "Aveiro", "et"= "Estarreja", "il"="Ílhavo", "mu"="Murtosa", 
                                 "oa"= "Oliveira de Azeméis", "ob"= "Oliveira do Bairro", "ov"= "Ovar", 
                                 "se"="Sever do Vouga","va"= "Vagos"),
         municipio_label = paste0(nome_municipio, "\n(n = ", total, ")"))

av_age_gender_mun_counts <- av_age_gender_mun_counts %>%
  left_join(av_municipio_totals %>% select(municipio, municipio_label), by = "municipio")

av_age_gender_mun_counts$name_gender <- factor(av_age_gender_mun_counts$name_gender, levels = c("Feminino", "Masculino"))
av_age_gender_mun_counts$ageclass <- factor(av_age_gender_mun_counts$ageclass, levels = c("[18,40[", "[40,60[", "[60,80[", "[80,100]"))

  # av_age_gender_mun_distribution plot 
png("av_age_gender_mun_distribution.png", width = 1400, height = 1000)
ggplot(av_age_gender_mun_counts, aes(x = ageclass, y = n, fill = name_gender)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9), width = 0.9) +
  geom_text(aes(x = ageclass, label = n, group = name_gender), 
            position = position_dodge2(width = 0.9, preserve='single'), 
            hjust = -0.6, vjust = 0.3, size = 5) +
  labs(title = "Distribuição da população geral por Género Biológico e Faixa Etária no Distrito de Aveiro", x = "Faixa Etária", 
       y = "Nº de Indivíduos", fill = "Género Biológico")+
  scale_fill_manual(values = c("Feminino" = "hotpink1", "Masculino" = "deepskyblue", labels = c("Feminino", "Masculino")))+
  coord_flip()+
  scale_x_discrete(expand = expansion(mult = c(0, 0.2)))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  facet_wrap(~municipio_label, scales = 'free', ncol = 6)+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face = "bold", size = 26),
        strip.text = element_text(size = 16, face = "bold", colour = "black"),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 18),
        legend.text = element_text (colour = "black", size = 16),
        axis.title.x = element_text(face ="bold", colour = "black", size = 20),
        axis.title.y = element_text(face ="bold", colour = "black", size = 20),
        axis.text.x = element_text(colour = "black", size = 14),
        axis.text.y = element_text(colour = "black", size = 14),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.7 - Distribution of IB by IC for each method-----------------------------------------------------------------------------------------
  # av_bioage_ic_distribution plot
png("av_bioage_ic_distribution.png", width = 1400, height = 1000)
linha_identidade <- data.frame(x = c(15, 100), y = c(15, 100))
ggplot(av_sep_methods, aes(x = age, y = bioage)) +
  geom_point(alpha = 1, color = "tan3", size = 3) +
  geom_smooth(aes(linetype = "Linha de Regressão (LM)"), method = "lm", se = FALSE, color = "red", linewidth = 0.9) +
  geom_line(data = linha_identidade, aes(x = x, y = y, linetype = "Linha de Identidade (y=x)"), color = "black", linewidth = 1.2) +
  facet_wrap(~methods, labeller = as_labeller(c("kdm" = "Método KDM","phenoage" = "Método PhenoAge")), scales = "free") +
  labs(title = "Comparação entre a Idade Cronológica e as Idades Biológicas calculadas pelos métodos",
       x = "Idade Cronológica",
       y = "Idade Biológica") +
  scale_linetype_manual(name = "Linhas no Gráfico", values = c("Linha de Regressão (LM)" = "dashed", "Linha de Identidade (y=x)" = "dotted"))+
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 135, 5))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        strip.text = element_text(face = "bold", colour = "black", size = 24),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (colour = "black", size = 18),
        axis.title.x = element_text(face ="bold", colour = "black", size = 22),
        axis.title.y = element_text(face ="bold", colour = "black", size = 22),
        axis.text.x = element_text(colour = "black", size = 16),
        axis.text.y = element_text(colour = "black", size = 16),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.8 - Distribution of IB by ageclass for each method-----------------------------------------------------------------------------------------
  # create a set of colours
colours_ic <- brewer.pal(5, "Oranges")
colours_ic <- colours_ic[-1]

  # av_ib_ageclass_distribution plot
png("av_ib_ageclass_distribution.png", width = 1400, height = 1000)
ggplot(av_sep_methods, aes(x = ageclass, y = bioage, fill = ageclass)) +
  geom_boxplot(alpha = 0.9, outlier.shape = 21, outlier.size = 4) +
  facet_wrap(~methods, labeller = as_labeller(c("kdm" = "Método KDM", "phenoage" = "Método PhenoAge")), scales = "free") +
  labs(title = "Distribuição da Idade Biológica da população geral por Faixa Etária", x = "Faixa Etária", y = "Idade Biológica") +
  scale_fill_manual(values = colours_ic, name = "Faixa Etária") +
  scale_y_continuous(breaks = seq(0, 150, 10))+
  theme_minimal() +
  theme(plot.title=element_text(hjust = 0.5, face = "bold", size = 30),
        strip.text = element_text(face = "bold", colour = "black", size = 24),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (colour = "black", size = 18),
        axis.title.x = element_blank(),
        axis.title.y=element_text(face ="bold", colour = "black", size = 24),
        axis.text.y=element_text(colour = "black", size = 20),
        axis.text.x = element_blank(),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.9 - Distribution of IB by gender for each method-----------------------------------------------------------------------------------------
av_sep_methods$name_gender <- factor(av_sep_methods$name_gender, levels = c("Feminino", "Masculino"))

  # av_bioage_gender_distribution plot
png("av_bioage_gender_distribution.png", width = 1400, height = 1000)
ggplot(av_sep_methods, aes(x = name_gender, y = bioage, fill = name_gender)) +
  geom_boxplot(alpha = 0.9, outlier.shape = 21, outlier.size = 4) +
  facet_wrap(~methods, labeller = as_labeller(c("kdm" = "Método KDM", "phenoage" = "Método PhenoAge")), scales = "free") +
  labs(title = "Distribuição da Idade Biológica da população geral por Género Biológico", x = "Género Biológico", y = "Idade Biológica") +
  scale_fill_manual(values = c("Feminino" = "hotpink1", "Masculino" = "deepskyblue"),
                    labels = c("Feminino", "Masculino"), name = "Género Biológico") +
  scale_y_continuous(breaks = seq(0, 160, 5)) +
  theme_minimal() +
  theme(plot.title=element_text(hjust = 0.5, face = "bold", size = 30),
        strip.text = element_text(face = "bold", colour = "black", size = 24),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (colour = "black", size = 18),
        axis.title.x = element_blank(),
        axis.title.y=element_text(face ="bold", colour = "black", size = 24),
        axis.text.y=element_text(colour = "black", size = 20),
        axis.text.x = element_blank(),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.10 - Distribution of IB by hba1cclass for each method-----------------------------------------------------------------------------------------
av_sep_methods$hba1cclass <- factor(av_sep_methods$hba1cclass, levels = c("ND", "PD", "D"))

  # av_bioage_hba1cclass_distribution plot
png("av_bioage_hba1cclass_distribution.png", width = 1400, height = 1000)
ggplot(av_sep_methods, aes(x = hba1cclass, y = bioage, fill = hba1cclass)) +
  geom_boxplot(alpha = 0.9, outlier.shape = 21, outlier.size = 4) +
  facet_wrap(~methods, labeller = as_labeller(c("kdm" = "Método KDM", "phenoage" = "Método PhenoAge")), scales = "free") +
  scale_fill_manual(values = c("ND"= "mediumseagreen", "PD"= "gold3", "D" = "brown3"), labels = c("ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"),
                    name = "Classes de Hemoglobina Glicada")+
  scale_y_continuous(breaks = seq(0, 160, 5)) +
  labs(title = "Distribuição da Idade Biológica da população geral por Classes de Hemoglobina Glicada", x = "Classe de Hemoglobina Glicada", y = "Idade Biológica") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        strip.text = element_text(face = "bold", colour = "black", size = 24),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (colour = "black", size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face ="bold", colour = "black", size = 24),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "black", size = 20),
        axis.line.x.bottom = element_line(color = "grey61"),
        axis.line.y.left = element_line(color = "grey61"), 
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.11 - Distribution of IB of individuals of hba1cclass by gender for each method-----------------------------------------------------------------------------------------
av_sep_methods$name_gender <- factor(av_sep_methods$name_gender, levels = c("Feminino", "Masculino"))

  # av_bioage_hba1cclass_gender_distribution plot
png("av_bioage_hba1cclass_gender_distribution.png", width = 1400, height = 1000)
ggplot(av_sep_methods, aes(x = hba1cclass, y = bioage, fill = name_gender)) +
  geom_boxplot(alpha = 0.9, outlier.shape = 21, outlier.size = 4, position = position_dodge(width = 0.8)) +
  facet_wrap(~methods, labeller = as_labeller(c("kdm" = "Método KDM", "phenoage" = "Método PhenoAge")), scales = "free") +
  scale_fill_manual(values = c("Feminino" = "hotpink1", "Masculino" = "deepskyblue"), 
                    name = "Género Biológico") +
  scale_x_discrete(labels = c("ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"))+
  scale_y_continuous(breaks = seq(20, 150, 5)) +
  labs(title = "Idade Biológica por Género Biológico segundo as Classes de Hemoglobina Glicada", x = "Classe de Hemoglobina Glicada", y = "Idade Biológica") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        strip.text = element_text(face = "bold", colour = "black", size = 24),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (colour = "black", size = 18),
        axis.title.x = element_text(face ="bold", colour = "black", size = 24),
        axis.title.y = element_text(face ="bold", colour = "black", size = 24),
        axis.text.x = element_text(colour = "black", size = 22),
        axis.text.y = element_text(colour = "black", size = 22),
        axis.line.x.bottom = element_line(color = "grey61"),
        axis.line.y.left = element_line(color = "grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.12 - Distribution of IB of individuals by glicclass for each method-----------------------------------------------------------------------------------------
av_sep_methods$glicclass <- factor(av_sep_methods$glicclass, levels = c("H","ND", "PD", "D"))

  # av_bioage_glicclass_distribution plot
png("av_bioage_glicclass_distribution.png", width = 1400, height = 1000)
ggplot(av_sep_methods, aes(x = glicclass, y = bioage, fill = glicclass)) +
  geom_boxplot(alpha = 0.9, outlier.shape = 21, outlier.size = 4) +
  facet_wrap(~methods, labeller = as_labeller(c("kdm" = "Método KDM", "phenoage" = "Método PhenoAge")), scales = "free") +
  scale_fill_manual(values = c("H" = "royalblue","ND"= "mediumseagreen", "PD"= "gold3", "D" = "brown3"), 
                    labels = c("H" = "Hipoglicémico","ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"), name = "Classes de Glicose em Jejum")+
  scale_y_continuous(breaks = seq(0, 150, 5)) +
  labs(title = "Distribuição da Idade Biológica da população geral por Classes de Glicose em Jejum", x = "Classe de Glicose em Jejum", y = "Idade Biológica") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        strip.text = element_text(face = "bold", colour = "black", size = 24),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (colour = "black", size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face ="bold", colour = "black", size = 24),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "black", size = 20),
        axis.line.x.bottom = element_line(color = "grey61"),
        axis.line.y.left = element_line(color = "grey61"), 
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.13 - Distribution of IB of individuals of glicclass by gender for each method-----------------------------------------------------------------------------------------
av_sep_methods$name_gender <- factor(av_sep_methods$name_gender, levels = c("Feminino", "Masculino"))

  # av_bioage_glicclass_gender_distribution plot
png("av_bioage_glicclass_gender_distribution.png", width = 1400, height = 1000)
ggplot(av_sep_methods, aes(x = glicclass, y = bioage, fill = name_gender)) +
  geom_boxplot(alpha = 0.9, outlier.shape = 21, outlier.size = 4, position = position_dodge(width = 0.8)) +
  facet_wrap(~methods, labeller = as_labeller(c("kdm" = "Método KDM", "phenoage" = "Método PhenoAge")), scales = "free") +
  scale_fill_manual(values = c("Feminino" = "hotpink1", "Masculino" = "deepskyblue"), label = c("Feminino", "Masculino"), name = "Género Biológico") +
  scale_x_discrete(labels = c("H" = "Hipoglicémico","ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"))+
  scale_y_continuous(breaks = seq(0, 150, 5)) +
  labs(title = "Idade Biológica por Género Biológico segundo as Classes de Glicose em Jejum", x = "Classe de Glicose em Jejum", y = "Idade Biológica") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        strip.text = element_text(face = "bold", colour = "black", size = 24),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (colour = "black", size = 18),
        axis.title.x = element_text(face ="bold", colour = "black", size = 24),
        axis.title.y = element_text(face ="bold", colour = "black", size = 24),
        axis.text.x = element_text(colour = "black", size = 22),
        axis.text.y = element_text(colour = "black", size = 22),
        axis.line.x.bottom = element_line(color = "grey61"),
        axis.line.y.left = element_line(color = "grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.14 - Distribution of Accelerated Aging of individuals by hba1cclass for each method----------------------------------------------------------------------------------------- 
av_sep_advanced_methods$hba1cclass <- factor(av_sep_advanced_methods$hba1cclass, levels = c("ND", "PD", "D"))

  # av_accel_bioage_hba1cclass_distribution plot
png("av_accel_bioage_hba1cclass_distribution.png", width = 1400, height = 1000)
ggplot(av_sep_advanced_methods, aes(x = hba1cclass, y = accel_bioage, fill = hba1cclass)) +
  geom_boxplot(alpha = 0.9, outlier.shape = 21, outlier.size = 4) +
  facet_wrap(~methods_advanced, labeller = as_labeller(c("kdm_advance" = "Método KDM", "phenoage_advance" = "Método PhenoAge")), scales = "free") +
  scale_fill_manual(values = c("ND"= "mediumseagreen", "PD"= "gold3", "D" = "brown3"), 
                    labels = c("ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"), name = "Classes de Hemoglobina Glicada")+
  scale_y_continuous(breaks = seq(-25, 80, 5)) +
  labs(title = "Aceleração do Envelhecimento da população geral por Classes de Hemoglobina Glicada",
       x = "Classe de Hemoglobina Glicada",
       y = "Aceleração do Envelhecimento") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        strip.text = element_text(face = "bold", colour = "black", size = 24),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (colour = "black", size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face ="bold", colour = "black", size = 24),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "black", size = 20),
        axis.line.x.bottom = element_line(color = "grey61"),
        axis.line.y.left = element_line(color = "grey61"), 
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.15 - Distribution of Accelerated Aging of individuals of hba1cclass by gender for each method-----------------------------------------------------------------------------------------
av_sep_advanced_methods$name_gender <- factor(av_sep_advanced_methods$name_gender, levels = c("Feminino", "Masculino"))
av_sep_advanced_methods$hba1cclass <- factor(av_sep_advanced_methods$hba1cclass, levels = c("ND", "PD", "D"))

  # av_accel_bioage_hba1cclass_gender_distribution plot
png("av_accel_bioage_hba1cclass_gender_distribution.png", width = 1400, height = 1000)
ggplot(av_sep_advanced_methods, aes(x = hba1cclass, y = accel_bioage, fill = name_gender)) +
  geom_boxplot(alpha = 0.9, outlier.shape = 21, outlier.size = 4, position = position_dodge(width = 0.8)) +
  facet_wrap(~methods_advanced, labeller = as_labeller(c("kdm_advance" = "Método KDM", "phenoage_advance" = "Método PhenoAge")), scales = "free") +
  scale_fill_manual(values = c("Feminino" = "hotpink1", "Masculino" = "deepskyblue"), labels = c("Feminino", "Masculino"), name = "Género Biológico") +
  scale_x_discrete(labels = c("ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"))+
  scale_y_continuous(breaks = seq(-20, 70, 5)) +
  labs(title = "Aceleração do Envelhecimento por Género Biológico por Classes de Hemoglobina Glicada",
       x = "Classe de Hemoglobina Glicada",
       y = "Aceleração do Envelhecimento") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        strip.text = element_text(face = "bold", colour = "black", size = 24),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (colour = "black", size = 18),
        axis.title.x = element_text(face ="bold", colour = "black", size = 24),
        axis.title.y = element_text(face ="bold", colour = "black", size = 24),
        axis.text.x = element_text(colour = "black", size = 20),
        axis.text.y = element_text(colour = "black", size = 20),
        axis.line.x.bottom = element_line(color = "grey61"),
        axis.line.y.left = element_line(color = "grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.16 - Distribution of Accelerated Aging of individuals by glicclass for each method-----------------------------------------------------------------------------------------  
av_sep_advanced_methods$glicclass <- factor(av_sep_advanced_methods$glicclass, levels = c("H","ND", "PD", "D"))

  # av_accel_bioage_glicclass_distribution plot
png("av_accel_bioage_glicclass_distribution.png", width = 1400, height = 1000)
ggplot(av_sep_advanced_methods, aes(x = glicclass, y = accel_bioage, fill = glicclass)) +
  geom_boxplot(alpha = 0.9, outlier.shape = 21, outlier.size = 4) +
  facet_wrap(~methods_advanced, labeller = as_labeller(c("kdm_advance" = "Método KDM", "phenoage_advance" = "Método PhenoAge")), scales = "free") +
  scale_fill_manual(values = c("H" = "royalblue","ND"= "mediumseagreen", "PD"= "gold3", "D" = "brown3"), 
                    labels = c("H" = "Hipoglicémico","ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"), name = "Classes de Glicose em Jejum")+
  scale_y_continuous(breaks = seq(-20, 80, 5)) +
  labs(title = "Aceleração do Envelhecimento da população geral por Classes de Glicose em Jejum",
       x = "Classe de Glicose em Jejum",
       y = "Aceleração do Envelhecimento") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        strip.text = element_text(face = "bold", colour = "black", size = 24),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (colour = "black", size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face ="bold", colour = "black", size = 24),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "black", size = 20),
        axis.line.x.bottom = element_line(color = "grey61"),
        axis.line.y.left = element_line(color = "grey61"), 
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 7.17 - Distribution of Accelerated Aging of individuals of glicclass by gender for each method-----------------------------------------------------------------------------------------
av_sep_advanced_methods$name_gender <- factor(av_sep_advanced_methods$name_gender, levels = c("Feminino", "Masculino"))

  # av_accel_bioage_glicclass_gender_distribution plot
png("av_accel_bioage_glicclass_gender_distribution.png", width = 1400, height = 1000)
ggplot(av_sep_advanced_methods, aes(x = glicclass, y = accel_bioage, fill = name_gender)) +
  geom_boxplot(alpha = 0.9, outlier.shape = 21, outlier.size = 4, position = position_dodge(width = 0.8)) +
  facet_wrap(~methods_advanced, labeller = as_labeller(c("kdm_advance" = "Método KDM", "phenoage_advance" = "Método PhenoAge")), scales = "free") +
  scale_fill_manual(values = c("Feminino" = "hotpink1", "Masculino" = "deepskyblue"), labels = c("Feminino", "Masculino"), name = "Género Biológico") +
  scale_x_discrete(labels = c("H" = "Hipoglicémico","ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"))+
  scale_y_continuous(breaks = seq(-20, 70, 5)) +
  labs(title = "Aceleração do Envelhecimento por Género Biológico por Classes de Glicose em Jejum", x = "Classe de Glicose em Jejum", y = "Aceleração do Envelhecimento") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        strip.text = element_text(face = "bold", colour = "black", size = 24),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (colour = "black", size = 18),
        axis.title.x = element_text(face ="bold", colour = "black", size = 24),
        axis.title.y = element_text(face ="bold", colour = "black", size = 24),
        axis.text.x = element_text(colour = "black", size = 20),
        axis.text.y = element_text(colour = "black", size = 20),
        axis.line.x.bottom = element_line(color = "grey61"),
        axis.line.y.left = element_line(color = "grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 8 - Correlation between the clinical biomarkers, IC, IB KDM e IB PhenoAge without transform variables (in attachments)
ggplot(DATA_AV, aes(x = age)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Histograma da Idade Cronológica") # not normal

ggplot(DATA_AV, aes(x = mcv)) + 
  geom_histogram(bins = 30, fill = "firebrick", color = "black") +
  labs(title = "Histograma da mcv") # not normal 

ggplot(DATA_AV, aes(x = rdw)) + 
  geom_histogram(bins = 30, fill = "firebrick", color = "black") +
  labs(title = "Histograma da rdw") # not normal

ggplot(DATA_AV, aes(x = wbc)) + 
  geom_histogram(bins = 30, fill = "firebrick", color = "black") +
  labs(title = "Histograma da wbc") # not normal 

ggplot(DATA_AV, aes(x = lymph)) + 
  geom_histogram(bins = 30, fill = "firebrick", color = "black") +
  labs(title = "Histograma da lymph") # not normal

ggplot(DATA_AV, aes(x = alp)) + 
  geom_histogram(bins = 30, fill = "firebrick", color = "black") +
  labs(title = "Histograma da alp") # not normal 

ggplot(DATA_AV, aes(x = lncrp)) + 
  geom_histogram(bins = 30, fill = "firebrick", color = "black") +
  labs(title = "Histograma da lncrp") # not normal

ggplot(DATA_AV, aes(x = totchol)) + 
  geom_histogram(bins = 30, fill = "firebrick", color = "black") +
  labs(title = "Histograma da totchol") # not normal

ggplot(DATA_AV, aes(x = hba1c)) + 
  geom_histogram(bins = 30, fill = "firebrick", color = "black") +
  labs(title = "Histograma da hba1c") # not normal 

ggplot(DATA_AV, aes(x = glucose)) + 
  geom_histogram(bins = 30, fill = "firebrick", color = "black") +
  labs(title = "Histograma da glucose") # not normal 

ggplot(DATA_AV, aes(x = lncreat)) + 
  geom_histogram(bins = 30, fill = "firebrick", color = "black") +
  labs(title = "Histograma da lncreat") # not normal

ggplot(DATA_AV, aes(x = albumin)) + 
  geom_histogram(bins = 30, fill = "firebrick", color = "black") +
  labs(title = "Histograma da albumin") # not normal

ggplot(DATA_AV, aes(x = uap)) + 
  geom_histogram(bins = 30, fill = "firebrick", color = "black") +
  labs(title = "Histograma da uap") # not normal

# create the correlation matrix
axis <- c("mcv", "rdw", "albumin", "lymph", "lncreat", "lncrp", "alp", "wbc", "totchol", "uap", "hba1c", "glucose", "kdm", "phenoage", "age")
cor_data_biomarkers <- DATA_AV [, axis]
cor_data_biomarkers <- na.omit(cor_data_biomarkers)

# test the correlation with the spearman correlation test
cor_data_biomarkers_matrix <- cor(cor_data_biomarkers, use = "complete.obs", method = "spearman")
print(cor_data_biomarkers_matrix)

corlong_data_biomarkers <- melt(cor_data_biomarkers_matrix, varnames = c("axis1", "axis2"), value.name = "Correlacao")

# cor_biomarkers_age_statistics plot
png("cor_biomarkers_age_statistics.png", width = 1400, height = 1000)
ggplot(corlong_data_biomarkers, aes(x = axis1, y = axis2, fill = Correlacao)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "royalblue4", mid = "white", high = "darkred", midpoint = 0, limits = c(-1, 1)) +
  geom_text(aes(label = round(Correlacao, 2)), size = 7, color = "black") +
  labs(title = "Relação entre os Biomarcadores em estudo e a Idade Cronológica da população geral", x = "", y = "", 
       fill = "Valor de ρ")+
  scale_x_discrete(labels = c(age = "Idade \n Cronológica")) +
  scale_y_discrete(labels = c(age = "Idade Cronológica")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        legend.position = "right", 
        legend.direction = "vertical",
        legend.justification = "center",
        legend.box.just = "center",
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (hjust = 0.5, colour = "black", size = 16),
        axis.title.x=element_text(face ="bold", colour = "black", size = 24),
        axis.title.y=element_text(face ="bold", colour = "black", size = 24),
        axis.text.x=element_text(colour = "black", face = "bold", size = 20, angle = 45, margin = margin(t = 15), hjust = 0.5, vjust = 1),
        axis.text.y=element_text(colour = "black", face = "bold", size = 20),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()