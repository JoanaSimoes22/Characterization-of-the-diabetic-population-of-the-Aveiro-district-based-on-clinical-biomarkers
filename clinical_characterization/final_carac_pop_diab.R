# 0 - define the work directory

setwd("~/R studio_files/A1C_GLIC") 

# 1 - check if i have the packages that i will need--------------------------------------------------------------------
library(devtools)
library(tidyverse)
library(reshape2)
library(DescTools)
library(usethis)
library(stats)

# retrieve the citation of the used packages
citation("devtools")
citation("tidyverse")
citation("reshape2")
citation("DescTools")
citation("usethis")
citation("stats")

# 2 - Upload the file with the clinical information-------------------------------------------------------------------
DATA = reatidyverseDATA = readxl::read_excel("a1c_glic.xlsx", col_names = TRUE)

# 3 - Make the changes needed in the file--------------------------------------------------------------------------
  # create ageclasses; create a1cclasses; create glicclasses; create genderclasses
DATA <- DATA %>%
  mutate(
    ageclass = case_when( # ageclass
      age>=0 & age<20 ~ "[0,20[",
      age>=20 & age<40 ~ "[20,40[",
      age>=40 & age<60 ~ "[40,60[",
      age>=60 & age<80 ~ "[60,80[",
      age>=80 & age<=105 ~ "[80,105]",
      TRUE ~ NA_character_), 
    a1cclass = case_when( # a1cclass
      a1c<5.7 ~ "ND",
      a1c>=5.7 & a1c<6.5 ~ "PD",
      a1c>=6.5 ~"D"),
    glicclass = case_when( # glicclass
      glic<70 ~ "H",
      glic>=70 & glic<100 ~ "ND",
      glic>=100 & glic<126 ~ "PD",
      glic>=126 ~ "D"),
    gender = str_replace(gender,"0", "Feminino"), # genderclass
    gender = str_replace(gender,"1", "Masculino"))

write.csv(DATA,"carac_database.csv") # write a file with the new information

nrow(read.csv("carac_database.csv")) -> totcount # total number of individuals of the database

# 4 - Analysis of the population of the Aveiro District--------------------------------------------------------------
  # filter the previous database to only have the Aveiro District patients
DATA_AV <- DATA %>% 
  dplyr::filter(!grepl("ou", municipio, ignore.case = TRUE))

write.csv(DATA_AV,"carac_AV_database.csv") # write a file for the Aveiro District population

# 4.1 - total number of individuals of the Aveiro District -----------------------------------------------------------------
nrow(read.csv("carac_AV_database.csv")) -> av_totcount

# 4.2 - Population analysis of the individuals by gender (Gender_distribution)---------------------------------------------------------------------
  # gender colours used (can be changed)
    #009ACD --> deepskyblue3
    #FF6EB4--> hotpink1

  # count individuals by gender and create percentage labels
av_gender_counts <- DATA_AV %>% 
  count(gender) %>%  
  mutate(percent = n / sum(n) * 100) %>% 
  arrange(gender)

av_gender_counts <- av_gender_counts %>% 
  mutate(label = paste0(n, " (", sprintf("%.2f", percent), "%)"))

  # Gender_distribution plot
png("gender_distribution.png", width = 1400, height = 1000)
ggplot(av_gender_counts, aes(x = gender, y = n, fill = gender)) +
  geom_bar(width = 0.6, stat = "identity") +
  geom_text(aes(label = label), vjust = -0.5, hjust = 0.5, size = 8) +
  labs(title = "Distribuição da população geral por género biológico", 
       y = "Número de indivíduos", fill = "Género Biológico") +
  scale_fill_manual(values = c("Feminino"= "hotpink1", "Masculino"= "deepskyblue"), labels = c("Feminino", "Masculino"))+
  scale_x_discrete(expand = expansion(mult = c(0, 0.25)), labels = c("Feminino", "Masculino"))+
  scale_y_continuous(limits = c(0,13000), breaks = seq(0, 13000, by = 1000), expand = expansion(mult = c(0, 0.05)))+
  #coord_flip()
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5, face = "bold", size = 34),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 26),
        legend.text = element_text (colour = "black", size = 24),
        axis.title.x=element_blank(),
        axis.title.y=element_text(face ="bold", colour = "black", size = 28),
        axis.text.x=element_blank(),
        axis.text.y=element_text(colour = "black", size = 24),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = "black", linewidth = 0.75),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 4.3 - Population analysis of the individuals by ageclass (ageclass_distribution)-----------------------------------------------------------------
  # count individuals by ageclass, create percentage labels and define the order of the ageclass
av_age_counts <- DATA_AV %>%
  count(ageclass) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  arrange(ageclass)

av_age_counts <- av_age_counts %>% 
  mutate(label = paste0(n, " (", sprintf("%.2f", percent), "%)"))

av_age_counts$ageclass <- factor(av_age_counts$ageclass, levels = c("[0,20[", "[20,40[", "[40,60[", "[60,80[", "[80,105]"))

  # ageclass_distribution plot
png("ageclass_distribution.png", width = 1400, height = 1000)
ggplot(av_age_counts, aes(x = ageclass, y = n, fill = ageclass)) +
  geom_bar(width = 0.8, stat = "identity") +
  geom_text(aes(label = label), hjust = -0.05, vjust = 0.5, size = 7) +
  labs(title = "Distribuição da população geral por faixa etária", x = "Faixa Etária", y = "Nº de indivíduos", fill = "Faixa Etária") +
  scale_fill_brewer(palette = "Set1")+
  scale_x_discrete(expand = expansion(mult = c(0,0.05)))+
  scale_y_continuous(limits = c(0, 13000), breaks= seq(0, 13000,by = 1000), expand = expansion(mult = c(0, 0.075)))+
  coord_flip()+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5, face = "bold", size = 30),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 22),
        legend.text = element_text (colour = "black", size = 20),
        axis.title.x=element_text(face ="bold", colour = "black", size = 24),
        axis.title.y=element_text(face ="bold", colour = "black", size = 24),
        axis.text.x=element_text(colour = "black", size = 22),
        axis.text.y=element_text(colour = "black", size = 22),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.75),
        axis.ticks.y = element_line(color = "black", linewidth = 0.75),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 4.4 - Population analysis of the individuals by gender, ageclass and municipality (age_gender_mun_distribution)--------------------------------------------------------
  # count individuals by gender/ageclass/municipality, create percentage labels and define the order of the ageclass and municipality
av_age_gender_mun_counts <- DATA_AV %>% 
  count(municipio, gender,ageclass)

av_age_gender_mun_counts$ageclass <- factor(av_age_gender_mun_counts$ageclass, 
                                        levels = c("[0,20[", "[20,40[", "[40,60[", "[60,80[", "[80,105]"))

municipio_totals <- av_age_gender_mun_counts %>% 
  group_by(municipio) %>% 
  summarise (total = sum (n)) %>% 
  mutate(nome_municipio = recode(municipio, "ag" = "Águeda", "al"="Albergaria-a-Velha", "an"= "Anadia", 
                                 "av" = "Aveiro", "et"= "Estarreja", "il"="Ílhavo", 
                                 "me"="Mealhada", "mu"="Murtosa", "oa"= "Oliveira de Azeméis", 
                                 "ob"= "Oliveira do Bairro", "ou"="Outros", "ov"= "Ovar", 
                                 "se"="Sever do Vouga", "sf"= "Santa Maria da Feira", "sm"= "São João da Madeira", 
                                 "va"= "Vagos", "vc"= "Vale de Cambra"),
         municipio_label = paste0(nome_municipio, "\n(n = ", total, ")"))

av_age_gender_mun_counts <- av_age_gender_mun_counts %>%
  left_join(municipio_totals %>% select(municipio, municipio_label), by = "municipio")

  # age_gender_mun_distribution plot
png("age_gender_mun_distribution.png", width = 1400, height = 1000)
ggplot(av_age_gender_mun_counts, aes(x = ageclass, y = n, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9), width = 0.9) +
  geom_text(aes(y = n, label = n, group = gender), 
            position = position_dodge2(width = 0.9, preserve = 'single'), hjust = -0.2, vjust = 0.3, size = 5) +
  labs(title = "Distribuição da população geral por género biológico por faixa etária no Distrito de Aveiro", x = "Faixa Etária", 
       y = "Nº de Indivíduos", fill = "Género Biológico")+
  scale_fill_manual(values = c("Feminino" = "hotpink1", "Masculino" = "deepskyblue"))+
  scale_x_discrete(expand = expansion(mult = c(0,0.3)))+
  scale_y_continuous(expand = expansion(mult = c(0,0.4))) +
  facet_wrap(~municipio_label, scales = 'free')+
  coord_flip()+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face = "bold", size = 30),
        strip.text = element_text(size = 20, face = "bold", colour = "black"),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (colour = "black", size = 16),
        axis.title.x = element_text(face ="bold", colour = "black", size = 20),
        axis.title.y = element_text(face ="bold", colour = "black", size = 20),
        axis.text.x = element_text(colour = "black", size = 18),
        axis.text.y = element_text(colour = "black", size = 18),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.spacing = unit(1.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 4.5 - Population analysis of the individuals by municipality ---------------------------------------------------------------------------------------------------------------------------------------------
  # count individuals by municipality, create percentage labels and define a set of colours
av_mun_counts <- DATA_AV %>%
  count(municipio)

av_mun_counts <- av_mun_counts %>% 
  mutate(total = sum(n),
         perc = n / total * 100,
         label = paste0(n, " (", sprintf("%.2f", perc), "%)"),
         municipio_factor = factor(municipio, levels = municipio[order(-n)]))

colors_municipio <- colorRampPalette(c("red3", "hotpink3","purple1", "deepskyblue4", "darkslategray3", "forestgreen", 
                                       "darkseagreen", "yellow", "chocolate2"))(16)
scales::show_col(colors_municipio)

  # mun_distribution plot
png("mun_distribution.png", width = 1400, height = 1000)
ggplot(av_mun_counts, aes(x = municipio_factor, y = n, fill = municipio_factor)) +
  geom_bar(width = 0.8, stat = "identity") +
  geom_text(aes(label = label), hjust = -0.1, vjust = 0.5, size = 7) +
  labs(title = "Distribuição da população geral no Distrito de Aveiro", x = "Município", y = "Nº de indivíduos", fill = "Município") +
  scale_fill_manual(values = colors_municipio)+
  scale_x_discrete(labels= c("ag" = "Águeda", "al"="Albergaria-a-Velha", "an"= "Anadia", 
                             "av" = "Aveiro", "et"= "Estarreja", "il"="Ílhavo", 
                             "me"="Mealhada", "mu"="Murtosa", "oa"= "Oliveira de Azeméis", 
                             "ob"= "Oliveira do Bairro", "ou"="Outros", "ov"= "Ovar", 
                             "se"="Sever do Vouga", "sf"= "Santa Maria da Feira", "sm"= "São João da Madeira", 
                             "va"= "Vagos", "vc"= "Vale de Cambra"), 
                   expand = expansion(mult = c(0,0.05)))+
  scale_y_continuous(limits = c(0, 7000), breaks= seq(0, 7000, by = 500), expand = expansion(mult = c(0, 0.15)))+
  coord_flip()+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5, face = "bold", size = 30),
        legend.position = "none", 
        axis.title.x=element_text(face ="bold", colour = "black", size = 22),
        axis.title.y=element_text(face ="bold", colour = "black", size = 22),
        axis.text.x=element_text(colour = "black", size = 18),
        axis.text.y=element_text(colour = "black", size = 18),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.75),
        axis.ticks.y = element_line(color = "black", linewidth = 0.75),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 4.6 - Population analysis of the individuals by zone and municipality of the district (3 zones) ---------------------------------------------------------------------------------------------------------------------------------------------
  # create zones and integrate the municipality
zona_norte <- c("sf", "sm", "ov", "oa", "vc")
zona_centro <- c("mu", "ag", "al", "av", "et", "il", "se")
zona_sul <- c("me", "an", "ob", "va")

  # count individuals by zone and municipality, create percentage labels and labels to zones
av_mun_zone_counts <- DATA_AV %>%
  mutate(zona = case_when(
    municipio %in% zona_norte ~ "Zona Norte",
    municipio %in% zona_centro ~ "Zona Centro",
    municipio %in% zona_sul ~ "Zona Sul",
    TRUE ~ NA_character_)) %>%
  group_by(zona, municipio) %>%
  summarise(n_individuos = n(), .groups = "drop") %>%
  group_by(zona) %>%
  mutate(total_zona = sum(n_individuos)) %>%
  ungroup() %>%
  mutate(total = sum(n_individuos),
         perc = n_individuos / total * 100,
         label = paste0(n_individuos , "\n (", sprintf("%.2f", perc), "%)"),
         zona_label = paste0(zona, "\n(n = ", total_zona, ")")) %>%
  mutate(zona_label = factor(zona_label, 
                             levels = paste0(c("Zona Norte", "Zona Centro", "Zona Sul"),"\n(n = ",
                                             tapply(n_individuos, zona, sum)[c("Zona Norte", "Zona Centro", "Zona Sul")],")")))

  # mun_zone_distribution plot
png("mun_zone_distribution.png", width = 1400, height = 1000)
ggplot(av_mun_zone_counts, aes(x = municipio, y = n_individuos, fill = zona)) +
  geom_bar(stat = "identity", width = 0.95) +
  geom_text(aes(label = label), vjust = -0.5, size = 6) +
  scale_fill_manual(values = c("Zona Norte" = "forestgreen", "Zona Centro" = "deepskyblue4", "Zona Sul" = "brown4")) +
  facet_wrap(~ zona_label, scales = "free") +                  
  scale_x_discrete(labels = c("ag" = "Águeda", "al"="Albergaria-a-Velha", "an"= "Anadia", "av" = "Aveiro", "et"= "Estarreja", 
                              "il"="Ílhavo", "me"="Mealhada", "mu"="Murtosa", "oa"= "Oliveira de Azeméis", "ob"= "Oliveira do Bairro",
                              "ou"="Outros", "ov"= "Ovar", "se"="Sever do Vouga", "sf"= "Santa Maria da Feira", "sm"= "São João da Madeira",
                              "va"= "Vagos", "vc"= "Vale de Cambra"),
                   expand = expansion(mult = c(0,0.1)))+
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  labs(title = "Distribuição da população geral no Distrito de Aveiro por Zona", x = "Município", y = "Número de indivíduos") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        strip.text = element_text(face = "bold", size = 22), 
        legend.position = "none", 
        axis.title.x = element_text(face ="bold", colour = "black", size = 22),
        axis.title.y = element_text(face ="bold", colour = "black", size = 22),
        axis.text.x = element_text(colour = "black", size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black", size = 20),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"),
        axis.ticks.x = element_line(color = "black", linewidth = 0.75),
        axis.ticks.y = element_line(color = "black", linewidth = 0.75),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 4.7 - Population analysis of the individuals by a1cclass (a1c_distribution)-------------------------------------------------------------------------------------
  # count individuals by a1cclass, create percentage labels and a1cclass order
av_a1c_counts <- DATA_AV %>% 
  count(a1cclass)

av_a1c_counts <- av_a1c_counts %>% 
  mutate(total = sum(n),
         perc = n / total * 100,
         label = paste0(n, " (", sprintf("%.2f", perc), "%)"))

av_a1c_counts$a1cclass <- factor(av_a1c_counts$a1cclass, levels = c("ND", "PD", "D"))

  # a1c_distribution plot
png("a1c_distribution.png", width = 1400, height = 1000)
ggplot(av_a1c_counts, aes(x = a1cclass, y = n, fill = a1cclass)) +
  geom_bar(width = 0.8, stat = "identity") +
  geom_text(aes(label = label), vjust = 0.5, hjust = -0.05, size = 7) +
  labs(title = "Distribuição da população geral por classe de Hemoglobina Glicada",
    x = "Classes de Hemoglobina Glicada", y = "Número de indivíduos", fill = "Classes") +
  scale_fill_manual(values = c("ND"= "mediumseagreen", "PD"= "gold3", "D" = "brown3"), 
                    labels = c("ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"))+
  scale_x_discrete(expand = expansion(mult = c(0, 0.25)),
                   labels = c("ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"))+
  scale_y_continuous(limits = c(0, 9000), breaks = seq(0, 9000, by=500), expand = expansion(mult = c(0, 0.13)))+
  coord_flip()+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (size = 18, colour = "black"),
        axis.title.x = element_text(face ="bold", colour = "black", size = 24),
        axis.title.y = element_text(face ="bold", colour = "black", size = 24),
        axis.text.x = element_text(colour = "black", size = 18),
        axis.text.y = element_blank(),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.75),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 4.8 - Population analysis of the individuals of a1cclass by gender (a1c_gender_distribution and a1cclass_gender_distribution_statistics)-----------------
  # count individuals of a1cclass by gender, create percentage labels and a1cclass order
av_a1c_gender_counts <- DATA_AV %>% 
  group_by(a1cclass,gender) %>% 
  dplyr::summarize(count = n(),.groups="drop")

av_a1c_gender_counts <- av_a1c_gender_counts %>%
  mutate(perc = count / av_totcount * 100,
         label = paste0(count, " (", sprintf("%.2f", perc), "%)"))

av_a1c_gender_counts$x_pos <- as.numeric(factor(av_a1c_gender_counts$a1cclass, levels = c("ND", "PD", "D")))
av_a1c_gender_counts$x_pos <- recode(av_a1c_gender_counts$a1cclass, "ND" = 0, "PD" = 0.6, "D"  = 1.2)

  # a1c_gender_counts plot
(ggplot(av_a1c_gender_counts, aes(x = x_pos, y = count, fill = gender)) +
  geom_bar(stat="identity", position = position_dodge(width=0.5), width=0.4) + 
  geom_text(aes(label = label), 
            position = position_dodge(width=0.5), 
            vjust=-0.4, size = 7) +
  labs(title="Distribuição do género biológico da população geral por classe de Hemoglobina Glicada", 
       x="Classe de Hemoglobina Glicada", 
       y="Nº de Indivíduos", 
       fill="Género Biológico")+
  scale_fill_manual(values = c("Masculino"= "deepskyblue", "Feminino"= "hotpink1"))+
  scale_x_continuous(breaks = c(0, 0.6, 1.20), labels = c("Não Diabético", "Pré-Diabético", "Diabético"), 
                     expand = expansion(mult = c(0, 0.05)))+
  scale_y_continuous(limits = c(0,5200),breaks= seq(0,5200,by=200), 
                     expand = expansion(mult = c(0, 0.05)))+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5, face = "bold", size = 30), 
        legend.position = "bottom", 
        legend.justification = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (size = 18, colour = "black"),
        axis.title.x=element_text(face ="bold", colour = "black", size = 24),
        axis.title.y=element_text(face ="bold", colour = "black", size = 24),
        axis.text.x=element_text(colour = "black", size = 20),
        axis.text.y=element_text(colour = "black", size = 20),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.75),
        axis.ticks.y = element_line(color = "black", linewidth = 0.75),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())) -> plot_a1cclass_gender
plot_a1cclass_gender

  # Apply Pearson's qui-square test
DATA_AV %>% 
  select(a1cclass, gender) %>% 
  group_by(a1cclass, gender) %>% 
  tally %>% 
  pivot_wider(names_from = a1cclass, values_from = n) %>% 
  ungroup -> table_a1cclass_gender

genero <- table_a1cclass_gender$gender
a1cclasses <- names(table_a1cclass_gender)[-1]
table_a1cclass_gender <- select(table_a1cclass_gender, -1)

table_a1cclass_gender <- as.matrix(table_a1cclass_gender)
dimnames(table_a1cclass_gender) <- list (gender = genero, a1cclass = a1cclasses)

chisq.test(table_a1cclass_gender) -> chisq.res     # standardized residuals from the test
chisq.test(table_a1cclass_gender)$expected         # expected values
CramerV(table_a1cclass_gender, conf.level = 0.95)  # check the effect (0.052) [0, 0.3] - weak; [0.3,0.7] - medium; > 0.7 - strong
chisq.res$p.value         # Save p-value (8.26*10^-15 < 0.05)
melt(chisq.res$stdres, varnames = c("gender", "a1cclass"), value.name = "value") -> table_stdres_a1cclass_gender

  # classification of the standardized residuals to better check the results
table_stdres_a1cclass_gender %>%
  mutate(sign = case_when(
    value > 4 & value <= 7 ~ '+',
    value < -4 & value >= -7 ~ '-',
    value > 7 ~ '++',
    value < -7 ~ '--',
    TRUE ~ NA)) -> table_stdres_a1cclass_gender

label_positions_4 <- av_a1c_gender_counts %>%
  group_by(gender) %>%
  arrange(gender, desc(a1cclass)) %>%
  mutate(
    rel = count / sum(count),
    cum_rel = cumsum(rel),
    rel_mid = cum_rel - rel / 2
  ) %>%
  ungroup() %>%
  left_join(x = ., y = table_stdres_a1cclass_gender, by = c('gender', 'a1cclass'))

  # a1cclass_gender_distribution_statistics plot (final plot)
png("a1cclass_gender_distribution_statistics.png", width = 1400, height = 1000)
plot_a1cclass_gender +
  geom_text(data = label_positions_4,
            aes(x = x_pos, y = count/2, label = sign, size = sign), position = position_dodge (width = 0.5), vjust = -0.5, size = 8)
dev.off()

# 4.9 - Population analysis of the individuals of a1cclass by ageclass (age_a1c_distribution and a1cclass_ageclass_distribution_statistics)----------------------------------
  # count the proportion of individuals of a1cclass by ageclass, create percentage labels, a1cclass order, ageclass order
av_age_a1c_counts <- DATA_AV %>% 
  count (ageclass, a1cclass) %>% 
  group_by(ageclass) %>% 
  mutate(rel=100*n/sum(n), 
         total = sum(n)) %>% 
  ungroup

av_age_a1c_counts$a1cclass <- factor(av_age_a1c_counts$a1cclass, levels = c("ND", "PD", "D"))
av_age_a1c_counts$ageclass <- factor(av_age_a1c_counts$ageclass, levels = c("[0,20[", "[20,40[", "[40,60[", "[60,80[", "[80,105]"))

for_the_labels_1 <- av_age_a1c_counts %>% 
  select(ageclass,total) %>% 
  distinct()

  # age_a1c_counts plot
(ggplot() +
  geom_bar(data = av_age_a1c_counts, aes(x = ageclass, y = rel, fill = a1cclass), 
           stat = "identity", position = "stack", color = "black", width = 0.4) +
  geom_text(data = for_the_labels_1, aes(x = ageclass, y = 103, label = total), size = 7)+ 
  labs(title = "Percentagem de indivíduos por faixa etária segundo as Classes de Hemoglobina Glicada", 
       x = "Faixa etária", y = "Percentagem de indivíduos", fill = "Classe de Hemoglobina Glicada") +
  scale_fill_manual(values = c("ND"= "mediumseagreen", "PD"= "gold3", "D" = "brown3"), 
                    labels = c("ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"))+
  scale_x_discrete(expand = expansion(mult = c(0,0.05)))+
  scale_y_continuous(breaks = seq(0, 100, by = 5),
                     labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  coord_flip()+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5, face = "bold", size = 30),
        legend.position = "bottom", legend.direction = "horizontal", legend.justification = "center", legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (size = 18),
        axis.title.x=element_text(face ="bold", colour = "black", size = 26),
        axis.title.y=element_text(face ="bold", colour = "black", size = 26),
        axis.text.x=element_text(colour = "black", size = 20),
        axis.text.y=element_text(colour = "black", size = 20),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.75),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())-> plot_a1cclass_ageclass)
  plot_a1cclass_ageclass

  # Apply Pearson's qui-square test
DATA_AV %>% 
  select(ageclass, a1cclass) %>% 
  group_by(ageclass, a1cclass) %>% 
  tally %>% 
  pivot_wider(names_from = ageclass, values_from = n) %>% 
  ungroup -> table_a1cclass_ageclass

a1cclasses <- table_a1cclass_ageclass$a1cclass
ageclasses <- names(table_a1cclass_ageclass)[-1]
table_a1cclass_ageclass <- select(table_a1cclass_ageclass, -1)

table_a1cclass_ageclass <- as.matrix(table_a1cclass_ageclass)
dimnames(table_a1cclass_ageclass) <- list (a1cclass = a1cclasses, ageclass = ageclasses)

chisq.test(table_a1cclass_ageclass) -> chisq.res     # standardized residuals from the test
chisq.test(table_a1cclass_ageclass)$expected         # expected values
CramerV(table_a1cclass_ageclass, conf.level = 0.95)  # check the effect (0.23) [0, 0.3] - weak; [0.3,0.7] - medium; > 0.7 - strong 
chisq.res$p.value         # save p-value (0 < 0.05)
melt(chisq.res$stdres) -> table_stdres_a1cclass_ageclass

  # classification of the standardized residuals to better check the results
table_stdres_a1cclass_ageclass %>% 
  mutate(sign = case_when(
    value > 20 ~ '+++',
    value > 10 & value <= 20 ~ '++',
    value > 3 & value <= 10 ~ '+',
    value < -20 ~ '---',
    value < -10 & value >= -20 ~ '--',
    value < -3 & value >= -10 ~ '-',
    TRUE ~ NA)) -> table_stdres_a1cclass_ageclass

label_positions_3 <- av_age_a1c_counts %>%
  group_by(ageclass) %>%
  arrange(ageclass, desc(a1cclass)) %>%  
  mutate(
    cum_rel = cumsum(rel),
    rel_mid = cum_rel - rel / 2
  ) %>%
  ungroup() %>% 
  left_join(x = ., y = table_stdres_a1cclass_ageclass, by = c('a1cclass', 'ageclass'))

  # a1cclass_ageclass_distribution_statistics plot (final plot)
png("a1cclass_ageclass_distribution_statistics.png", width = 1400, height = 1000)
plot_a1cclass_ageclass + 
  geom_text(data = label_positions_3, 
            aes(x = ageclass, y = rel_mid, label = sign, size = sign))+
  scale_size_manual(values = c(
    "+" = 8,
    "++" = 8,
    "+++" = 8,
    "-" = 8,
    "--" = 8,
    "---" = 8), guide = "none")
dev.off()

# 4.10 - Population analysis of the individuals of a1cclass by municipality (a1c_municipaly_distribution and a1cclass_municipaly_distribution_statistics)----------------------------------------------
  # count the proportion of individuals of a1cclass by municipality, create percentage labels, a1cclass order 
av_municipaly_a1c_counts <- DATA_AV %>% 
  count (municipio, a1cclass) %>% 
  group_by(municipio) %>% 
  mutate(rel=100*n/sum(n), 
         total = sum(n)) %>% 
  ungroup

av_municipaly_a1c_counts$a1cclass <- factor(av_municipaly_a1c_counts$a1cclass, levels = c("ND", "PD", "D"))

for_the_labels_2 <- av_municipaly_a1c_counts %>% 
  select(municipio,total) %>% 
  distinct()

ordem <- av_municipaly_a1c_counts %>% 
  group_by(municipio) %>% 
  arrange(total) %>% 
  pull(municipio) %>% unique 

  # a1c_municipaly_distribution plot
(ggplot() +
  geom_bar(data = av_municipaly_a1c_counts, 
           aes(x = factor(municipio, levels = rev(ordem)), y = rel, fill = a1cclass), 
           stat = "identity", position = "stack", color = "black", width = 0.7) +
  geom_text(data = for_the_labels_2, 
            aes(x = municipio, y = 103, label = total),
            position = position_dodge2(width = 1,preserve ='single'), vjust = 0.5, size = 7) + 
  labs(title = "Distribuição da população geral por Classe de Hemoglobina Glicada no Distrito de Aveiro", 
       x = "Município", y = "Percentagem de indivíduos", fill = "Classe de Hemoglobina Glicada") +
  scale_fill_manual(values = c("ND"= "mediumseagreen", "PD"= "gold3", "D" = "brown3"), 
                    labels = c("ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"))+
  scale_x_discrete(labels= c("ag" = "Águeda", "al"="Albergaria-a-Velha", "an"= "Anadia", 
                             "av" = "Aveiro", "et"= "Estarreja", "il"="Ílhavo", 
                             "me"="Mealhada", "mu"="Murtosa", "oa"= "Oliveira de Azeméis", 
                             "ob"= "Oliveira do Bairro", "ou"="Outros", "ov"= "Ovar", 
                             "se"="Sever do Vouga", "sf"= "Santa Maria da Feira", "sm"= "São João da Madeira", 
                             "va"= "Vagos", "vc"= "Vale de Cambra"),
                   expand = expansion(mult = c(0,0.05)))+
  scale_y_continuous(breaks = seq(0, 100, by = 5),
                     labels = scales::percent_format(scale = 1),
                     expand=expansion(mult = c(0, 0.05))) +
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  coord_flip()+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 26),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (size = 18, colour = "black"),
        axis.title.x=element_text(face ="bold", colour = "black", size = 22),
        axis.title.y=element_text(face ="bold", colour = "black", size = 22),
        axis.text.x=element_text(colour = "black", size = 18),
        axis.text.y=element_text(colour = "black", size = 18),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"),
        axis.ticks.x = element_line(color = "black", linewidth = 0.75),
        axis.ticks.y = element_line(color = "black", linewidth = 0.75),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())-> plot_a1cclass_municipio)
plot_a1cclass_municipio

  # Apply Pearson's qui-sqaure test
DATA_AV %>% 
  select(municipio, a1cclass) %>% 
  group_by(municipio, a1cclass) %>% 
  tally %>% 
  pivot_wider(names_from = municipio, values_from = n) %>% 
  select(-sm) %>%     # 'sm' has "NA's" so it was removed
  ungroup -> table_a1cclass_municipio

a1cclasses <- table_a1cclass_municipio$a1cclass
municipios <- names(table_a1cclass_municipio)[-1]
table_a1cclass_municipio <- select(table_a1cclass_municipio, -1)

table_a1cclass_municipio <- as.matrix(table_a1cclass_municipio)
dimnames(table_a1cclass_municipio) <- list (a1cclass = a1cclasses, municipio = municipios)

chisq.test(table_a1cclass_municipio) -> chisq.res     # standardized residuals from the test
chisq.test(table_a1cclass_municipio)$expected         # expected values
CramerV(table_a1cclass_municipio, conf.level = 0.95)  # effect (0.089) [0, 0.3] - weak; [0.3,0.7] - medium; > 0.7 - strong
chisq.res$p.value         # save p-value (9.31*10^-62 < 0.05)
melt(chisq.res$stdres) -> table_stdres_a1cclass_municipio

  # classification of the standardized residuals to better check the results
table_stdres_a1cclass_municipio %>% 
  mutate(sign = case_when(value > 7 ~ '+++',
                          value > 3 & value <= 7 ~ '++',
                          value > 1 & value <= 3 ~ '+',
                          value < -7 ~ '---',
                          value < -3 & value >= -7 ~ '--',
                          value < -1 & value >= -3 ~ '-',
                          TRUE ~ NA)) -> table_stdres_a1cclass_municipio

label_positions_1 <- av_municipaly_a1c_counts %>%
  group_by(municipio) %>%
  arrange(municipio, desc(a1cclass)) %>%
  mutate(
    cum_rel = cumsum(rel),
    rel_mid = cum_rel - rel / 2
  ) %>%
  ungroup() %>% 
  left_join(x = ., y = table_stdres_a1cclass_municipio, by = c('a1cclass', 'municipio'))

  # a1cclass_municipaly_distribution_statistics plot
png("a1cclass_municipaly_distribution_statistics.png", width = 1400, height = 1000)
plot_a1cclass_municipio + 
  geom_text(data = label_positions_1, 
            aes(x = municipio, y = rel_mid, label = sign), size = 8)
dev.off()

# 4.11 - Population analysis of the individuals by glicclass (glic_distribution)--------------------------------------------------
  # count individuals by glicclass, create percentage labels and glicclass order
av_glic_counts <- DATA_AV %>% 
  count(glicclass)

av_glic_counts <- av_glic_counts %>% 
  mutate(total = sum(n),
         perc = n / total * 100,
         label = paste0(n, " (", sprintf("%.2f", perc), "%)"))

av_glic_counts$glicclass <- factor(av_glic_counts$glicclass, levels = c("H","ND", "PD", "D"))

  # glic_distribution plot
png("glic_distribution.png", width = 1400, height = 1000)
ggplot(av_glic_counts, aes(x = glicclass, y = n, fill = glicclass)) +
  geom_bar(width = 0.8, stat = "identity") +
  geom_text(aes(label = label), vjust = 0.5, hjust = -0.1, size = 7) +
  labs(title = "Distribuição da população geral por classe de Glicose em Jejum",
    x = "Classes de Glicose em Jejum", y = "Número de indivíduos", fill = "Classes de Glicose em Jejum") +
  scale_fill_manual(values = c("H" = "royalblue", "ND"= "mediumseagreen", "PD"= "gold3", "D" = "brown3"), 
                    labels = c("H" = "Hipoglicémico","ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"))+
  scale_x_discrete(expand = expansion(mult = c(0, 0.15)),
                   labels = c("H"= "Hipoglicémico","ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"))+
  scale_y_continuous(limits = c(0, 11000), breaks= seq(0, 11000,by = 500), expand = expansion(mult = c(0, 0.15)))+
  coord_flip()+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face = "bold", size = 26),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (size = 18, colour = "black"),
        axis.title.x=element_text(face ="bold", colour = "black", size = 22),
        axis.title.y=element_text(face ="bold", colour = "black", size = 22),
        axis.text.x=element_text(colour = "black", size = 18),
        axis.text.y=element_blank(),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.75),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 4.12 - Population analysis of the individuals of glicclass by gender (glic_gender_distribution and glicclass_gender_distribution_statistics)-----------------
  # count individuals of glicclass by gender, create percentage labels and glicclass order
av_glic_gender_counts <- DATA_AV %>% 
  group_by(glicclass,gender) %>% 
  summarize(count = n(), .groups="drop")

av_glic_gender_counts <- av_glic_gender_counts %>%
  mutate(perc = count / av_totcount * 100,
         label = paste0(count, " (", sprintf("%.2f", perc), "%)"))

av_glic_gender_counts$x_pos <- as.numeric(factor(av_glic_gender_counts$glicclass, levels = c("H","ND", "PD", "D")))
av_glic_gender_counts$x_pos <- recode(av_glic_gender_counts$glicclass,"H" = 0, "ND" = 0.6, "PD" = 1.2, "D"  = 1.8)

  # glic_gender_distribution plot
(ggplot(av_glic_gender_counts, aes(x = x_pos, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.4) + 
  geom_text(aes(label = label), 
            position = position_dodge(width = 0.5), 
            vjust = -0.4, 
            size = 7) +
  labs(title="Distribuição do género biológico da população geral por classe de Glicose em Jejum", 
    x="Classe de Glicose em Jejum", 
    y="Nº de Indivíduos", 
    fill="Género Biológico")+
  scale_fill_manual(values = c("Masculino"= "deepskyblue", "Feminino"= "hotpink1"))+
  scale_x_continuous(breaks = c(0, 0.6, 1.2, 1.8), labels = c("Hipoglicémico", "Não Diabético", "Pré-Diabético", "Diabético"), 
                     expand = expansion(mult = c(0, 0.05)))+
  scale_y_continuous(limits = c(0,6500), 
                     breaks= seq(0,6500,by=500), 
                     expand = expansion(mult = c(0, 0.05)))+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30), 
        legend.position = "bottom", legend.direction = "horizontal", legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (size = 18, colour = "black"),
        axis.title.x=element_text(face ="bold", colour = "black", size = 24),
        axis.title.y=element_text(face ="bold", colour = "black", size = 24),
        axis.text.x=element_text(colour = "black", size = 20),
        axis.text.y=element_text(colour = "black", size = 20),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"),
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())) -> plot_glicclass_gender
plot_glicclass_gender

  # Apply Pearson's qui-square test
av_glic_gender_counts$glicclass <- factor(av_glic_gender_counts$glicclass, levels = c("H","ND", "PD", "D"))

DATA_AV %>% 
  select(glicclass, gender) %>% 
  group_by(glicclass, gender) %>% 
  tally %>% 
  pivot_wider(names_from = glicclass, values_from = n) %>% 
  ungroup -> table_glicclass_gender

genero <- table_glicclass_gender$gender
glicclasses <- names(table_glicclass_gender)[-1]
table_glicclass_gender <- select(table_glicclass_gender, -1)

table_glicclass_gender <- as.matrix(table_glicclass_gender)
dimnames(table_glicclass_gender) <- list (gender = genero, glicclass = glicclasses)

chisq.test(table_glicclass_gender) -> chisq.res     # standardized residuals from the test
chisq.test(table_glicclass_gender)$expected         # expected values
CramerV(table_glicclass_gender, conf.level = 0.95)  # effect (0.14) [0, 0.3] - weak; [0.3,0.7] - medium; > 0.7 - strong
chisq.res$p.value         # save p-value (9.53*10^-107 < 0.05)
melt(chisq.res$stdres, varnames = c("gender", "glicclass"), value.name = "value") -> table_stdres_glicclass_gender

  # classification of the standardized residuals to better check the results
table_stdres_glicclass_gender %>% 
  mutate(sign = case_when(
    value > 4 & value <= 10 ~ '+',
    value < -4 & value >= -10 ~ '-',
    value > 10 & value <= 20 ~ '++',
    value < -10 & value >= -20 ~ '--',
    value > 20 ~ '+++',
    value < -20 ~ '---',
    TRUE ~ NA)) -> table_stdres_glicclass_gender

label_positions_4 <- av_glic_gender_counts %>%
  group_by(gender) %>%
  arrange(gender, desc(glicclass)) %>% 
  mutate(
    rel = count / sum(count),
    cum_rel = cumsum(rel),
    rel_mid = cum_rel - rel / 2
  ) %>%
  ungroup() %>% 
  left_join(x = ., y = table_stdres_glicclass_gender, by = c('gender', 'glicclass'))

  # glicclass_gender_distribution_statistics plot (final plot) 
png("glicclass_gender_distribution_statistics.png", width = 1400, height = 1000)
plot_glicclass_gender + 
  geom_text(data = label_positions_4, 
            aes(x = x_pos, y = count/2, label = sign, size = sign), position = position_dodge (width = 0.5), vjust = -0.5, size = 8)
dev.off()

# 4.13 - Population analysis of the individuals of glicclass by ageclass (age_glic_distribution and glicclass_ageclass_distribution_statistics)----------------------------------
  # count the proportion of individuals of glicclass by ageclass, create percentage labels, glicclass order, ageclass order
av_age_glic_counts <- DATA_AV %>% 
  count (ageclass, glicclass) %>% 
  group_by(ageclass) %>% 
  mutate(rel=100*n/sum(n), 
         total = sum(n)) %>% 
  ungroup 

av_age_glic_counts$glicclass <- factor(av_age_glic_counts$glicclass, levels = c("H","ND", "PD", "D"))
av_age_glic_counts$ageclass <- factor(av_age_glic_counts$ageclass, levels = c("[0,20[", "[20,40[", "[40,60[", "[60,80[", "[80,105]"))

for_the_labels_3 <- av_age_glic_counts %>% 
  select(ageclass,total) %>% 
  distinct()

  # age_glic_distribution plot
(ggplot() +
  geom_bar(data = av_age_glic_counts, aes(x = ageclass, y = rel, fill = glicclass), 
           stat = "identity", position = "stack", color = "black", width = 0.4) +
  geom_text(data = for_the_labels_3, aes(x = ageclass, y = 103, label = total), size = 7)+ 
  labs(title = "Percentagem de indivíduos por faixa etária segundo as Classes de Glicose em Jejum", 
       x = "Faixa etária", y = "Percentagem de indivíduos", fill = "Classe de Glicose em Jejum") +
  scale_fill_manual(values = c("H" = "royalblue", "ND"= "mediumseagreen", "PD"= "gold3", "D" = "brown3"), 
                    labels = c("H" = "Hipoglicémico", "ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"))+
  scale_x_discrete(expand = expansion(mult = c(0, 0.05)))+
  scale_y_continuous(breaks = seq(0, 100, by = 5),
                     labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  coord_flip()+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (size = 18),
        axis.title.x=element_text(face ="bold", colour = "black", size = 24),
        axis.title.y=element_text(face ="bold", colour = "black", size = 24),
        axis.text.x=element_text(colour = "black", size = 20),
        axis.text.y=element_text(colour = "black", size = 20),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> plot_glicclass_ageclass)
plot_glicclass_ageclass

  # Apply Pearson's qui-square test
DATA_AV_filtered <- DATA_AV %>%
  filter(ageclass != "[100, 120[") %>%
  droplevels()

table_glicclass_ageclass <- DATA_AV_filtered %>% 
  select(ageclass, glicclass) %>% 
  group_by(ageclass, glicclass) %>% 
  tally %>% 
  pivot_wider(names_from = ageclass, values_from = n, values_fill = 0) %>% 
  ungroup

glicclasses <- table_glicclass_ageclass$glicclass
ageclasses <- names(table_glicclass_ageclass)[-1]
table_glicclass_ageclass <- select(table_glicclass_ageclass, -1)

table_glicclass_ageclass <- as.matrix(table_glicclass_ageclass)
dimnames(table_glicclass_ageclass) <- list (glicclass = glicclasses, ageclass = ageclasses)

chisq.test(table_glicclass_ageclass) -> chisq.res     # standardized residuals from the test
chisq.test(table_glicclass_ageclass)$expected         # expected values
CramerV(table_glicclass_ageclass, conf.level = 0.95)  # effect (0.16) [0, 0.3] - weak; [0.3,0.7] - medium; > 0.7 - strong
chisq.res$p.value         # save p-value (0 < 0.05)
melt(chisq.res$stdres) -> table_stdres_glicclass_ageclass

  # classification of the standardized residuals to better check the results
table_stdres_glicclass_ageclass %>% 
  mutate(sign = case_when(
    value > 20 ~ '+++',
    value >= 10 & value <= 20 ~ '++',
    value > 2 & value <= 10 ~ '+',
    value <= -20 ~ '---',
    value <= -10 & value >= -20 ~ '--',
    value < -2 & value >= -10 ~ '-',
    TRUE ~ NA)) -> table_stdres_glicclass_ageclass

label_positions_4 <- av_age_glic_counts %>%
  group_by(ageclass) %>%
  arrange(ageclass, desc(glicclass)) %>%  
  mutate(
    cum_rel = cumsum(rel),
    rel_mid = cum_rel - rel / 2
  ) %>%
  ungroup() %>% 
  left_join(x = ., y = table_stdres_glicclass_ageclass, by = c('glicclass', 'ageclass'))

  # glicclass_ageclass_distribution_statistics plot (final plot)
png("glicclass_ageclass_distribution_statistics.png", width = 1400, height = 1000)
plot_glicclass_ageclass + 
  geom_text(data = label_positions_4, 
            aes(x = ageclass, y = rel_mid, label = sign, size = sign))+
  scale_size_manual(values = c(
    "+" = 8,
    "++" = 8,
    "+++" = 8,
    "-" = 8,
    "--" = 8,
    "---" = 8), guide = "none")
dev.off()

# 4.12 - Population analysis of the individuals of glicclass by municipality (glic_municipaly_distribution and glicclass_municipaly_distribution_statistics)----------------------------------------------
  # count the proportion of individuals of glicclass by municipality, create percentage labels, glicclass order 
av_municipaly_glic_counts <- DATA_AV %>% 
  count (municipio, glicclass) %>% 
  group_by(municipio) %>% 
  mutate(rel=100*n/sum(n), 
         total = sum(n)) %>% 
  ungroup

ordem <- av_municipaly_glic_counts %>% 
  group_by(municipio) %>% 
  arrange(total) %>% 
  pull(municipio) %>% unique 

av_municipaly_glic_counts$glicclass <- factor(av_municipaly_glic_counts$glicclass, levels = c("H","ND", "PD", "D"))

for_the_labels_4 <- av_municipaly_glic_counts %>% 
  select(municipio,total) %>% 
  distinct()

  # glic_municipaly_distribution plot
(ggplot() +
  geom_bar(data = av_municipaly_glic_counts, 
           aes(x = factor(municipio, levels = rev(ordem)), y = rel, fill = glicclass), 
           stat = "identity", position = "stack", color = "black", width = 0.7) +
  geom_text(data = for_the_labels_4, 
            aes(x = municipio, y = 103, label = total),
            position = position_dodge2(width = 1, preserve = 'single'), vjust = 0.5, size = 7) + 
  labs(title = "Distribuição da população geral por Classes de Glicose em Jejum no Distrito de Aveiro", 
       x = "Município", y = "Percentagem de indivíduos", fill = "Classe de Glicose em Jejum") +
  scale_fill_manual(values = c("H" = "royalblue", "ND"= "mediumseagreen", "PD"= "gold3", "D" = "brown3"), 
                    labels = c("H" = "Hipoglicémico","ND" = "Não Diabético", "PD" = "Pré-Diabético", "D" = "Diabético"))+
  scale_x_discrete(labels= c("ag" = "Águeda", "al"="Albergaria-a-Velha", "an"= "Anadia", 
                             "av" = "Aveiro", "et"= "Estarreja", "il"="Ílhavo", 
                             "me"="Mealhada", "mu"="Murtosa", "oa"= "Oliveira de Azeméis", 
                             "ob"= "Oliveira do Bairro", "ou"="Outros", "ov"= "Ovar", 
                             "se"="Sever do Vouga", "sf"= "Santa Maria da Feira", "sm"= "São João da Madeira", 
                             "va"= "Vagos", "vc"= "Vale de Cambra"), 
                   expand = expansion(mult = c(0,0.05)))+
  scale_y_continuous(breaks = seq(0, 100, by = 5),
                     labels = scales::percent_format(scale = 1),
                     expand=expansion(mult = c(0, 0.05))) +
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  coord_flip()+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 26),
        legend.position = "bottom", legend.direction = "horizontal", 
        legend.justification = "center", 
        legend.box.just = "center", 
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (size = 18, colour = "black"),
        axis.title.x=element_text(face ="bold", colour = "black", size = 22),
        axis.title.y=element_text(face ="bold", colour = "black", size = 22),
        axis.text.x=element_text(colour = "black", size = 18),
        axis.text.y=element_text(colour = "black", size = 18),
        axis.line.x.bottom = element_line(color="grey61"),
        axis.line.y.left = element_line(color="grey61"), 
        axis.ticks.x = element_line(color = "black", linewidth = 0.75),
        axis.ticks.y = element_line(color = "black", linewidth = 0.75),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())-> plot_glicclass_municipio)
plot_glicclass_municipio

  # Apply Pearson's qui-square test 
DATA_AV %>% 
  select(municipio, glicclass) %>% 
  group_by(municipio, glicclass) %>% 
  tally %>% 
  pivot_wider(names_from = municipio, values_from = n) %>% 
  select(-sm) %>%         # 'sm' has "NA's" so it was removed
  ungroup -> table_glicclass_municipio

glicclasses <- table_glicclass_municipio$glicclass
municipios <- names(table_glicclass_municipio)[-1]
table_glicclass_municipio <- select(table_glicclass_municipio, -1)

table_glicclass_municipio <- as.matrix(table_glicclass_municipio)
dimnames(table_glicclass_municipio) <- list (glicclass = glicclasses, municipio = municipios)

chisq.test(table_glicclass_municipio) -> chisq.res     # standardized residuals
chisq.test(table_glicclass_municipio)$expected         # expected values
CramerV(table_glicclass_municipio, conf.level = 0.95)  # effect (0.074) [0, 0.3] - weak; [0.3,0.7] - medium; > 0.7 - strong
chisq.res$p.value         # save p-value (6.72*10^-58 < 0.05)
melt(chisq.res$stdres) -> table_stdres_glicclass_municipio

  # classification of the standardized residuals to better check the results
table_stdres_glicclass_municipio %>% 
  mutate(sign = case_when(value > 7 ~ '+++',
                          value > 3 & value <= 7 ~ '++',
                          value > 1 & value <= 3 ~ '+',
                          value < -7 ~ '---',
                          value < -3 & value >= -7 ~ '--',
                          value < -1 & value >= -3 ~ '-',
                          TRUE ~ NA)) -> table_stdres_glicclass_municipio

label_positions_2 <- av_municipaly_glic_counts %>%
  group_by(municipio) %>%
  arrange(municipio, desc(glicclass)) %>%  # Ensure correct stacking order
  mutate(
    cum_rel = cumsum(rel),
    rel_mid = cum_rel - rel / 2
  ) %>%
  ungroup() %>% 
  left_join(x = ., y = table_stdres_glicclass_municipio, by = c('glicclass', 'municipio'))

  # glicclass_municipaly_distribution_statistics plot (final plot)
png("glicclass_municipaly_distribution_statistics.png", width = 1400, height = 1000)
plot_glicclass_municipio + 
  geom_text(data = label_positions_2, 
            aes(x = municipio, y = rel_mid, label = sign), size = 8)
dev.off()

# 5 - OTHERS
# 5.1 - Correlation between the glycemic biomarkers and the chronological age
ggplot(DATA_AV, aes(x = age)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Histograma da Idade") # not normal

ggplot(DATA_AV, aes(x = a1c)) + 
  geom_histogram(bins = 30, fill = "firebrick", color = "black") +
  labs(title = "Histograma da HbA1c") # not normal

ggplot(DATA_AV, aes(x = glic)) + 
  geom_histogram(bins = 30, fill = "darkgreen", color = "black") +
  labs(title = "Histograma da Glicose") # not normal

  # create the matrix
axis <- c("age", "a1c", "glic")
a1c_glic_age_cor_data <- DATA_AV [, axis]
a1c_glic_age_cor_data <- na.omit(a1c_glic_age_cor_data)

  # Apply correlation test (spearman)
a1c_glic_age_cor_matrix <- cor(a1c_glic_age_cor_data, use = "complete.obs", method = "spearman")
print(a1c_glic_age_cor_matrix)

  # check the results
cor.test(a1c_glic_age_cor_data$age, a1c_glic_age_cor_data$a1c, method = "spearman") # rho = 0.28 (positive correlation)
cor.test(a1c_glic_age_cor_data$age, a1c_glic_age_cor_data$glic, method = "spearman") # valor de rho = 0.19 (positive correlation)
cor.test(a1c_glic_age_cor_data$a1c, a1c_glic_age_cor_data$glic, method = "spearman") # valor de rho = 0.69 (positive correlation)

a1c_glic_age_cor_long <- melt(a1c_glic_age_cor_matrix, varnames = c("axis1", "axis2"), value.name = "Correlacao")

  # create heatmap of correlation 
png("a1c_glic_age_distribution_statistics.png", width = 1400, height = 1000)
ggplot(a1c_glic_age_cor_long, aes(x = axis1, y = axis2, fill = Correlacao)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "royalblue4", mid = "white", high = "darkred", midpoint = 0, limits = c(-1, 1)) +
  geom_text(aes(label = round(Correlacao, 2)), size = 7, color = "black") +
  labs(title = "Relação entre os Biomarcadores em estudo e a Idade Cronológica da população geral", x = "", y = "", 
       fill = "Valor de ρ")+
  scale_x_discrete(labels = c(age = "Idade Cronológica", a1c = "HbA1c (%)", glic = "Glicose (mg/dL)")) +
  scale_y_discrete(labels = c(age = "Idade Cronológica", a1c = "HbA1c (%)", glic = "Glicose (mg/dL)")) +
  theme_minimal() +
  theme(plot.title=element_text(hjust = 0.5, face = "bold", size = 30),
        legend.position = "right", 
        legend.direction = "vertical",
        legend.justification = "center",
        legend.box.just = "center",
        legend.title = element_text (hjust = 0.5, colour = "black", face = "bold", size = 20),
        legend.text = element_text (hjust = 0.5, colour = "black", size = 16),
        axis.title.x=element_text(face ="bold", colour = "black", size = 24),
        axis.title.y=element_text(face ="bold", colour = "black", size = 24),
        axis.text.x=element_text(colour = "black", face = "bold", size = 20),
        axis.text.y=element_text(colour = "black", face = "bold", size = 20),
        axis.ticks.length = unit(5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

# 5.2 - correlation between hba1c and glic values (only) ------------------------------------------------------------------------------------------------
ggplot (DATA_AV, aes(x = a1c, y = glic))+
  geom_point()

cor(x = DATA_AV$a1c, y = DATA_AV$glic, method = "spearman")
cor.test(x = DATA_AV$a1c, y = DATA_AV$glic, method = "spearman") # p-value (its not possible estimate because we have lots of repetitive values) and correlation value rho --> 0.69 (positive correlation)
length(DATA_AV$a1c) # total number of entrys
length(unique(DATA_AV$a1c)) # check the impact (only 128 entrys are unique)
