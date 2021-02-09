#Importando as libraries necessárias
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)

#Importando CSV pre-processado
total_alunos <- read.csv("datasets/preprocessed_data/no_filtered_analysis.csv", encoding = "UTF-8")

################# Functions

# Converte x para porcentagem considerando o total de alunos
calc_percent <- function(x) {
  return(round((x / nrow(total_alunos) * 100), 2))
}

################# Relação geral de evasão: Araras X Outras Cidades

# Relação de alunos evadidos de Araras e outras cidades
alunos_evadidos_araras <- total_alunos %>%
  count(EVADIDO, RESID_ARARAS) %>%
  group_by(RESID_ARARAS) %>%
  summarize(across(.fns = sum)) %>%
  select(c(RESID_ARARAS, n))

alunos_evadidos_araras$n <-lapply(alunos_evadidos_araras$n, calc_percent)

alunos_evadidos_araras$RESID_ARARAS[alunos_evadidos_araras$RESID_ARARAS == 1] <- "SIM"
alunos_evadidos_araras$RESID_ARARAS[alunos_evadidos_araras$RESID_ARARAS == 0] <- "NÃO"

ggplot(alunos_evadidos_araras, aes(x = "" , y = n, fill=RESID_ARARAS)) +
  geom_bar(stat="identity", width = 1, color = "black") +
  coord_polar("y", start=0) +
  labs(title = "Perfil do Aluno (%)", subtitle = "Quantos residem em Araras?",
       caption = "Data source: FHO Uniararas - Sistemas de Informação (2014 à 2019)") +
  geom_text(aes(label=n), vjust=-0.6, color="black",
       position = position_stack(vjust = 0.4), size=5) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0), 
        axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual("Situação do Aluno", labels = c("Não reside", "Reside"), values = c("#FF6347", "#4169E1"))

################# Relação de Evadidos e Não Evadidos

# Relação de Evadidos e Não Evadidos
relac_eva_neva <- total_alunos %>%
  count(EVADIDO)

relac_eva_neva$n <- lapply(relac_eva_neva$n, calc_percent)

relac_eva_neva$EVADIDO[relac_eva_neva$EVADIDO == 1] <- "EVADIDO"
relac_eva_neva$EVADIDO[relac_eva_neva$EVADIDO == 0] <- "NAO EVADIDO"

ggplot(relac_eva_neva, aes(x = "" , y = n, fill=EVADIDO)) +
  geom_bar(stat="identity", width = 1, color = "black")+
  coord_polar("y", start=0) +
  labs(title = "Perfil do Aluno (%)", subtitle = "Relação de Evadidos e Não Evadidos",
       caption = "Data source: FHO Uniararas - Sistemas de Informação (2014 à 2019)") +
  geom_text(aes(label=n), vjust=-0.6, color="black",
            position = position_stack(vjust = 0.4), size=5) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0), 
        axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual("Situação do Aluno", labels = c("Evadido", "Não evadido"), values = c("#FF6347", "#4169E1"))

################## Quantidade de alunos por grade (2014 até 2019)

# Alunos por grade
alunos_por_grade <- total_alunos

alunos_por_grade <- total_alunos %>%
  count(GRADE_CORRENTE, EVADIDO) %>%
  group_by(GRADE_CORRENTE) %>%
  summarize(across(.fns = sum)) %>%
  select(c(GRADE_CORRENTE, n))

ggplot(alunos_por_grade, aes(x = GRADE_CORRENTE , y = n, fill = as.factor(GRADE_CORRENTE))) +
  geom_bar(stat="identity", width = 1, color = "black")+
  xlab("Anos") + ylab("Nº de alunos evadidos") +
  labs(title = "Perfil do Aluno", subtitle = "Quantidade de alunos por grade (2014 até 2019)",
       caption = "Data source: FHO Uniararas - Sistemas de Informação (2014 à 2019)") +
  geom_text(aes(label=n), vjust=-0.6, color="black",
            position = position_stack(vjust = 0.4), size=5) +
  theme_update() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0)) +
  scale_fill_manual("Anos", labels = c("2014", "2015", "2016", "2017", "2018", "2019"), 
                    values = c("#FF7F50", "#FFA500", "#CD5C5C", "#BA55D3", "#7B68EE", "#DC143C"))


################# Bolsistas vs não bolsistas

bolsista <- calc_percent(as.numeric(nrow(filter(total_alunos, BOLSISTA == 1))))

n_bolsista <- calc_percent(as.numeric(nrow(filter(total_alunos, BOLSISTA == 0))))

plot2 <- data.frame(situacao = c("Bolsista", "Não bolsista"), num = c(bolsista, n_bolsista))

ggplot(plot2, aes(x="", y=num, fill=situacao))+
  geom_bar(width = 1, stat = "identity", color="black")+
  coord_polar("y", start=0)+
  labs(title = "Perfil do Aluno (%)", subtitle = "Quantos possuem bolsa de estudos?",
       caption = "Data source: FHO Uniararas - Sistemas de Informação (2014 à 2019)") +
  geom_text(aes(label=num), vjust=-0.6, color="black",
            position = position_stack(vjust = 0.6), size=5) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0)) +
  scale_fill_manual("Situação do Aluno", labels = c("Bolsista", "Não bolsista"), values = c("#4169E1", "#FF6347"))

################# Calcular nota média de todos os alunos que evadiram e nota média dos que não evadiram.

ggplot(total_alunos, aes(x=RA, y=NOTA_MEDIA, color=as.factor(EVADIDO))) +
  geom_point() +
  labs(title = "Perfil do Aluno", subtitle = "Distribuição da relação entre alunos e nota média",
       caption = "Data source: FHO Uniararas - Sistemas de Informação (2014 à 2019)") +
  ylab("Nota Média") +
  geom_hline(yintercept=mean(total_alunos$NOTA_MEDIA), size=0.6) +
  theme_update() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
        axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual("Situação do Aluno", labels = c("Não evadido", "Evadido"), values = c("#4169E1", "#FF6347")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

################# Top 10 matérias mais cursadas (gráfico de barras).

# Converte x para porcentagem considerando o total de alunos
calc_percent_top10 <- function(df) {
  for(i in 1:nrow(df)){
    df[i,2] <- round((df[i,2] / df[i,4] * 100), 0)
  }
  
  return(df)
}

top10Disci_APR <- select(total_alunos, matches("SIF|NCS")) %>%
  pivot_longer(cols = matches("SIF|NCS")) %>%
  count(name, value) %>%
  filter(value == 1) %>%
  group_by(name) %>%
  rename("tipo_reprov_disc" = value, "tot_alunos_disc" = n) %>%
  summarise(total_alunos_disc = as.double(sum(tot_alunos_disc))) %>%
  arrange(-total_alunos_disc) %>%
  head(10) %>%
  mutate(label = "A")

totDisciRepApro <- select(total_alunos, matches("SIF|NCS")) %>%
  pivot_longer(cols = matches("SIF|NCS")) %>%
  count(name, value) %>%
  filter(value > 1) %>%
  rename("tipo_reprov_disc" = value, "tot_alunos_disc" = n)

totDisciRepApro <- totDisciRepApro[order(
  totDisciRepApro$tot_alunos_disc, decreasing = TRUE),]

top10Disci_REP <- filter(totDisciRepApro, tipo_reprov_disc != 1 & name %in% top10Disci$name) %>%
  select(name, tot_alunos_disc) %>%
  group_by(name) %>%
  summarise(total_alunos_disc = as.double(sum(tot_alunos_disc))) %>%
  head(10) %>%
  arrange(name) %>%
  mutate(label = "R")

top10Disc <- rbind(top10Disci_REP, top10Disci_APR) 

top10Disc <- within(top10Disc, total_alunos <- ave(total_alunos_disc, name, FUN = sum))

top10Disc <- calc_percent_top10(top10Disc)

ggplot(top10Disc, aes(x = reorder(name, -total_alunos_disc) , 
                      y = total_alunos_disc, fill = label)) +
  geom_bar(stat="identity", width = 1, color = "black", position = "dodge2") +
  labs(title = "Perfil do Aluno", subtitle = "Top 10 matérias mais cursadas (%)",
       caption = "Data source: FHO Uniararas - Sistemas de Informação (2014 à 2019)") +
  ylab("% de Alunos") +
  xlab("Disciplinas") +
  geom_text(aes(label=total_alunos_disc), hjust = "center", color="black", 
            position=position_dodge(width=1),vjust=-0.4, size=3)+
  theme_update() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.title=element_text(size=9), axis.text=element_text(size=7)) +
  scale_fill_manual("Situação\ndo Aluno", labels = c("Aprovado", "Reprovado"), values = c("#4169E1", "#FF6347"))



################# Média de reprovações e aprovações (evadidos vs. não evadidos) (gráficos ou tabela separados)

totAproReproAlunos <- total_alunos %>%
  count(GRADE_CORRENTE,EVADIDO, TOT_APROVACOES, TOT_REPROVACOES)

medAprovaEvad <- head(totAproReproAlunos %>%
                        filter(EVADIDO == 1) %>%
                        mutate(TOT_APR_EVAD = n * TOT_APROVACOES) %>%
                        mutate(TOT_REP_EVAD = n * TOT_REPROVACOES) %>%
                        select(c(GRADE_CORRENTE, TOT_APR_EVAD, TOT_REP_EVAD)) %>%
                        group_by(GRADE_CORRENTE) %>%
                        summarise(across(everything(), mean))) %>%
  rename(`Ano` = GRADE_CORRENTE,
         `Média - Total Aprovações` = TOT_APR_EVAD,
         `Média - Total Reprovações` = TOT_REP_EVAD)

medAprovaEvad$`Média - Total Aprovações` <- format(medAprovaEvad$`Média - Total Aprovações`,
                                           digits = 2, decimal.mark = ".")
medAprovaEvad$`Média - Total Reprovações` <- format(medAprovaEvad$`Média - Total Reprovações`,
                                            digits = 2, decimal.mark = ".")

grid.newpage()
table <- tableGrob(medAprovaEvad, rows = NULL)
h <- grobHeight(table)
w <- grobWidth(table)
title <- textGrob("Perfil do aluno evadido", y=unit(0.5,"npc") + 0.5*h, 
                  vjust=-4, gp=gpar(fontsize=20))
footnote <- textGrob("Média de aprovações x Média de reprovações", 
                     x=unit(0.25,"npc") - 0.5*w,
                     y=unit(0.5,"npc") - 0.5*h, 
                     vjust=-13, hjust=0,gp=gpar( fontface="italic"))
gt <- gTree(children=gList(table, title, footnote))
grid.draw(gt)


medAprovaNaoEvad <- head(totAproReproAlunos %>%
                           filter(EVADIDO == 0) %>%
                           mutate(TOT_APR_N_EVAD = n * TOT_APROVACOES) %>%
                           mutate(TOT_REP_N_EVAD = n * TOT_REPROVACOES) %>%
                           select(c(GRADE_CORRENTE, TOT_APR_N_EVAD, TOT_REP_N_EVAD)) %>%
                           group_by(GRADE_CORRENTE) %>%
                           summarise(across(everything(), mean)))%>%
  rename(`Ano` = GRADE_CORRENTE,
         `Média - Total Aprovações` = TOT_APR_N_EVAD,
         `Média - Total Reprovações` = TOT_REP_N_EVAD)

medAprovaNaoEvad$`Média - Total Aprovações` <- format(medAprovaNaoEvad$`Média - Total Aprovações`,
                                              digits = 2, decimal.mark = ".")

medAprovaNaoEvad$`Média - Total Reprovações` <- format(medAprovaNaoEvad$`Média - Total Reprovações`,
                                               digits = 2, decimal.mark = ".")

grid.newpage()
table <- tableGrob(medAprovaNaoEvad, rows = NULL)
h <- grobHeight(table)
w <- grobWidth(table)
title <- textGrob("Perfil do aluno não evadido", y=unit(0.5,"npc") + 0.5*h, 
                  vjust=-4, gp=gpar(fontsize=20))
footnote <- textGrob("Média de aprovações x Média de reprovações", 
                     x=unit(0.25,"npc") - 0.5*w,
                     y=unit(0.5,"npc") - 0.5*h, 
                     vjust=-13, hjust=0,gp=gpar( fontface="italic"))
gt <- gTree(children=gList(table, title, footnote))
grid.draw(gt)