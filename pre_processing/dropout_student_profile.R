#Importando as libraries necessárias
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

#Importando CSV pre-processado
total_alunos <- read.csv("pre_processed_analysis.csv", encoding = "UTF-8")

################# Relação geral de evasão: Araras X Outras Cidades

# Relação de alunos evadidos de Araras e outras cidades
alunos_evadidos_araras <- total_alunos %>%
  count(EVADIDO, RESID_ARARAS) %>%
  count(RESID_ARARAS)

# Converte x para porcentagem considerando o total de alunos
calc_percent <- function(x) {
  return(round((x / nrow(total_alunos) * 100), 2))
}

alunos_evadidos_araras$n <-lapply(alunos_evadidos_araras$n, calc_percent)

alunos_evadidos_araras$RESID_ARARAS[alunos_evadidos_araras$RESID_ARARAS == 1] <- "SIM"
alunos_evadidos_araras$RESID_ARARAS[alunos_evadidos_araras$RESID_ARARAS == 0] <- "NÃO"

ggplot(alunos_evadidos_araras, aes(x = "" , y = n, fill=RESID_ARARAS)) +
  geom_bar(stat="identity", width = 1, color = "white")+
  coord_polar("y", start=0) +
  labs(title = "Perfil do Aluno Evadido: Quantos residem em Araras? (%)") +
  geom_text(aes(label=n), vjust=-0.6, color="white",
            position = position_stack(vjust = 0.4), size=5)+
  theme_void()

################# Relação de Evadidos e Não Evadidos

# Relação de Evadidos e Não Evadidos
relac_eva_neva <- total_alunos %>%
  count(EVADIDO)

relac_eva_neva$n <- lapply(relac_eva_neva$n, calc_percent)

relac_eva_neva$EVADIDO[relac_eva_neva$EVADIDO == 1] <- "EVADIDO"
relac_eva_neva$EVADIDO[relac_eva_neva$EVADIDO == 0] <- "NAO EVADIDO"

ggplot(relac_eva_neva, aes(x = "" , y = n, fill=EVADIDO)) +
  geom_bar(stat="identity", width = 1, color = "white")+
  coord_polar("y", start=0) +
  labs(title = "Relação de Evadidos e Não Evadidos (%)") +
  geom_text(aes(label=n), vjust=-0.7, color="white",
            position = position_stack(vjust = 0.4), size=5)+
  theme_void()

################## Quantidade de alunos por grade (2015 até 2020)

# Alunos por grade
alunos_por_grade <- total_alunos 

alunos_por_grade <- total_alunos %>%
  count(GRADE_CORRENTE, EVADIDO) %>%
  count(GRADE_CORRENTE)

alunos_por_grade$n <- lapply(alunos_por_grade$n, calc_percent)


ggplot(alunos_por_grade, aes(x = GRADE_CORRENTE , y = n, fill = as.factor(GRADE_CORRENTE))) +
  geom_bar(stat="identity", width = 1, color = "white")+
  #coord_polar(theta= "y", start=0) +
  labs(title = "Relação de Evasões por grade (%)") +
  geom_text(aes(label=n), vjust=-0.7, color="white",
            position = position_stack(vjust = 0.4), size=5)+
  scale_fill_discrete(name = "Grade Corrente") +
  scale_color_manual()
theme_update()


################# Bolsistas vs não bolsistas

bolsista <- calc_percent(as.numeric(nrow(filter(total_alunos, BOLSISTA == 1))))

n_bolsista <- calc_percent(as.numeric(nrow(filter(total_alunos, BOLSISTA == 0))))

plot2 <- data.frame(situacao = c("Bolsista", "Não bolsista"), num = c(bolsista, n_bolsista))

plot_bolsista <- ggplot(plot2, aes(x="", y=num, fill=situacao))+
  geom_bar(width = 1, stat = "identity", color="white")+
  coord_polar("y", start=0)+
  labs(title = "Perfil do Aluno:\n Quantos possuem bolsa de estudos? (%)") +
  geom_text(aes(label=num), vjust=-0.7, color="white",
            position = position_stack(vjust = 0.5), size=7)+
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
plot_bolsista + theme(plot.background = element_rect(fill = "#e2ded3", colour = "#e2ded3"))
plot_bolsista + scale_fill_discrete(name = "Situação do Aluno")

################# Calcular nota média de todos os alunos que evadiram e nota média dos que não evadiram.

ggplot(total_alunos, aes(x=RA, y=NOTA_MEDIA, color=as.factor(EVADIDO))) +
  geom_point() +
  labs(title = "Distribuição da relação entre alunos e nota média", subtitle = "Aluno evadido x Aluno não evadido",
       caption = "Data source: FHO Uniararas (2015 à 2020)") +
  ylab("Nota Média") +
  geom_hline(yintercept=mean(total_alunos$NOTA_MEDIA), size=0.6) +
  theme_update() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
        axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual("Situação do Aluno", labels = c("Não evadido", "Evadido"), values = c("#333ccc", "red")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

################# Quantidade de alunos que cursaram cada matéria (gráfico de barras).





################# Média de reprovações e aprovações (evadidos vs. não evadidos) (gráficos ou tabela separados)

  