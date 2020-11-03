#Importando as libraries necessárias
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)

#Importando CSV pre-processado
total_alunos <- read.csv("pre_processed_analysis.csv", encoding = "UTF-8")

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

################# Top 20 matérias mais cursadas (gráfico de barras).
################# PERGUNTAR PARA O NEGRETTO

#Recebendo os codigos das disciplinas
cod_disciplina <- colnames(total_alunos[11:ncol(total_alunos)])

#Vetor para armazenar numero de cursantes de cada disciplina
num_alunos <- c()
#Contabilizando numero de cursantes para cada disciplina
for (i in 11:ncol(total_alunos)){
  t <- count(total_alunos, total_alunos[,i])
  num_alunos <- append(num_alunos, sum(t[2:nrow(t),"n"]))
}

#Criando Data frame e tratando os NA values (inserindo o valor 0)
top10Disc <- data.frame(cod_disciplina, num_alunos)
top10Disc[is.na(top10Disc)] <- 0

#Organizando em ordem decrescente os numeros de cursantes
top10Disc <- top10Disc[order(top10Disc$num_alunos, decreasing = TRUE), ]


ggplot(top10Disc[1:10, ], aes(x = reorder(cod_disciplina, -num_alunos) , 
                              y = num_alunos, fill = reorder(cod_disciplina, -num_alunos))) +
  geom_bar(stat="identity", width = 1, color = "white")+
  ylab("Nº de Alunos") +
  xlab("Disciplinas") +
  labs(title = "As 20 disciplinas mais cursadas de SI (2014-2019)") +
  geom_text(aes(label=num_alunos), vjust=-0.7, color="white",
            position = position_stack(vjust = 0.4), size=5)+
  scale_color_manual() +
  scale_fill_discrete(name = "Código da\nDisciplina ") +
theme_update()

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


