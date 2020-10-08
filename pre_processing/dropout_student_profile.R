#Importando as libraries necessárias
library(ggplot2)

#Importando CSV pre-processado
total_alunos <- read.csv("pre_processed_analysis.csv", encoding = "UTF-8")

# Filtra apenas os alunos que evadiram
alunos_evadidos <- filter(total_alunos, EVADIDO == 1)

residem <- as.numeric(nrow(filter(alunos_evadidos, RESID_ARARAS == 1)))

n_residem <- as.numeric(nrow(filter(alunos_evadidos, RESID_ARARAS == 0)))

plot1 <- data.frame(situacao = c("Reside", "Não reside"), num = c(residem, n_residem))

ggplot(plot1, aes(x = situacao , y = num, fill=situacao)) +
  geom_bar(stat="identity")+
  labs(title = "Perfil do Aluno Evadido: Quantos residem em Araras?") +
  xlab("Residem em Araras?") + 
  ylab("Nº de Alunos") +
  geom_text(aes(label=num), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=5)+
  theme_update()

  