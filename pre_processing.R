# Importação das libs
library(dplyr)
library(ggplot2)
library(scales)
library(data.table)
library(tidyr)

# Leitura do dataset
evasao_alunos <- read.csv("dados_evasao.csv", encoding = "UTF-8")

# Verifico o tipo das colunas e se existem valores
str(evasao_alunos)
apply(evasao_alunos, 2, function(x) any(is.na(x)))

# Filtra apenas a grade superior a 2015
evasao_filtrado <- filter(evasao_alunos, GRADE_CORRENTE >= "2015")

# Faço o replace de , por .
evasao_filtrado$NOTA_MEDIA <- gsub(",", '.', evasao_filtrado$NOTA_MEDIA, fixed =T)
evasao_filtrado$PONTUACAO_PS <- gsub(",", '.', evasao_filtrado$PONTUACAO_PS, fixed =T)

# Converto as notas para numérico
evasao_filtrado$NOTA_MEDIA <- as.numeric(as.character(evasao_filtrado$NOTA_MEDIA))
evasao_filtrado$PONTUACAO_PS <- as.numeric(as.character(evasao_filtrado$PONTUACAO_PS))

# Unifica o nome de algumas colunas em uma nova, SHA5795_APROVADO
evasao_filtrado <- evasao_filtrado %>%
  unite(DSC_MAT_STATUS, COD_MATERI, DSC_STATUS_MAT, sep = "_", remove = FALSE)

# Antes de fazer este agrupamento precisamos decidir algumas coisas...
# Vamos usar as pontuações ps e médias por matéria? Se sim, teremos que mudar um pouco as coisas
# se não vamos usar a média geral e teremos que calcular por aluno antes do agrupamento

# a grade corrente também teria que ter colunas baseadas em médias dos alunos
# ou algo no sentido pois o aluno pode ter cursado matérias de grades diferentes.




# Preciso dropar algumas colunas e possívelmente adicionar novamente depois
# para fazer o reshape
columns_to_remove <- c("X", "COD_MATERI", "COD_MATERI", "DSC_MAT")

`%ni%` <- Negate(`%in%`)
evasao_filtrado <- subset(evasao_filtrado, select = names(evasao_filtrado)
                          %ni% columns_to_remove)

# Faz o reshape da base criando todas as colunas de MAT + STATUS_APROVACAO para todos os alunos
evasao_filtrado <- reshape(data=evasao_filtrado,idvar="RA",
                           v.names = "DSC_STATUS_MAT",
                           timevar = "DSC_MAT_STATUS",
                           direction="wide",
                           sep = "_")
















# PLAYGROUND
country<-data.frame(c("87389","87391","73220", "87389"),
                    c("LÒGICA","MATEMÀTICA","ALGORITMOS", "LOGICA"),
                    c(10, 7, 8.7, 4), c("2018", "2018", "2018", "2018"),
                    c("APROVADO","APROVADO","APROVADO", "REPROVADO NOTA"))

colnames(country)<- c("RA","DISCIPLINA","NOTA", "GRADE", "STATUS_DSC")
country_L_to_w <- reshape(data=country,idvar="RA",
                          v.names = "STATUS_DSC",
                          timevar = "DISCIPLINA",
                          direction="wide")


# https://www.datasciencemadesimple.com/reshape-in-r-from-wide-to-long-from-long-to-wide/#:~:text=Reshape%20from%20wide%20to%20long,()%20and%20cast()%20function.


