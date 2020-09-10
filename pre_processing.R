# Importação das libs
library(dplyr)
library(ggplot2)
library(scales)
library(data.table)
library(tidyr)

# Leitura do dataset
evasao_alunos <- read.csv("datasets/dados_evasao.csv", encoding = "UTF-8")

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


# TODOS:
# Fazer a média geral por aluno, agregar as notas para todos os RAs repetidos
# Verificar se existem casos onde o aluno tem 2 reprovações para a mesma coluna (REPROVADO P. NOTA 2x)


# Resolve os NANs das colunas devidas (apenas da coluna 12 por enquanto)
# depois esta coluna é reindexada para a coluna 7
valuesToColumnMean <- function(df) {
  for (i in 1:ncol(df)) {
    if (is.numeric(df[,i]) && i == 12) {
      df[is.na(df[,i]), i] <- round(mean(df[,i], na.rm = TRUE))
    }
  }
  return(df)
}

evasao_filtrado <- valuesToColumnMean(evasao_filtrado)

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

# Substituir todos os APROVADOS ou REPROVADOS por 1
evasao_filtrado <- sapply(evasao_filtrado, function(x) {
  x <- gsub("^.*(APROVADO|REPROVADO).*$", 1, x)}) %>% as.data.frame()

# Substitui todos os NAN por 0, o que indicando que o aluno não cursou
# ou não teve o preenchimento do registro informado.
evasao_filtrado[is.na(evasao_filtrado)] <- as.numeric(0)


# Exporta o dataset final
write.csv(evasao_filtrado, file = "test.csv")




# PLAYGROUND
country<-data.frame(c("87389","87389","87389", "87389"),
                    c("LÓGICA","MATEMÁTICA","LOGICA", "LOGICA"),
                    c(10, 7, 8.7, 4), c("2018", "2018", "2018", "2018"),
                    c("APROVADO","APROVADO","REPROVADO NOTA", "REPROVADO NOTA"))

colnames(country)<- c("RA","DISCIPLINA","NOTA", "GRADE", "STATUS_DSC")
country_L_to_w <- reshape(data=country,idvar="RA",
                          v.names = "STATUS_DSC",
                          timevar = "DISCIPLINA",
                          direction="wide")


# https://stackoverflow.com/questions/29271549/replace-all-occurrences-of-a-string-in-a-data-frame#:~:text=If%20you%20are%20only%20looking,var2%201%20a%20%3C2%20%3C3
# https://www.datasciencemadesimple.com/reshape-in-r-from-wide-to-long-from-long-to-wide/#:~:text=Reshape%20from%20wide%20to%20long,()%20and%20cast()%20function.


