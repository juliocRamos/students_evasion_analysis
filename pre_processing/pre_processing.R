# Importacao das libs
library(dplyr)
library(ggplot2)
library(scales)
library(data.table)
library(tidyr)

# TODO: colocar a coluna TOT_REPROVACOES entre as primeiras colunas.
# da forma que fiz ela fica na última posição.

# Leitura do dataset
evasao_alunos <- read.csv("datasets/dados_evasao.csv", encoding = "UTF-8")

# Verifico o tipo das colunas e se existem valores
str(evasao_alunos)
apply(evasao_alunos, 2, function(x) any(is.na(x)))

# Filtra apenas a grade superior a 2015
evasao_filtrado <- filter(evasao_alunos, GRADE_CORRENTE >= "2015")

tmp_1 <- evasao_filtrado

# Tranformando os Status da coluna DSC_STATUS_MAT em numérico onde:
# 0 = Não Cursou a disciplina(*)
# 1 = APROVADO
# 2 = REPROVADO POR NOTA
# 3 = REPROVADO POR FREQ.
# 4 = REPROVADO NOTA & FREQ.
# 5 = TEVE REPROVAÇÃO(ÕES) E FOI APROVADO POSTERIORMENTE(*)
# (*) Serão inseridos nas próximas linhas os valores
tmp_1$DSC_STATUS_MAT[which(tmp_1$DSC_STATUS_MAT == "APROVADO")] <- as.numeric(as.character(1))
tmp_1$DSC_STATUS_MAT[which(tmp_1$DSC_STATUS_MAT == "REPROVADO POR NOTA")] <- as.numeric(as.character(2))
tmp_1$DSC_STATUS_MAT[which(tmp_1$DSC_STATUS_MAT == "REPROVADO POR FREQ.")] <- as.numeric(as.character(3))
tmp_1$DSC_STATUS_MAT[which(tmp_1$DSC_STATUS_MAT == "REPROVADO NOTA & FREQ.")] <- as.numeric(as.character(4))

# Faz o transpose entre duas colunas (COD_MATERI e DSC_STATUS_MAT), fazendo uma coluna de disciplina em seus
# respectivos status para cada aluno e fazendo o merge de RAs (por aluno).
# Obs.: values_fill = 0 ---> Valores null corresponde a alunos que não cursaram a disciplina,
# atribuindo o valor 0 para esses casos;
# values_fn = function(x) -----> Função criada para tratar dados duplicados (Alunos que possuem duas aprovações
# ou que foram reprovados e aprovados na ultima ocorrencia).
tmp_1 <- pivot_wider(tmp_1, 
                     id_cols = RA, 
                     names_from = COD_MATERI, 
                     values_from = DSC_STATUS_MAT, 
                     values_fill = 0, 
                     values_fn = function(x){
                        if(length(x) > 1){
                          last_value <- tail(x, n=1)
                          first_value <- head(x, n=1)
                          if (first_value == "1") {
                            return(1)
                          } else if(last_value %in% c("2", "3", "4")) {
                            return(as.numeric(as.character(last_value)))
                          } else {
                            return(5)
                          }
                        } else {
                          return(as.numeric(as.character(x)))
                        }
                      })

# Faco o replace de , por .
evasao_filtrado$NOTA_MEDIA <- gsub(",", '.', evasao_filtrado$NOTA_MEDIA, fixed =T)
evasao_filtrado$PONTUACAO_PS <- gsub(",", '.', evasao_filtrado$PONTUACAO_PS, fixed =T)

# Converto as notas para numerico
evasao_filtrado$NOTA_MEDIA <- as.numeric(as.character(evasao_filtrado$NOTA_MEDIA))
evasao_filtrado$PONTUACAO_PS <- as.numeric(as.character(evasao_filtrado$PONTUACAO_PS))

# Unifica o nome de algumas colunas em uma nova, SHA5795_APROVADO
evasao_filtrado <- evasao_filtrado %>%
  unite(DSC_MAT_STATUS, COD_MATERI, DSC_STATUS_MAT, sep = "_", remove = FALSE)

# Resolve os NANs das colunas devidas (apenas da coluna 12 por enquanto)
# depois esta coluna sera reindexada para a coluna 7
valuesToColumnMean <- function(df) {
  for (i in 1:ncol(df)) {
    if (is.numeric(df[,i]) && i == 12) {
      df[is.na(df[,i]), i] <- round(mean(df[,i], na.rm = TRUE))
    }
  }
  return(df)
}

evasao_filtrado <- valuesToColumnMean(evasao_filtrado)

# Criando dataframe temporario (calculo da nota media) 
tmp <- evasao_filtrado

# Preciso dropar algumas colunas e possivelmente adicionar novamente depois
# para fazer o reshape
columns_to_remove <- c("X", "COD_MATERI", "COD_MATERI", "DSC_MAT")

`%ni%` <- Negate(`%in%`)
evasao_filtrado <- subset(evasao_filtrado, select = names(evasao_filtrado)
                          %ni% columns_to_remove)

# Faz o reshape da base criando todas as colunas de MAT + STATUS_APROVACAO para 
# todos os alunos
evasao_filtrado <- reshape(data=evasao_filtrado,idvar="RA",
                           v.names = "DSC_STATUS_MAT",
                           timevar = "DSC_MAT_STATUS",
                           direction="wide",
                           sep = "_")

# Alterando as notas das disciplinas pela media geral de cada aluno 
evasao_filtrado$NOTA_MEDIA <- round(tapply(tmp$NOTA_MEDIA, tmp$RA, mean), 2)


# Substituir todos os APROVADOS ou REPROVADOS por 1
evasao_filtrado <- sapply(evasao_filtrado, function(x) {
  x <- gsub("^.*(APROVADO|REPROVADO).*$", 1, x)
}) %>% as.data.frame()

# Substitui todos os NAN por 0, o que indicando que o aluno nao cursou
# ou nao teve o preenchimento do registro informado.
evasao_filtrado[is.na(evasao_filtrado)] <- 0


# Conta as reprovacoes por aluno
count_reprovacoes <- filter(evasao_alunos, grepl("REPROVADO", DSC_STATUS_MAT)
                            & GRADE_CORRENTE >= "2015") %>%
  count(RA, DSC_MAT, DSC_STATUS_MAT) %>%
  count(RA) %>%
  filter(n >= 1) %>%
  unite(merged_rows, c(RA, n), sep = ", ") 


# Cria a coluna TOT_REPROVACOES (cai em última posição)
evasao_filtrado["TOT_REPROVACOES"] <- 0

criarColunasTotReprovacoes <- function(d1, d2) {

  # Processo os alunos linha a linha para substituir apenas os dos índices corretos
  for (x in d1){
    for (i in 1:nrow(d1)) {
      ra <- as.character(strsplit(x, split = ",")[[i]][[1]])
      reprovacoes <- as.numeric(strsplit(x, split = ",")[[i]][[2]])

      if (nrow(d2[which(d2$RA == ra), ] != '')) {
        d2$TOT_REPROVACOES[which(d2$RA == ra)] <- reprovacoes
      }
    }
  }
  
  return(d2)
}

# Aplica as reprovacoes para os alunos adequados
evasao_filtrado <- criarColunasTotReprovacoes(count_reprovacoes, evasao_filtrado)

# Exporta o dataset final
write.csv(evasao_filtrado, file = "pre_processed_analysis.csv")









# PLAYGROUND
country<-data.frame(c("87389","87389","87389", "87389"),
                    c("LOGICA","MATEMATICA","LOGICA", "LOGICA"),
                    c(10, 7, 8.7, 4), c("2018", "2018", "2018", "2018"),
                    c("APROVADO","APROVADO","REPROVADO NOTA", "REPROVADO NOTA"))

colnames(country)<- c("RA","DISCIPLINA","NOTA", "GRADE", "STATUS_DSC")
country_L_to_w <- reshape(data=country,idvar="RA",
                          v.names = "STATUS_DSC",
                          timevar = "DISCIPLINA",
                          direction="wide")



# https://stackoverflow.com/questions/29271549/replace-all-occurrences-of-a-string-in-a-data-frame#:~:text=If%20you%20are%20only%20looking,var2%201%20a%20%3C2%20%3C3
# https://www.datasciencemadesimple.com/reshape-in-r-from-wide-to-long-from-long-to-wide/#:~:text=Reshape%20from%20wide%20to%20long,()%20and%20cast()%20function.


