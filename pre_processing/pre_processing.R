# Importacao das libs
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

# Faco o replace de , por .
evasao_filtrado$NOTA_MEDIA <- gsub(",", '.', evasao_filtrado$NOTA_MEDIA, fixed =T)
evasao_filtrado$PONTUACAO_PS <- gsub(",", '.', evasao_filtrado$PONTUACAO_PS, fixed =T)

# Converto as notas para numerico
evasao_filtrado$NOTA_MEDIA <- as.numeric(as.character(evasao_filtrado$NOTA_MEDIA))
evasao_filtrado$PONTUACAO_PS <- as.numeric(as.character(evasao_filtrado$PONTUACAO_PS))

# Converte 'S' ou 'N' em booleano 0 ou 1
colunas_convertidas <- lapply(evasao_filtrado, function(x){
  gsub("S", 1, x)
})

colunas_convertidas <- lapply(colunas_convertidas, function(x){
  gsub("N", 0, x)
})

evasao_filtrado$EVADIDO <- colunas_convertidas$EVADIDO
evasao_filtrado$RESID_ARARAS <- colunas_convertidas$RESID_ARARAS
evasao_filtrado$BOLSISTA <- colunas_convertidas$BOLSISTA

# Resolve os NANs das colunas devidas (apenas da coluna 11 por enquanto)
# depois esta coluna sera reindexada para a coluna 7
valuesToColumnMean <- function(df) {
  for (i in 1:ncol(df)) {
    if (is.numeric(df[,i]) && i == 11) {
      df[is.na(df[,i]), i] <- round(mean(df[,i], na.rm = TRUE))
    }
  }
  return(df)
}

evasao_filtrado <- valuesToColumnMean(evasao_filtrado)

# Conta as reprovacoes por aluno
count_reprovacoes <- filter(evasao_filtrado, grepl("REPROVADO", DSC_STATUS_MAT)) %>%
  count(RA, DSC_MAT, DSC_STATUS_MAT) %>%
  count(RA) %>%
  filter(n >= 1) %>%
  unite(merged_rows, c(RA, n), sep = ", ")

# Conta as aprovacoes por aluno
count_aprovacoes <- filter(evasao_filtrado, grepl("APROVADO", DSC_STATUS_MAT)) %>%
  count(RA, DSC_MAT, DSC_STATUS_MAT) %>%
  count(RA) %>%
  filter(n >= 1) %>%
  unite(merged_rows, c(RA, n), sep = ", ")

count_tot_mat_cursadas <- filter(evasao_filtrado, 
                                 grepl("APROVADO|REPROVADO", DSC_STATUS_MAT)) %>%
  count(RA, DSC_MAT, DSC_STATUS_MAT) %>%
  count(RA) %>%
  filter(n >= 1) %>%
  unite(merged_rows, c(RA, n), sep = ", ")

# Cria a coluna TOT_REPROVACOES
evasao_filtrado <- evasao_filtrado %>%
  mutate(TOT_REPROVACOES = 0, .after=PONTUACAO_PS)

# Cria a coluna TOT_APROVACOES
evasao_filtrado <- evasao_filtrado %>%
  mutate(TOT_APROVACOES = 0, .after=TOT_REPROVACOES)

# Cria a coluna TOT_MAT_CURSADAS
evasao_filtrado <- evasao_filtrado %>%
  mutate(TOT_MAT_CURSADAS = 0, .after=TOT_APROVACOES)

# Processa os alunos linha a linha para preencher os campos corretos para cada RA
preencherNovasColunas <- function(d1, d2, col_name) {

  for (x in d1){
    for (i in 1:nrow(d1)) {
      ra <- as.character(strsplit(x, split = ",")[[i]][[1]])
      reprovacoes <- as.numeric(strsplit(x, split = ",")[[i]][[2]])

      if (nrow(d2[which(d2$RA == ra), ] != '')) {
        switch(col_name, 
               "TOT_REPROVACOES" = {
                 d2$TOT_REPROVACOES[which(d2$RA == ra)] <- reprovacoes 
               }, 
               "TOT_APROVACOES" = {
                 d2$TOT_APROVACOES[which(d2$RA == ra)] <- reprovacoes 
               }, 
               "TOT_MAT_CURSADAS" = {
                 d2$TOT_MAT_CURSADAS[which(d2$RA == ra)] <- reprovacoes
               })
      }
    }
  }

  return(d2)
}

# Aplica as reprovacoes, aprovavoes e total de matérias cursadas para os alunos adequados
evasao_filtrado <- criarNovaColuna(count_reprovacoes, evasao_filtrado, "TOT_REPROVACOES")
evasao_filtrado <- criarNovaColuna(count_aprovacoes, evasao_filtrado, "TOT_APROVACOES")
evasao_filtrado <- criarNovaColuna(count_tot_mat_cursadas, evasao_filtrado, "TOT_MAT_CURSADAS")

# Funcao que pega a lista de RAs do dataframe e faz o calculo da media geral,
# tanto da nota media das disciplinas como da pontuacao ps
calc_media_geral <- function(data_temp, ra_list){
  
  for (i in 1:length(ra_list)) {
    
    ra <- ra_list[i]
    
    if (nrow(data_temp[which(data_temp$RA == ra), ] != '')) {
      # Calcula a nota media geral
      data_temp$NOTA_MEDIA[which(data_temp$RA == ra)] <-
         round(mean(data_temp$NOTA_MEDIA[which(data_temp$RA == ra)]), 1)
       
      # # Calcula a pontuacao_ps geral
      data_temp$PONTUACAO_PS[which(data_temp$RA == ra)] <-
         round(mean(data_temp$PONTUACAO_PS[which(data_temp$RA == ra)]), 1)
    }
  }
  return (data_temp)
}

evasao_filtrado <- calc_media_geral(evasao_filtrado, unique(evasao_filtrado$RA))

# materias_por_aluno <- filter(evasao_alunos, GRADE_CORRENTE >= "2015")
materias_por_aluno <- evasao_filtrado

# Tranformando os Status da coluna DSC_STATUS_MAT em numérico onde:
# 0 = Não Cursou a disciplina(*)
# 1 = APROVADO
# 2 = REPROVADO POR NOTA
# 3 = REPROVADO POR FREQ.
# 4 = REPROVADO NOTA & FREQ.
# 5 = TEVE REPROVAÇÃO(ÕES) E FOI APROVADO POSTERIORMENTE(*)
# (*) Serão inseridos nas próximas linhas os valores
materias_por_aluno$DSC_STATUS_MAT[
  which(materias_por_aluno$DSC_STATUS_MAT == "APROVADO")] <- as.numeric(as.character(1))

materias_por_aluno$DSC_STATUS_MAT[
  which(materias_por_aluno$DSC_STATUS_MAT == "REPROVADO POR NOTA")] <- as.numeric(as.character(2))

materias_por_aluno$DSC_STATUS_MAT[
  which(materias_por_aluno$DSC_STATUS_MAT == "REPROVADO POR FREQ.")] <- as.numeric(as.character(3))

materias_por_aluno$DSC_STATUS_MAT[
  which(materias_por_aluno$DSC_STATUS_MAT == "REPROVADO NOTA & FREQ.")] <- as.numeric(as.character(4))

# Faz o transpose entre as colunas COD_MATERI e DSC_STATUS_MAT, criando uma coluna
# para cada status e para cada aluno, fazendo o merge de RAs (por aluno).
# values_fill = 0 (atribui-se 0 para alunos que não fizeram a matéria)
# values_fn = function(x) (Tratar dados duplicados (Alunos que possuem duas
# aprovações ou que foram reprovados e aprovados na ultima ocorrencia).
materias_por_aluno <- pivot_wider(materias_por_aluno, 
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

# Retirando algumas colunas desnecessárias no momento 
columns_to_remove <- c("X", "COD_MATERI", "DSC_STATUS_MAT", "DSC_MAT")

`%ni%` <- Negate(`%in%`)
evasao_filtrado <- subset(evasao_filtrado, select = names(evasao_filtrado)
                           %ni% columns_to_remove)

# Filtra os registros pela coluna RA e seleciona apenas as 10 primeiras
# colunas
evasao_filtrado <- distinct(evasao_filtrado, evasao_filtrado$RA,
                            .keep_all = TRUE) %>% select(1:10)


# Mergeia os dataframes por RA para adicionar as colunas de disciplina
evasao_filtrado <- merge(evasao_filtrado, materias_por_aluno, by = "RA")

# Exporta o dataframe final.
write.csv(evasao_filtrado, file = "pre_processed_analysis.csv")



# https://stackoverflow.com/questions/29271549/replace-all-occurrences-of-a-string-in-a-data-frame#:~:text=If%20you%20are%20only%20looking,var2%201%20a%20%3C2%20%3C3
# https://www.datasciencemadesimple.com/reshape-in-r-from-wide-to-long-from-long-to-wide/#:~:text=Reshape%20from%20wide%20to%20long,()%20and%20cast()%20function.


