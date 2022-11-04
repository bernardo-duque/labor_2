setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Input")

pacman::p_load(sf, tidyverse, geobr, ggplot2, terra, spData, 
               tictoc, readxl, writexl, highcharter, janitor,
               tidytext, RColorBrewer, tm, grDevices, reshape2, 
               R.utils, readr, data.table, zoo, lubridate,
               stringr, Hmisc,scales, gridExtra)

#####----- Base Filiacao ------#####

# selecionando as colunas relevantes

colunas <- c("titulo_eleitoral", "id_municipio","nome", "sigla_partido","data_filiacao", 
             "situacao_registro","data_desfiliacao", "data_cancelamento")

# baixando a base

tic("Tempo para baixar a base")
print("Comecando a baixar a base")

df <- read_delim("base_filiacao.csv", ",", col_names = TRUE,
                 col_select = all_of(colunas), progress = show_progress())

print("Terminou de baixar a base")
toc() #10-13 min

# criando amostra para faciliar tratamento

amostra <- sample_n(df,size = 10000,replace = F)

#save(amostra, file = "amostra_base_filiacao.RData")
#save(df,file = "base_filiacao.RData")

# filtrando para os anos relevantes

amostra <- amostra %>% 
  filter(situacao_registro == "REGULAR" | 
           data_cancelamento >= "2013-01-01"| data_desfiliacao >= "2013-01-01")

# conciliando as datas de desfiliacao/cancelamento (tratarei como se fossem a mesma coisa)

amostra <- amostra %>%
  mutate(data_desfiliacao = ifelse(is.na(data_desfiliacao) == T & is.na(data_cancelamento) == T,
                                   as.Date("2021-01-01"), 
                                   as.Date(data_desfiliacao)),
         data_cancelamento = ifelse(is.na(data_cancelamento) == T & is.na(data_desfiliacao) == T,
                                    as.Date("2021-01-01"),
                                    as.Date(data_cancelamento)),
         data_saida = ifelse(data_desfiliacao > data_cancelamento, zoo::as.Date(data_desfiliacao),
                             zoo::as.Date(data_cancelamento)))

amostra <- amostra %>% 
  mutate(data_desfiliacao = as.Date(data_desfiliacao),
         data_cancelamento = as.Date(data_cancelamento),
         data_saida = as.yearmon(as.Date(data_saida)))


# criando dummy para situacao regular e coluna para mes e ano de saida

amostra <- amostra %>%
  mutate(situacao_registro = ifelse(situacao_registro == "REGULAR",1,0),
         mes_saida = format(data_saida, "%m"),
         ano_saida = format(data_saida, "%Y")) %>%
  select(-data_desfiliacao,-data_cancelamento)

# definindo a virada do ano como outubro para captar mudancas pre eleitorais

amostra <- amostra %>%
  mutate(ano_saida = as.numeric(ano_saida), mes_saida = as.numeric(mes_saida),
         ano_saida = ifelse(mes_saida>10,ano_saida + 1,ano_saida),
         titulo_eleitoral = as.numeric(titulo_eleitoral))

colunas_anos <- vector(length = 8)
for (i in 2013:2020) {
  colunas_anos[i-2012] <- paste0("ano_",i)
  amostra[,colunas_anos[i-2012]] <- 0
}

base_ano <- amostra %>%
  mutate(ano_2013 = ifelse(ano_saida >= 2013,1,0),
         ano_2014 = ifelse(ano_saida >= 2014,1,0),
         ano_2015 = ifelse(ano_saida >= 2015,1,0),
         ano_2016 = ifelse(ano_saida >= 2016,1,0),
         ano_2017 = ifelse(ano_saida >= 2017,1,0),
         ano_2018 = ifelse(ano_saida >= 2018,1,0),
         ano_2019 = ifelse(ano_saida >= 2019,1,0),
         ano_2020 = ifelse(ano_saida >= 2020,1,0))

### FALTA LIMPAR PARA O CASO DE MAIS DE UM PARTIDO POR PESSOA AQUI

partidos_ano <- base_ano %>%
  group_by(sigla_partido) %>%
  summarise(across(ano_2013:ano_2020, ~sum(.x)))

base_ano <- base_ano %>%
  summarise(across(ano_2013:ano_2020, ~sum(.x)))

base_ano <- base_ano %>%
  pivot_longer(everything(),names_to = "ano", values_to = "num_filiados")

base_ano$ano <- as.numeric(str_remove(base_ano$ano,"ano_"))

save(base_ano,partidos_ano,"bases_tratadas.RData")


source('/Users/bernardoduque/Documents/Puc/theme_publication_ggplot.R')

quartz()
grafico_1 <- ggplot(base_ano,aes(ano,num_filiados))+
  geom_line(size=1.3) + geom_point() +
  xlab("Ano") + ylab("Number of Affiliates") + 
  scale_colour_brewer() + theme_clean() +
  scale_y_continuous(labels = label_number(accuracy = 1000))

g1 <- ggplot(base_ano,aes(x=ano,y=num_filiados)) + 
  geom_line(color = "dodgerblue") +
  geom_point(aes(y=num_filiados),color = "dodgerblue") +
  #  geom_text(aes(label = num_empreg),vjust = -0.8)+
  xlab("Year") + ylab("Number of Affiliates") +
  scale_y_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(face = "bold")) 

line <- ggplot(base_ano,aes(ano,num_filiados)) + 
  geom_line(size=1.3) + geom_point()
grid.arrange(Line,(Line +scale_colour_Publication()+ theme_Publication()),nrow=1)


save(grafico_1,file = "grafico_1.RData")

  
## PEGAR OS 5 PARTIDOS COM MAIS FILIADOS E PIVOTLONGER
  

# amostra %>% mutate(n=1) %>% group_by(titulo_eleitoral) %>% summarise(contagem = sum(n)) mais de um partido


  
  
seq(from = 2013,to = as.numeric(amostra$ano_saida[5]), by = 1)
seq(2013,ano_saida,1)
  

#####----- Base Servidores -----#####

servidores <- read.csv("servidores_federais_2013-2020.csv")

# limpando servidores que estao com vinculo sigiloso

tipo_errado <- servidores %>%
  filter(tipo_vinculo == -11) %>%
  select(id_servidor) %>%
  pull

servidores <- servidores %>%
  filter(id_servidor %nin% tipo_errado,
         nome %nin% c("Sigiloso","SEM INFORMACAO"))

# alterando coluna de tipo de vinculo

servidores <- servidores %>%
  mutate(tipo_vinculo = ifelse(tipo_vinculo == 1, "Confianca",
                               ifelse(tipo_vinculo == 2, "Servidor","Outros")))

# criando dummys para separar os anos

servidores <- servidores %>%
  mutate(comeco_emprego = as.Date(comeco_emprego),
         fim_emprego = as.Date(fim_empregp)) %>%
  select(-fim_empregp)


# definindo o ano como acabando em novembro (ja que registro comeca no 1o dia do mes)

servidores <- servidores %>%
  mutate(ano_2013 = ifelse(comeco_emprego < "2013-11-01",1,0),
         ano_2014 = ifelse(comeco_emprego < "2014-11-01" &
                            fim_emprego >= "2013-10-01", 1,0),
         ano_2015 = ifelse(comeco_emprego < "2015-11-01" &
                             fim_emprego >= "2014-11-01", 1,0),
         ano_2016 = ifelse(comeco_emprego < "2016-11-01" &
                             fim_emprego >= "2015-11-01", 1,0),
         ano_2017 = ifelse(comeco_emprego < "2017-11-01" &
                             fim_emprego >= "2016-11-01", 1,0),
         ano_2018 = ifelse(comeco_emprego < "2018-11-01" &
                             fim_emprego >= "2017-11-01", 1,0),
         ano_2019 = ifelse(comeco_emprego < "2019-11-01" &
                             fim_emprego >= "2018-11-01", 1,0),
         ano_2020 = ifelse(comeco_emprego < "2020-11-01" &
                             fim_emprego >= "2019-11-01", 1,0))

# agrupando todos os servidores

total <- servidores %>%
  group_by(id_servidor,nome,cpf) %>% 
  summarise(comeco_emprego = min(comeco_emprego),
            fim_emprego = max(fim_emprego),
            across(starts_with("ano_"),sum))

total <- total %>%
  ungroup() %>%
  mutate(across(starts_with("ano_"),~ifelse(.x > 0,1,0)))

# criando base por ano

total_ano <- total %>%
  select(starts_with("ano_")) %>%
  summarise(across(everything(),sum)) %>%
  ungroup()

total_ano <- total_ano %>%
  pivot_longer(everything(),names_to = "ano",values_to = "num_empreg") %>%
  mutate(ano = as.integer(str_remove(ano,"ano_")))

g2 <- ggplot(total_ano,aes(x=ano,y=num_empreg)) + 
  geom_line(color = "dodgerblue") +
  geom_point(aes(y=num_empreg),color = "dodgerblue") +
#  geom_text(aes(label = num_empreg),vjust = -0.8)+
  xlab("Year") + ylab("Number of Workers") +
  scale_y_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(face = "bold")) 
  
