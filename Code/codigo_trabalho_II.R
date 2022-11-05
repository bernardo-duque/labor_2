setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Input")

pacman::p_load(sf, tidyverse, geobr, ggplot2, terra, spData, 
               tictoc, readxl, writexl, highcharter, janitor,
               tidytext, RColorBrewer, tm, grDevices, reshape2, 
               R.utils, readr, data.table, zoo, lubridate,
               stringr, Hmisc,scales, gridExtra, rio)

#####----- Base Filiacao ------#####

# selecionando as colunas relevantes

colunas <- c("titulo_eleitoral", "id_municipio","nome", "sigla_partido","data_filiacao", 
             "situacao_registro","data_desfiliacao", "data_cancelamento")

# baixando a base

tic("Tempo para baixar a base")
print("Comecando a baixar a base")

amostra <- import("base_filiacao.csv")

print("Terminou de baixar a base")
toc() #10-13 min

# criando amostra para faciliar tratamento

#amostra <- sample_n(amostra,size = 100000,replace = F)

#save(amostra, file = "amostra_base_filiacao.RData")
#save(amostra,file = "base_filiacao.rds")
read_rds("base_filiacao.rds")

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
         data_saida = ifelse(is.na(data_desfiliacao),data_cancelamento,
                             ifelse(is.na(data_cancelamento),data_desfiliacao,
                                    ifelse(data_desfiliacao > data_cancelamento,data_desfiliacao,
                                           data_cancelamento))))

amostra <- amostra %>% 
  mutate(data_saida = as.Date(data_saida),
         titulo_eleitoral = as.numeric(titulo_eleitoral)) %>%
  select(-data_cancelamento,-data_desfiliacao)

amostra <- amostra %>%
  filter(is.na(titulo_eleitoral)==F)

# definindo a virada do ano como outubro para captar mudancas pre eleitorais

amostra <- amostra %>%
  mutate(ano_2013 = ifelse(data_filiacao < "2013-11-01",1,0),
         ano_2014 = ifelse(data_filiacao < "2014-11-01" &
                             data_saida >= "2013-10-01", 1,0),
         ano_2015 = ifelse(data_filiacao < "2015-11-01" &
                             data_saida >= "2014-11-01", 1,0),
         ano_2016 = ifelse(data_filiacao < "2016-11-01" &
                             data_saida >= "2015-11-01", 1,0),
         ano_2017 = ifelse(data_filiacao < "2017-11-01" &
                             data_saida >= "2016-11-01", 1,0),
         ano_2018 = ifelse(data_filiacao < "2018-11-01" &
                             data_saida >= "2017-11-01", 1,0),
         ano_2019 = ifelse(data_filiacao < "2019-11-01" &
                             data_saida >= "2018-11-01", 1,0),
         ano_2020 = ifelse(data_filiacao < "2020-11-01" &
                             data_saida >= "2019-11-01", 1,0))

# analisando o caso de mais de um partido por pessoa no mesmo ano

print(paste0("Há ",round(((nrow(amostra) - length(unique(amostra$nome)))/nrow(amostra))*100,2),
             "% de indivíduos com mais de um partido na amostra"))

# 32,19% das obsrrvacoes com mais de um partido, fazendo checkpoint
# equivalente a 15,41% dos individuos
save(amostra,file = "base_filiacao_com_dupl.rds")

# removendo esses individuos

titulos <- which(duplicated(amostra$titulo_eleitoral))
titulos <- amostra$titulo_eleitoral[titulos]

# guardando os nomes para se retirar na base dos servidores
nomes_retirar <- amostra %>%
  filter(titulo_eleitoral %in% titulos) %>%
  select(nome) %>%
  pull

amostra <- amostra %>%
  filter(titulo_eleitoral %nin% titulos)

rm(titulos)

save(amostra,file = "base_filiacao_limpa.rds")

# montando as bases finais para plotar

partidos_ano <- amostra %>%
  group_by(sigla_partido) %>%
  summarise(across(ano_2013:ano_2020, ~sum(.x)))

base_ano <- amostra %>%
  summarise(across(ano_2013:ano_2020, ~sum(.x)))

base_ano <- base_ano %>%
  pivot_longer(everything(),names_to = "ano", values_to = "num_filiados")

base_ano$ano <- as.numeric(str_remove(base_ano$ano,"ano_"))

setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Output")
save(base_ano,partidos_ano,file = "bases_grafico_1.RData")

g1 <- ggplot(base_ano,aes(x=ano,y=num_filiados)) + 
  geom_line() +
  geom_point(aes(y=num_filiados)) +
  #  geom_text(aes(label = num_empreg),vjust = -0.8)+
  xlab("Year") + ylab("Number of Affiliates") +
  scale_y_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(face = "bold")) 

  
## plotando os partidos com mais numero de filiados

partidos_ano <- partidos_ano %>%
  filter(sigla_partido %in% c("MDB","PT","PSDB","PP","PDT"))

partidos_ano <- partidos_ano %>%
  pivot_longer(starts_with("ano_"),names_to = "anos", values_to = "num_fil")

partidos_ano$anos <- as.numeric(str_remove(partidos_ano$anos,"ano_"))

# plotando

g2 <- ggplot(partidos_ano, aes(x=anos, y=num_fil)) +
    geom_line(aes(linetype=sigla_partido)) +
   geom_point(aes(linetype=sigla_partido),size = 0.5) +
   xlab("Year") + ylab("Number of Affiliates") +
   scale_y_continuous(labels = comma) +
   theme_bw() + 
   theme(panel.grid.major.x = element_blank(),
        axis.text = element_text(face = "bold"))

save(g1,g2, file = "descritivas_filiacao.RData")

  
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

g3 <- ggplot(total_ano,aes(x=ano,y=num_empreg)) + 
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
  
