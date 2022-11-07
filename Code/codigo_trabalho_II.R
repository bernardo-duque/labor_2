setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Input")

pacman::p_load(sf, tidyverse, geobr, ggplot2, terra, spData, 
               tictoc, readxl, writexl, highcharter, janitor,
               tidytext, RColorBrewer, tm, grDevices, reshape2, 
               R.utils, readr, data.table, zoo, lubridate,
               stringr, Hmisc,scales, gridExtra, rio, kableExtra)

#####----- Base Filiacao ------#####

# selecionando as colunas relevantes

colunas <- c("titulo_eleitoral", "id_municipio","nome", "sigla_partido","data_filiacao", 
             "situacao_registro","data_desfiliacao", "data_cancelamento")

# baixando a base

tic("Tempo para baixar a base")
print("Comecando a baixar a base")

amostra <- import("base_filiacao.csv", setclass = "tbl") %>%
  select(any_of(colunas))
  

print("Terminou de baixar a base")
toc() #3 min

# criando amostra para faciliar tratamento

#amostragem <- sample_n(amostra,size = 100000,replace = F)

#saveRDS(amostragem,file = "amostragem_filiacao.rds")
#amostra <- readRDS("amostragem_filiacao.rds")
#saveRDS(amostra,file = "base_filiacao.rds")
#amostra <- readRDS("base_filiacao.rds")

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

print(paste0("Há ",round(((nrow(amostra) - length(unique(amostra$titulo_eleitoral)))/nrow(amostra))*100,2),
             "% de observações com mais de um partido na amostra."))

# 13,36% das observacoes com mais de um partido, fazendo checkpoint
saveRDS(amostra,file = "base_filiacao_com_dupl.rds")
#amostra <- readRDS("base_filiacao_com_dupl.rds")

# vendo individuos com mais de um partido

titulos <- which(duplicated(amostra$titulo_eleitoral))
titulos <- amostra$titulo_eleitoral[titulos]

print(paste0("Há ",round(((length(unique(titulos)))/length(unique(amostra$titulo_eleitoral)))*100,2),
             "% de indivíduos com mais de um partido na amostra."))

#11,06% de individuos com mais de um partido

# guardando os nomes para se retirar na base dos servidores
nomes_retirar <- amostra %>%
  filter(titulo_eleitoral %in% titulos) %>%
  select(nome) %>%
  pull

# criando tabela com numero de pessoas com mais de um partido na amostra

t1 <- amostra %>%
  mutate(n=1) %>%
  group_by(titulo_eleitoral) %>%
  summarise(num_partidos = sum(n))

t1 <- t1 %>%
  ungroup() %>%
  count(num_partidos)

unicos <- length(unique(amostra$titulo_eleitoral))

t1 <- t1 %>%
  mutate(porc = (n/unicos)*100)

# criando base por partido

partidos_ano <- amostra %>%
  group_by(sigla_partido) %>%
  summarise(across(ano_2013:ano_2020, ~sum(.x)))

# removendo individuos com mais de um partido (11%)

amostra <- amostra %>%
  filter(titulo_eleitoral %nin% titulos)

rm(titulos)

saveRDS(amostra,file = "base_filiacao_limpa.rds")

setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Output")

save(t1,file = "num_part_pess.RData")

# criando base por ano

base_ano <- amostra %>%
  summarise(across(ano_2013:ano_2020, ~sum(.x)))

base_ano <- base_ano %>%
  pivot_longer(everything(),names_to = "ano", values_to = "num_filiados")

base_ano$ano <- as.numeric(str_remove(base_ano$ano,"ano_"))

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

g2 <- ggplot(partidos_ano, aes(x=anos, y=num_fil, group = sigla_partido)) +
      geom_line(aes(linetype=sigla_partido)) +
      geom_point(size = 0.5) +
      #xlab("Year") + ylab("Number of Affiliates") +
      labs(y = "Number of Affiliates", x= "Year", linetype = "Party") +
      scale_y_continuous(labels = comma) +
      scale_linetype_manual(values = c("longdash", "dotted","dashed", "solid", "dotdash")) +
      theme_bw() + 
      theme(panel.grid.major.x = element_blank(),
            axis.text = element_text(face = "bold"))

save(g1,g2, file = "descritivas_filiacao.RData")

  
#####----- Base Servidores -----#####

rm(g1,g2,t1,partidos_ano,base_ano)
setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Input")
#amostra <- readRDS("base_filiacao_limpa.rds")
servidores <- read.csv("servidores_outubro.csv")

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

# separando os anos

servidores <- servidores %>%
  mutate(ano = year(data_empregado))

servidores <- servidores %>%
  mutate(data_empregado = as.Date(data_empregado),
         data_ingresso = as.Date(data_dipl_ingresso_servico_publico)) %>%
  select(-data_dipl_ingresso_servico_publico)

# contando numero de pessoas unicas sem data de ingresso

unicos <- servidores %>%
  filter(is.na(data_ingresso))

print(paste0("Há ",round(((length(unique(unicos$id_servidor)))/length(unique(servidores$id_servidor)))*100,2),
             "% de indivíduos sem data de ingresso na base."))

# 7,47% de servidores unicos sem data de ingresso

# numero de servidores que entrou dps de 2013

pos_2013 <- unicos %>%
  group_by(id_servidor) %>%
  summarise(data_empregado = min(data_empregado)) %>%
  filter(data_empregado > "2013-10-01") %>%
  nrow

print(paste0("Há ",round((pos_2013/length(unique(servidores$id_servidor)))*100,2),
             "% de indivíduos sem data de ingresso na base, mas que entraram depois de 2013."))

# 3,7% de individuos sem data de ingresso, mas que entraram depois de 2013, 
# resta entao 3.7 de individuos sem data de ingresso que serao dropados da analise posteriormente

# alocando data de inicio

inicio <- unicos %>%
  group_by(id_servidor,tipo_vinculo) %>% 
  summarise(data_suplementar = min(data_empregado)) %>%
filter(data_suplementar > "2013-10-01")

# dando join

servidores <- servidores %>%
  left_join(inicio)

servidores <- servidores %>%
  mutate(data_ingresso = ifelse(is.na(data_ingresso),data_suplementar,data_ingresso)) %>%
  select(-data_suplementar)

# sanity check

unicos_2013 <- servidores %>%
  filter(is.na(data_ingresso))

print(paste0("Há ",round(((length(unique(unicos_2013$id_servidor)))/
                            length(unique(servidores$id_servidor)))*100,2),
             "% de indivíduos sem data de ingresso na base."))

# 3,76% de servidores unicos sem data de ingresso

servidores <- servidores %>%
  mutate(data_ingresso = as.Date(data_ingresso))

# calculando duracao de emprego em dias 

servidores <- servidores %>%
  mutate(duracao = (data_ingresso - data_empregado))

servidores <- servidores %>%
  mutate(duracao = str_remove(duracao, " days"),
         duracao = str_remove(duracao, "-"),
         duracao_mes = round(as.numeric(duracao)/30)) %>%
  select(-duracao)


### Olhando para os numeros anuais
# criando base por ano

servidores <- servidores %>%
  mutate(n=1)

total <- tibble(year = c(2013:2020), num_empreg = NA, num_serv = NA, num_comis = NA,
                num_outros = NA)
for (anos in 2013:2020) {
  
  temp <- servidores %>%
    filter(ano == anos)
  
  total$num_empreg[anos - 2012] <- length(unique(temp$id_servidor))
  
  # servidores
  
  temp <- servidores %>%
    filter(ano == anos,
           tipo_vinculo == "Servidor")
  
  total$num_serv[anos - 2012] <- length(unique(temp$id_servidor))
  
  # comissionados
  
  temp <- servidores %>%
    filter(ano == anos,
           tipo_vinculo == "Confianca")
  
  total$num_comis[anos - 2012] <- length(unique(temp$id_servidor))
  
  # outros
  
  temp <- servidores %>%
    filter(ano == anos,
           tipo_vinculo == "Outros")
  
  total$num_outros[anos - 2012] <- length(unique(temp$id_servidor))
  
  rm(temp)
    
}

# alongando a base

total <- total %>%
  pivot_longer(cols = c(num_empreg,num_serv,num_comis,num_outros),names_to = "num") %>%
  mutate(num = ifelse(num == "num_empreg", "Total", 
                      ifelse(num == "num_serv", "Career",
                             ifelse(num == "num_comis","Appointed", "Other"))))

# plotando base por ano

g3 <- ggplot(total,aes(x=year,y=value)) + 
  geom_line(aes(group = num, linetype = num)) +
  scale_linetype_manual(values = c("dashed","twodash","dotted","solid")) +
  geom_point(size = 0.5) +
  #  geom_text(aes(label = num_empreg),vjust = -0.8)+
  labs(y = "Number of Public Workers", x= "Year", linetype = "Type") +
  scale_y_continuous(labels = comma) +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(face = "bold")) 

totais <- total %>%
  filter(num == "Total") %>%
  arrange(year) %>%
  mutate(total = value) %>%
  select(year,total)

t2 <- total %>%
  left_join(totais) %>%
  filter(num != "Total") %>%
  group_by(year,num) %>%
  summarise(porc = (value/total)*100)

g4 <- ggplot(t2,aes(x=year,y=porc)) + 
  geom_line(aes(group = num, linetype = num)) +
  scale_linetype_manual(values = c("dashed","twodash","dotted","solid")) +
  geom_point(size = 0.5) +
  #  geom_text(aes(label = num_empreg),vjust = -0.8)+
  labs(y = "% of Public Workers", x= "Year", linetype = "Type") +
  scale_y_continuous(labels = comma) +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(face = "bold")) 


setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Output")
save(g3,g4,file = "descritivas_trabalhadores.RData")

rm(g3,g4,unicos_2013,inicio,t2,totais,total,anos,pos_2013,tipo_errado)

### Olhando agora para os desligamentos
# salvando servidores para nao ter q rodar de novo o tratamento
setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Input")
saveRDS(servidores,file = "base_servidores_antes_ano.rds")
setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Output")

servidores <- servidores %>%
  mutate(empregado = 1)

## criando painel

# criando base do zero
servidores_ano <- tibble(ano = rep(2013:2020,length(unique(servidores$id_servidor))))

ids <- unique(servidores$id_servidor)
ids = rep(ids,length(2013:2020))

servidores_ano <- servidores_ano %>%
  arrange(ano) %>%
  cbind(ids) %>%
  mutate(id_servidor = ids) %>%
  select(-ids)

servidores_wide <- servidores %>%
  select(id_servidor,nome,cpf,tipo_vinculo,ano,duracao_mes,empregado)

# transformando base em wide

servidores_wide <- servidores_wide %>%
  group_by(id_servidor) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = tipo_vinculo, values_from = empregado) %>%
  select(-row)

servidores_wide <- servidores_wide %>%
  mutate(across(Servidor:Outros,~ifelse(is.na(.x),0,.x)))

# dando join

empregados <- servidores_wide %>%
  group_by(id_servidor,ano) %>%
  summarise(across(Servidor:Outros, sum))

servidores_ano <- servidores_ano %>%
  left_join(empregados)

rm(empregados)

# pegando info sobre duracao

durac <- servidores_wide %>%
  group_by(id_servidor,ano) %>%
  summarise(duracao_mes = max(duracao_mes))

servidores_ano <- servidores_ano %>%
  left_join(durac)

rm(durac)

# pegando info sobre nome 

nome <- servidores_wide %>%
  select(id_servidor,nome)

dupl <- duplicated(nome$id_servidor)

nome <- nome[!dupl,]

servidores_ano <- servidores_ano %>%
  left_join(nome)

# corrigindo nas e dupla contagem

servidores_ano <- servidores_ano %>%
  mutate(across(Servidor:duracao_mes, ~ifelse(is.na(.x),0,.x)),
         across(Servidor:duracao_mes, ~ifelse(.x > 0 ,1,0)))

# criando dummy para emprego

servidores_ano <- servidores_ano %>%
  mutate(empregado = ifelse(Servidor + Confianca + Outros > 0,1,0))

# usar coluna com dumy de emprego por ano para auxiliar na criacao do desligamento
servidores_ano <- servidores_ano %>%
  group_by(id_servidor) %>%
  mutate(desligado = ifelse(empregado == 1, 0,
                            ifelse()))




