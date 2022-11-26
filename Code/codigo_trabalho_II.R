setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Input")

pacman::p_load(sf, tidyverse, geobr, ggplot2, terra, spData, 
               tictoc, readxl, writexl, highcharter, janitor,
               tidytext, RColorBrewer, tm, grDevices, reshape2, 
               R.utils, readr, data.table, zoo, lubridate,
               stringr, Hmisc,scales, gridExtra, rio, kableExtra,
               ggpattern, ggfortify,dynlm, plm)

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
        axis.text = element_text(face = "bold")) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) 

  
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
            axis.text = element_text(face = "bold")) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) 

save(g1,g2, file = "descritivas_filiacao.RData")
save(base_ano, partidos_ano, file = "bases_descritivas_filiacao.RData")

  
#####----- Base Servidores -----#####

rm(g1,g2,t1,partidos_ano,base_ano,amostra)
setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Input")
servidores <- read.csv("servidores_outubro.csv")

# limpando servidores que estao com vinculo sigiloso

tipo_errado <- servidores %>%
  filter(tipo_vinculo == -11) %>%
  select(id_servidor) %>%
  pull

servidores <- servidores %>%
  filter(id_servidor %nin% tipo_errado,
         nome %nin% c("Sigiloso","SEM INFORMACAO"))

# checando quais sao as categorias mais frequentes tirando confianca e servidor

tipos <- servidores %>%
  count(tipo_vinculo) %>%
  arrange()

tipos_1 <- servidores %>%
  filter(tipo_vinculo == 1) %>%
  count(situacao_vinculo) %>%
  arrange(desc(n))

tipos_2 <- servidores %>%
  filter(tipo_vinculo == 2) %>%
  count(situacao_vinculo) %>%
  arrange(desc(n))

tipos_3 <- servidores %>%
  filter(tipo_vinculo == 3) %>%
  count(situacao_vinculo) %>%
  arrange(desc(n))

rm(tipos,tipos_1,tipos_2,tipos_3)

# alterando coluna de tipo de vinculo com base na definicao da base dos dados
# 1 Função ou Cargo de Confiança; 2 Cargo Emprego; 3 Demais situações - agentes públicos
# 4 Posto/Graduação
# Outros engloba basicamente pessoas sem vinculo, contratos temporarios, estagiarios

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

save(unicos_2013, file = "unicos_2013.RData")

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
        axis.text = element_text(face = "bold")) + 
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) 

totais <- total %>%
  filter(num != "Total") %>%
  group_by(year) %>%
  summarise(total = sum(value)) %>%
  select(year,total)

t2 <- total %>%
  left_join(totais) %>%
  filter(num != "Total") %>%
  group_by(year,num) %>%
  summarise(porc = round((value/total)*100,2))

g4 <- ggplot(t2, aes(x = year, y = porc)) + 
  geom_bar(aes(color = num, fill = num), 
            alpha = 0.5, position = position_dodge(0.8),
           stat = "identity", width = 0.4) +
 # geom_point(aes(y = porc, group = num),
  #           size = 0.5) +
  #geom_text(aes(label = porc),vjust = -0.8, size = 3)+
  scale_fill_manual(values=c("#21130d", "#1e81b0", "#76b5c5")) +
  scale_color_manual(values=c("#21130d", "#1e81b0", "#76b5c5"))+
  geom_text(aes(label = porc),vjust = -0.8, size = 3,
            hjust = ifelse(t2$num == "Career",.5,
                           ifelse(t2$num == "Appointed", 1.2,-.3))) +
  labs(y = "% of Public Workers", x= "Year", color = "Type", fill = "Type") +
  scale_y_continuous(labels = comma) +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        #panel.grid.minor.y = element_blank(),
        axis.text = element_text(face = "bold")) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) 

# g4 <- ggplot(t2, aes(x = year, y = porc)) + 
#   geom_bar_pattern(aes(pattern = num, color = num, fill =num), 
#            alpha = 0.5, position = position_dodge(0.8),
#            stat = "identity", width = 0.4,
#            pattern = 'polygon_tiling',
#            pattern_key_scale_factor = 1.2) +
#   scale_pattern_manual(values = c("stripe","none", "hexagonal")) +
#   geom_text(aes(label = porc),vjust = -0.8, size = 3,
#             hjust = ifelse(t2$num == "Career",.5,
#                            ifelse(t2$num == "Appointed", 1.2,-.3)))+
#   labs(y = "% of Public Workers", x= "Year", linetype = "Type") +
#   scale_y_continuous(labels = comma) +
#   theme_bw() + 
#   theme(panel.grid.major.x = element_blank(),
#         #panel.grid.minor.y = element_blank(),
#         axis.text = element_text(face = "bold")) 


setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Output")
save(g3,g4,file = "descritivas_trabalhadores.RData")
save(total,t2, file = "bases_descritivas_trabalhadores.RData")

rm(g3,g4,unicos,inicio,t2,totais,total,anos,pos_2013,tipo_errado)


### Olhando agora para os desligamentos
# salvando servidores para nao ter q rodar de novo o tratamento
setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Input")
saveRDS(servidores,file = "base_servidores_antes_ano.rds")
servidores <- readRDS(file = "base_servidores_antes_ano.rds")

####------ criando painel ------####

setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Input")
servidores <- readRDS(file = "base_servidores_antes_ano.rds")
setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Output")

servidores <- servidores %>%
  mutate(empregado = 1)

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

rm(nome)

# corrigindo nas e dupla contagem

servidores_ano <- servidores_ano %>%
  mutate(across(Servidor:duracao_mes, ~ifelse(is.na(.x),0,.x)),
         across(Servidor:Outros, ~ifelse(.x > 0 ,1,0)))

# criando dummy para emprego

servidores_ano <- servidores_ano %>%
  mutate(empregado = ifelse(Servidor + Confianca + Outros > 0,1,0))

# usar coluna com dumy de emprego por ano para auxiliar na criacao do desligamento

aux <- servidores_ano %>%
  mutate(ano_2013 = ifelse(empregado == 1 & ano == 2013,1,0),
         ano_2014 = ifelse(empregado == 1 & ano == 2014,1,0),
         ano_2015 = ifelse(empregado == 1 & ano == 2015,1,0),
         ano_2016 = ifelse(empregado == 1 & ano == 2016,1,0),
         ano_2017 = ifelse(empregado == 1 & ano == 2017,1,0),
         ano_2018 = ifelse(empregado == 1 & ano == 2018,1,0),
         ano_2019 = ifelse(empregado == 1 & ano == 2019,1,0),
         ano_2020 = ifelse(empregado == 1 & ano == 2020,1,0)) %>%
  select(id_servidor,starts_with("ano_"))

aux <- aux %>%
  group_by(id_servidor) %>%
  summarise(across(starts_with("ano_"),sum))

servidores_ano <- servidores_ano %>%
  left_join(aux)

rm(aux)

servidores_ano <- servidores_ano %>%
  mutate(desligado = ifelse(ano==2013,0,
                            ifelse(ano == 2014 & ano_2013 == 1 & empregado == 0,1,
                                   ifelse(ano == 2015 & ano_2014 == 1 & empregado == 0,1,
                                          ifelse(ano == 2016 & ano_2015 == 1 & empregado == 0,1,
                                                 ifelse(ano == 2017 & ano_2016 == 1 & empregado == 0,1,
                                                        ifelse(ano == 2018 & ano_2017 == 1 & empregado == 0,1,
                                                               ifelse(ano == 2019 & ano_2018 == 1 & empregado == 0,1,
                                                                      ifelse(ano == 2020 & ano_2019 == 1 & empregado == 0,1,0)))))))))

# criando dummy para contratado

servidores_ano <- servidores_ano %>%
  mutate(contratado = ifelse(ano==2013 & empregado == 1 & duracao_mes < 12,1,
                            ifelse(ano == 2014 & ano_2013 == 0 & empregado == 1,1,
                                   ifelse(ano == 2015 & ano_2014 == 0 & empregado == 1,1,
                                          ifelse(ano == 2016 & ano_2015 == 0 & empregado == 1,1,
                                                 ifelse(ano == 2017 & ano_2016 == 0 & empregado == 1,1,
                                                        ifelse(ano == 2018 & ano_2017 == 0 & empregado == 1,1,
                                                               ifelse(ano == 2019 & ano_2018 == 0 & empregado == 1,1,
                                                                      ifelse(ano == 2020 & ano_2019 == 0 & empregado == 1,1,0)))))))))

contratados <- servidores_ano %>%
  group_by(ano) %>%
  summarise(contratado = sum(contratado),
            desligado = sum(desligado)) %>%
  mutate(liq = contratado - desligado) %>%
  pivot_longer(names_to = "variable",values_to = "value",cols = c(contratado,desligado,liq))
  
  

g5 <- ggplot(contratados,aes(x=ano,y=value)) + 
  geom_line(aes(group = variable, linetype = variable)) +
  scale_linetype_manual(values = c("longdash","dotted","solid"), name = "Type",
                        labels = c("Hiring", "Dismissal", "Net Hiring")) +
  geom_point(size = 0.5) +
  #  geom_text(aes(label = num_empreg),vjust = -0.8)+
  labs(y = "Number of Public Workers", x= "Year") +
  scale_y_continuous(labels = comma, breaks = scales::breaks_pretty(n = 5)) +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(face = "bold"))   +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_vline(xintercept=c(2014,2016,2018), linetype="solid", color = "grey",
             alpha = 0.5, size = 4) #+
  #geom_vline(xintercept=c(2016,2020), linetype="solid", color = "grey",
  #           alpha = 0.2, size = 4)

# agora separando por tipo de vinculo
# nao ta o codigo mais limpo mas deu preguica de escrever um for

aux <- servidores_ano %>%
  mutate(ano_2013_serv = ifelse(Servidor == 1 & ano == 2013,1,0),
         ano_2014_serv = ifelse(Servidor == 1 & ano == 2014,1,0),
         ano_2015_serv = ifelse(Servidor == 1 & ano == 2015,1,0),
         ano_2016_serv = ifelse(Servidor == 1 & ano == 2016,1,0),
         ano_2017_serv = ifelse(Servidor == 1 & ano == 2017,1,0),
         ano_2018_serv = ifelse(Servidor == 1 & ano == 2018,1,0),
         ano_2019_serv = ifelse(Servidor == 1 & ano == 2019,1,0),
         ano_2020_serv = ifelse(Servidor == 1 & ano == 2020,1,0),
         ano_2013_conf = ifelse(Confianca == 1 & ano == 2013,1,0),
         ano_2014_conf = ifelse(Confianca == 1 & ano == 2014,1,0),
         ano_2015_conf = ifelse(Confianca == 1 & ano == 2015,1,0),
         ano_2016_conf = ifelse(Confianca == 1 & ano == 2016,1,0),
         ano_2017_conf = ifelse(Confianca == 1 & ano == 2017,1,0),
         ano_2018_conf = ifelse(Confianca == 1 & ano == 2018,1,0),
         ano_2019_conf = ifelse(Confianca == 1 & ano == 2019,1,0),
         ano_2020_conf = ifelse(Confianca == 1 & ano == 2020,1,0),
         ano_2013_outro = ifelse(Outros == 1 & ano == 2013,1,0),
         ano_2014_outro = ifelse(Outros == 1 & ano == 2014,1,0),
         ano_2015_outro = ifelse(Outros == 1 & ano == 2015,1,0),
         ano_2016_outro = ifelse(Outros == 1 & ano == 2016,1,0),
         ano_2017_outro = ifelse(Outros == 1 & ano == 2017,1,0),
         ano_2018_outro = ifelse(Outros == 1 & ano == 2018,1,0),
         ano_2019_outro = ifelse(Outros == 1 & ano == 2019,1,0),
         ano_2020_outro = ifelse(Outros == 1 & ano == 2020,1,0)) %>%
    select(id_servidor,matches("_serv|_conf|outro"))

aux <- aux %>%
  group_by(id_servidor) %>%
  summarise(across(matches("_serv|_conf|outro$"),sum))

servidores_ano <- servidores_ano %>%
  left_join(aux)

rm(aux)

# criando dummy de desligado por tipo (codigo feio...)

servidores_ano <- servidores_ano %>%
  mutate(desligado_serv = ifelse(ano==2013,0,
                            ifelse(ano == 2014 & ano_2013_serv == 1 & Servidor == 0,1,
                                   ifelse(ano == 2015 & ano_2014_serv == 1 & Servidor == 0,1,
                                          ifelse(ano == 2016 & ano_2015_serv == 1 & Servidor == 0,1,
                                                 ifelse(ano == 2017 & ano_2016_serv == 1 & Servidor == 0,1,
                                                        ifelse(ano == 2018 & ano_2017_serv == 1 & Servidor == 0,1,
                                                               ifelse(ano == 2019 & ano_2018_serv == 1 & Servidor == 0,1,
                                                                      ifelse(ano == 2020 & ano_2019_serv == 1 & Servidor == 0,1,0)))))))))


servidores_ano <- servidores_ano %>%
  mutate(desligado_conf = ifelse(ano==2013,0,
                            ifelse(ano == 2014 & ano_2013_conf == 1 & Confianca == 0,1,
                                   ifelse(ano == 2015 & ano_2014_conf == 1 & Confianca == 0,1,
                                          ifelse(ano == 2016 & ano_2015_conf == 1 & Confianca == 0,1,
                                                 ifelse(ano == 2017 & ano_2016_conf == 1 & Confianca == 0,1,
                                                        ifelse(ano == 2018 & ano_2017_conf == 1 & Confianca == 0,1,
                                                               ifelse(ano == 2019 & ano_2018_conf == 1 & Confianca == 0,1,
                                                                      ifelse(ano == 2020 & ano_2019_conf == 1 & Confianca == 0,1,0)))))))))

servidores_ano <- servidores_ano %>%
  mutate(desligado_outro = ifelse(ano==2013,0,
                                 ifelse(ano == 2014 & ano_2013_outro == 1 & Outros == 0,1,
                                        ifelse(ano == 2015 & ano_2014_outro == 1 & Outros == 0,1,
                                               ifelse(ano == 2016 & ano_2015_outro == 1 & Outros == 0,1,
                                                      ifelse(ano == 2017 & ano_2016_outro == 1 & Outros == 0,1,
                                                             ifelse(ano == 2018 & ano_2017_outro == 1 & Outros == 0,1,
                                                                    ifelse(ano == 2019 & ano_2018_outro == 1 & Outros == 0,1,
                                                                           ifelse(ano == 2020 & ano_2019_outro == 1 & Outros == 0,1,0)))))))))

# dummies para contratacao por tipo

servidores_ano <- servidores_ano %>%
  mutate(contratado_serv = ifelse(ano==2013 & Servidor == 1 & duracao_mes < 12,1,
                             ifelse(ano == 2014 & ano_2013_serv == 0 & Servidor == 1,1,
                                    ifelse(ano == 2015 & ano_2014_serv == 0 & Servidor == 1,1,
                                           ifelse(ano == 2016 & ano_2015_serv == 0 & Servidor == 1,1,
                                                  ifelse(ano == 2017 & ano_2016_serv == 0 & Servidor == 1,1,
                                                         ifelse(ano == 2018 & ano_2017_serv == 0 & Servidor == 1,1,
                                                                ifelse(ano == 2019 & ano_2018_serv == 0 & Servidor == 1,1,
                                                                       ifelse(ano == 2020 & ano_2019_serv == 0 & Servidor == 1,1,0)))))))))

servidores_ano <- servidores_ano %>%
  mutate(contratado_conf = ifelse(ano==2013 & Confianca == 1 & duracao_mes < 12,1,
                                  ifelse(ano == 2014 & ano_2013_conf == 0 & Confianca == 1,1,
                                         ifelse(ano == 2015 & ano_2014_conf == 0 & Confianca == 1,1,
                                                ifelse(ano == 2016 & ano_2015_conf == 0 & Confianca == 1,1,
                                                       ifelse(ano == 2017 & ano_2016_conf == 0 & Confianca == 1,1,
                                                              ifelse(ano == 2018 & ano_2017_conf == 0 & Confianca == 1,1,
                                                                     ifelse(ano == 2019 & ano_2018_conf == 0 & Confianca == 1,1,
                                                                            ifelse(ano == 2020 & ano_2019_conf == 0 & Confianca == 1,1,0)))))))))


servidores_ano <- servidores_ano %>%
  mutate(contratado_outro = ifelse(ano==2013 & Outros == 1 & duracao_mes < 12,1,
                                  ifelse(ano == 2014 & ano_2013_outro == 0 & Outros == 1,1,
                                         ifelse(ano == 2015 & ano_2014_outro == 0 & Outros == 1,1,
                                                ifelse(ano == 2016 & ano_2015_outro == 0 & Outros == 1,1,
                                                       ifelse(ano == 2017 & ano_2016_outro == 0 & Outros == 1,1,
                                                              ifelse(ano == 2018 & ano_2017_outro == 0 & Outros == 1,1,
                                                                     ifelse(ano == 2019 & ano_2018_outro == 0 & Outros == 1,1,
                                                                            ifelse(ano == 2020 & ano_2019_outro == 0 & Outros == 1,1,0)))))))))


contratados_tipo <- servidores_ano %>%
  group_by(ano) %>%
  summarise(contratado_serv = sum(contratado_serv),
            desligado_serv = sum(desligado_serv),
            contratado_conf = sum(contratado_conf),
            desligado_conf = sum(desligado_conf),
            contratado_outro = sum(contratado_outro),
            desligado_outro = sum(desligado_outro)) %>%
  mutate(liq_serv = contratado_serv - desligado_serv,
         liq_conf = contratado_conf - desligado_conf,
         liq_outro = contratado_outro - desligado_outro) %>%
  pivot_longer(names_to = "variable",values_to = "value",
               cols = c(liq_serv,liq_conf,liq_outro,
                        contratado_serv,desligado_serv,
                        contratado_conf,desligado_conf,
                        contratado_outro,desligado_outro))


# nesse grafico talvez valha a pena acrescentar no filtro q a pessoa esta desempregada no 
# setor publico, pq aq eh possivel q ela tenha perdido cargo de confianca mas ainda esteja
# como servidora ou outra.

g6 <- contratados_tipo %>%
  filter(variable %in% c("liq_serv", "liq_conf", "liq_outro")) %>%
  ggplot(aes(x=ano,y=value)) + 
  geom_line(aes(group = variable, linetype = variable)) +
  scale_linetype_manual(values = c("longdash","dotted","solid"), name = "Net Hirings",
                        labels = c("Appointed", "Other", "Career")) +
  geom_point(size = 0.5) +
  #  geom_text(aes(label = num_empreg),vjust = -0.8)+
  labs(y = "Number of Public Workers", x= "Year") +
  scale_y_continuous(labels = comma, breaks = scales::breaks_pretty(n = 5)) +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(face = "bold"))   +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_vline(xintercept=c(2014,2016,2018), linetype="solid", color = "grey",
             alpha = 0.5, size = 4) #+
#geom_vline(xintercept=c(2016,2020), linetype="solid", color = "grey",
#           alpha = 0.2, size = 4)


save(g5,g6, file = "descritivas_fluxos.RData")
save(contratados,contratados_tipo, file = "base_descritivas_fluxos.RData")

setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Input")
saveRDS(servidores_ano, file="base_servidored_painel.rds")
setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Output")

rm(contratados,contratados_tipo,g5,g6,servidores_wide,dupl,ids)

####----- Criando variavel de rotatividade-----####

setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Input")
servidores_ano <- readRDS(file="base_servidored_painel.rds")
load(file = "unicos_2013.RData")
setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Output")

# removendo os ids que nao tinham data de ingresso para computar rotatividade

unicos_2013 <- unique(unicos_2013$id_servidor)

aux <- servidores_ano %>%
  filter(id_servidor %nin% unicos_2013)

# criando a base com a rotatividade

rotatividade <-tibble(ano = 2013:2019,
                      hazard_12 = NA,
                      hazard_24 = NA,
                      hazard_48 = NA,
                      total_12 = NA,
                      total_24 = NA,
                      total_48 = NA)

for (anos in 2014:2020) {
  
  rotatividade$total_12[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 12) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  rotatividade$total_24[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 24) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  rotatividade$total_48[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 48) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 12) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_12[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 24) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_24[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 48) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_48[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
    
}

rotatividade <- rotatividade %>%
  mutate(hazard_12 = (hazard_12/total_12)*100,
         hazard_24 = (hazard_24/total_24)*100,
         hazard_48 = (hazard_48/total_48)*100)


rotatividade <- rotatividade %>%
  select(ano,hazard_12:hazard_48) %>%
  pivot_longer(cols = starts_with("hazard"),names_to = "variable",values_to = "value")


g7 <- ggplot(rotatividade,aes(x=ano,y=value)) + 
  geom_line(aes(group = variable, linetype = variable)) +
  scale_linetype_manual(values = c("solid","dotted","longdash"), name = "Duration",
                        labels = c("12 Months", "24 Months", "48 Months")) +
  geom_point(size = 0.5) +
  #  geom_text(aes(label = num_empreg),vjust = -0.8)+
  labs(y = "Number of Public Workers", x= "Year") +
  scale_y_continuous(labels = comma, breaks = scales::breaks_pretty(n = 4)) +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(face = "bold"))   +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) + 
  #geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_vline(xintercept=c(2014,2016,2018), linetype="solid", color = "grey",
             alpha = 0.5, size = 4)

save(g7, file ="descritiva_rotatividade.RData")
save(rotatividade, file = "base_descritiva_rotatividade.RData")
rm(g7,anos,colunas,unicos,rotatividade,aux)

### Quebrando por categoria

## Career

aux <- servidores_ano %>%
  filter(id_servidor %nin% unicos_2013)

# criando a base com a rotatividade

rotatividade <-tibble(ano = 2013:2019,
                      hazard_12 = NA,
                      hazard_24 = NA,
                      hazard_48 = NA,
                      total_12 = NA,
                      total_24 = NA,
                      total_48 = NA)

for (anos in 2014:2020) {
  
  rotatividade$total_12[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 12, Servidor == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  rotatividade$total_24[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 24, Servidor == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  rotatividade$total_48[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 48, Servidor == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 12, Servidor == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_12[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 24, Servidor == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_24[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 48, Servidor == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_48[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  
}

rotatividade <- rotatividade %>%
  mutate(hazard_12 = (hazard_12/total_12)*100,
         hazard_24 = (hazard_24/total_24)*100,
         hazard_48 = (hazard_48/total_48)*100)


rotatividade_serv <- rotatividade %>%
  select(ano,hazard_12:hazard_48) %>%
  pivot_longer(cols = starts_with("hazard"),names_to = "variable",values_to = "value")

## Appointed

aux <- servidores_ano %>%
  filter(id_servidor %nin% unicos_2013)

# criando a base com a rotatividade

rotatividade <-tibble(ano = 2013:2019,
                      hazard_12 = NA,
                      hazard_24 = NA,
                      hazard_48 = NA,
                      total_12 = NA,
                      total_24 = NA,
                      total_48 = NA)

for (anos in 2014:2020) {
  
  rotatividade$total_12[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 12, Confianca == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  rotatividade$total_24[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 24, Confianca == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  rotatividade$total_48[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 48, Confianca == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 12, Confianca == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_12[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 24, Confianca == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_24[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 48, Confianca == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_48[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  
}

rotatividade <- rotatividade %>%
  mutate(hazard_12 = (hazard_12/total_12)*100,
         hazard_24 = (hazard_24/total_24)*100,
         hazard_48 = (hazard_48/total_48)*100)


rotatividade_conf <- rotatividade %>%
  select(ano,hazard_12:hazard_48) %>%
  pivot_longer(cols = starts_with("hazard"),names_to = "variable",values_to = "value")

save(rotatividade_conf,rotatividade_serv, file = "base_rotatividade_cat.RData")
rm(rotatividade,rotatividade_conf,rotatividade_serv)


####------ Dando merge na base de filiacao com a dos servidores-----####

setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Input")
amostra <- readRDS("base_filiacao_limpa.rds")
servidores_ano <- readRDS(file="base_servidored_painel.rds")
setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Output")

a <- which(colnames(amostra)=="ano_2013")
colnames(amostra)[a:length(amostra)] <- c("fil_2013","fil_2014","fil_2015","fil_2016",
                                          "fil_2017","fil_2018","fil_2019","fil_2020")

# filtrando a base de filiacao com os nomes dos servidores

nomes <- unique(servidores_ano$nome)

amostra <- amostra %>%
  mutate(nome = toupper(nome)) %>%
  filter(nome %in% nomes)

# retirando nomes duplos das duas bases

duplos_serv <- servidores_ano %>%
  distinct(id_servidor,nome) %>%
  select(id_servidor,nome)

duplos_serv <- duplos_serv[!(duplicated(duplos_serv$nome) |
                             duplicated(duplos_serv$nome, 
                             fromLast = TRUE)), ] %>%
  select(nome) %>%
  pull

amostra <- amostra %>%
  filter(nome %in% duplos_serv)

duplos_am <- amostra %>%
  distinct(titulo_eleitoral,nome) %>%
  select(titulo_eleitoral,nome)

duplos_am <- duplos_am[!(duplicated(duplos_am$nome) |
                         duplicated(duplos_am$nome, 
                         fromLast = TRUE)), ] %>%
  select(nome) %>%
  pull

amostra <- amostra %>%
  filter(nome %in% duplos_am) %>%
  select(nome,sigla_partido,starts_with("fil_"))

servidores_ano <- servidores_ano %>%
  left_join(amostra)

rm(amostra)

setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Input")
saveRDS(servidores_ano, file = "bases_merged.rds")
setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Output")
rm(a,duplos_am,duplos_serv,nomes_retirar,nomes)


####----Descritivas bases merged----####

setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Input")
servidores_ano <- readRDS(file = "bases_merged.rds")
load(file = "unicos_2013.RData")
unicos_2013 <- unique(unicos_2013$id_servidor)
setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Output")

# criando dummy para partido que esta no poder (ideal seria coligacao)

servidores_ano <- servidores_ano %>%
  mutate(poder = ifelse(ano %in% c(2013,2014,2015) & sigla_partido == "PT",1,
                        ifelse(ano %in% c(2016,2017,2018) & sigla_partido == "MDB",1,
                               ifelse(ano %in% c(2019,2020) & sigla_partido == "PSL",1,0))))

# criando dummy para filiacao em cada ano

servidores_ano <- servidores_ano %>%
  mutate(filiado = ifelse(ano == 2013 & fil_2013 == 1,1,
                          ifelse(ano == 2014 & fil_2014 == 1, 1,
                                 ifelse(ano == 2015 & fil_2015 == 1, 1,
                                        ifelse(ano == 2016 & fil_2016,1,
                                               ifelse(ano == 2017 & fil_2017 == 1,1,
                                                      ifelse(ano == 2018 & fil_2018 == 1,1,
                                                             ifelse(ano == 2019 & fil_2019 == 1,1,
                                                                    ifelse(ano == 2020 & fil_2020 == 1,1,0)))))))))


servidores_ano <- servidores_ano %>%
  mutate(across(fil_2013:filiado,~ifelse(is.na(.x),0,.x)))

# pegando quantos servidores publicos eram filiados por ano

filiados_ano <- servidores_ano %>%
  filter(empregado == 1) %>%
  group_by(ano) %>%
  summarise(filiado = sum(filiado))

save(filiados_ano, file = "base_filiados_servidore_totais.RData")

rm(filiados_ano)

## pegando qual a proporcao por tipo 

porc_total <- servidores_ano %>%
  filter(empregado == 1) %>%
  mutate(n = 1) %>%
  group_by(ano) %>%
  summarise(total = (sum(filiado)/sum(n))*100)

porc_serv <- servidores_ano %>%
  mutate(n = 1) %>%
  filter(Servidor == 1) %>%
  group_by(ano) %>%
  summarise(servidores = (sum(filiado)/sum(n))*100)

porc_conf <- servidores_ano %>%
  mutate(n = 1) %>%
  filter(Confianca == 1) %>%
  group_by(ano) %>%
  summarise(confianca = (sum(filiado)/sum(n))*100)

porc_outros <- servidores_ano %>%
  mutate(n = 1) %>%
  filter(Outros == 1) %>%
  group_by(ano) %>%
  summarise(outros = (sum(filiado)/sum(n))*100)

# dando merge em todas as dfs de tipos

filiados_tipo <- list(porc_total,porc_serv,porc_conf,porc_outros)

filiados_tipo <- filiados_tipo %>%
  reduce(full_join, by = "ano")

save(filiados_tipo, file = "base_filiados_tipo.RData")

rm(porc_conf,porc_serv,porc_outros,porc_total, filiados_tipo)

## Vendo Net Hirings para filiados

contratados_filiados <- servidores_ano %>%
  filter(filiado == 1) %>%
  group_by(ano) %>%
  summarise(contratado_serv = sum(contratado_serv),
            desligado_serv = sum(desligado_serv),
            contratado_conf = sum(contratado_conf),
            desligado_conf = sum(desligado_conf),
            contratado_total = sum(contratado),
            desligado_total = sum(desligado)) %>%
  mutate(liq_serv = contratado_serv - desligado_serv,
         liq_conf = contratado_conf - desligado_conf,
         liq_total = contratado_total - desligado_total) %>%
  pivot_longer(names_to = "variable",values_to = "value",
               cols = c(liq_serv,liq_conf,liq_total,
                        contratado_serv,desligado_serv,
                        contratado_conf,desligado_conf,
                        contratado_total,desligado_total))

save(contratados_filiados, file = "base_contratados_filiados.RData")

### Calculando turnover para filiados

aux <- servidores_ano %>%
  filter(id_servidor %nin% unicos_2013)

# criando a base com a rotatividade

rotatividade <-tibble(ano = 2013:2019,
                      hazard_12 = NA,
                      hazard_24 = NA,
                      hazard_48 = NA,
                      total_12 = NA,
                      total_24 = NA,
                      total_48 = NA)

for (anos in 2014:2020) {
  
  rotatividade$total_12[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 12, filiado == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  rotatividade$total_24[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 24, filiado == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  rotatividade$total_48[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 48, filiado == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 12, filiado == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_12[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 24, filiado == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_24[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 48, filiado == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_48[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  
}

rotatividade <- rotatividade %>%
  mutate(hazard_12 = (hazard_12/total_12)*100,
         hazard_24 = (hazard_24/total_24)*100,
         hazard_48 = (hazard_48/total_48)*100)


rotatividade <- rotatividade %>%
  select(ano,hazard_12:hazard_48) %>%
  pivot_longer(cols = starts_with("hazard"),names_to = "variable",values_to = "value")

rotatividade_merg <- rotatividade

save(rotatividade_merg, file = "base_rotatividade_merg.RData")
rm(rotatividade,rotatividade_merg)

### Turnover para quem era do poder

aux <- servidores_ano %>%
  filter(id_servidor %nin% unicos_2013)

# criando a base com a rotatividade

rotatividade <-tibble(ano = 2013:2019,
                      hazard_12 = NA,
                      hazard_24 = NA,
                      hazard_48 = NA,
                      total_12 = NA,
                      total_24 = NA,
                      total_48 = NA)

for (anos in 2014:2020) {
  
  rotatividade$total_12[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 12, poder == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  rotatividade$total_24[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 24, poder == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  rotatividade$total_48[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 48, poder == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 12, poder == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_12[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 24, poder == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_24[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 48, poder == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_48[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  
}

rotatividade <- rotatividade %>%
  mutate(hazard_12 = (hazard_12/total_12)*100,
         hazard_24 = (hazard_24/total_24)*100,
         hazard_48 = (hazard_48/total_48)*100)


rotatividade <- rotatividade %>%
  select(ano,hazard_12:hazard_48) %>%
  pivot_longer(cols = starts_with("hazard"),names_to = "variable",values_to = "value")

rotatividade_poder <- rotatividade

save(rotatividade_poder, file = "base_rotatividade_poder.RData")
rm(rotatividade,rotatividade_poder)


# Quebrando por categoria

## Career

aux <- servidores_ano %>%
  filter(id_servidor %nin% unicos_2013)

# criando a base com a rotatividade

rotatividade <-tibble(ano = 2013:2019,
                      hazard_12 = NA,
                      hazard_24 = NA,
                      hazard_48 = NA,
                      total_12 = NA,
                      total_24 = NA,
                      total_48 = NA)

for (anos in 2014:2020) {
  
  rotatividade$total_12[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 12, poder == 1, Servidor == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  rotatividade$total_24[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 24, poder == 1, Servidor == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  rotatividade$total_48[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 48, poder == 1, Servidor == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 12, poder == 1, Servidor == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_12[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 24, poder == 1, Servidor == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_24[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 48, poder == 1, Servidor == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_48[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  
}

rotatividade <- rotatividade %>%
  mutate(hazard_12 = (hazard_12/total_12)*100,
         hazard_24 = (hazard_24/total_24)*100,
         hazard_48 = (hazard_48/total_48)*100)


rotatividade <- rotatividade %>%
  select(ano,hazard_12:hazard_48) %>%
  pivot_longer(cols = starts_with("hazard"),names_to = "variable",values_to = "value")

rotatividade_poder_serv <- rotatividade
rm(rotatividade)

## Appointed

aux <- servidores_ano %>%
  filter(id_servidor %nin% unicos_2013)

# criando a base com a rotatividade

rotatividade <-tibble(ano = 2013:2019,
                      hazard_12 = NA,
                      hazard_24 = NA,
                      hazard_48 = NA,
                      total_12 = NA,
                      total_24 = NA,
                      total_48 = NA)

for (anos in 2014:2020) {
  
  rotatividade$total_12[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 12, poder == 1, Confianca == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  rotatividade$total_24[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 24, poder == 1, Confianca == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  rotatividade$total_48[anos - 2013] <- servidores_ano %>%
    filter(ano == anos - 1, duracao_mes == 48, poder == 1, Confianca == 1) %>%
    summarise(n = sum(empregado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 12, poder == 1, Confianca == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_12[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 24, poder == 1, Confianca == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_24[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  ids <- aux %>%
    filter(ano == anos - 1, duracao_mes == 48, poder == 1, Confianca == 1) %>%
    select(id_servidor) %>% 
    pull
  
  rotatividade$hazard_48[anos - 2013] <- servidores_ano %>%
    filter(id_servidor %in% ids, ano == anos) %>%
    summarise(n = sum(desligado)) %>%
    pull
  
  
}

rotatividade <- rotatividade %>%
  mutate(hazard_12 = (hazard_12/total_12)*100,
         hazard_24 = (hazard_24/total_24)*100,
         hazard_48 = (hazard_48/total_48)*100)


rotatividade <- rotatividade %>%
  select(ano,hazard_12:hazard_48) %>%
  pivot_longer(cols = starts_with("hazard"),names_to = "variable",values_to = "value")

rotatividade_poder_conf <- rotatividade

save(rotatividade_poder_serv,rotatividade_poder_conf, file = "base_rotatividade_poder_cat.RData")
rm(rotatividade,rotatividade_poder_conf,rotatividade_poder_serv)

## Regressao

servidores_regressao <- servidores_ano %>% 
  filter(id_servidor %nin% unicos_2013) %>% #tirando os que nao tem data de ingresso 
  mutate(mudanca_poder = ifelse(ano %in% c(2015,2017,2019),1,0))

saveRDS(servidores_regressao, file = "servidores_regressao.rds")

setwd("/Users/bernardoduque/Documents/Puc/Trabalho II/Trabalho Final/Output")
servidores_regressao <- readRDS(file = "servidores_regressao.rds")

modelo_1 <- plm(desligado ~ lag(Servidor,1) + lag(Confianca,1) + lag(Outros,1) + lag(duracao_mes,1) + poder + filiado,
                data = servidores_regressao, index = c("id_servidor","ano"),
                model = "pooling")
modelo_2 <- plm(desligado ~ lag(Servidor,1) + lag(Confianca,1) + lag(Outros,1) + lag(duracao_mes,1) + poder + filiado, 
                  data = servidores_regressao, index = c("id_servidor","ano"))
modelo_3 <- plm(desligado ~ lag(Servidor,1) + lag(Confianca,1) + lag(Outros,1) + lag(duracao_mes,1) + poder + filiado + poder*lag(Servidor,1) + poder*lag(Confianca,1) + lag(poder,1)*mudanca_poder, 
                effect = "twoways",
                data = servidores_regressao, index = c("id_servidor","ano"))

modelo_4 <- plm(contratado ~ Servidor + Confianca + poder , 
                data = servidores_regressao, index = c("id_servidor","ano"))
modelo_5 <- plm(contratado ~ Servidor + Confianca + poder + filiado, 
                data= servidores_regressao, index = c("id_servidor","ano"))

modelos <- list()

for(i in 1:5){
  modelos[[i]] <- get(paste0("modelo_",i))
}

save(modelo_1,modelo_2,modelo_3, modelo_4, modelo_5,file = "modelos_pt_1.RData")
rm(modelo_1,modelo_2,modelo_3,modelo_4, modelo_5,servidores_ano)

modelo_6 <- plm(contratado ~ Servidor + Confianca  + poder + filiado + mudanca_poder*poder, 
                effect = "twoways", 
                data = servidores_regressao, index = c("id_servidor","ano"))

modelos[[6]] <- modelo_6

save(modelo_6,file = "modelos_pt_2.RData")
rm(modelo_6)

save(modelos,file = "modelos.RData")

# copiar e colar output no console no latex pra tabela sair direito
# texreg direto e stargazer tao bugados no markdown, por isso copiar e colar

library(texreg)

texreg(list(modelo_1,modelo_2,modelo_3))

texreg(modelos,custom.header = list("Dismissals" = 1:3, "Hirings" = 4:6),
       custom.coef.map = list("lag(Servidor, 1)"="Lag(Career)","lag(Confianca, 1)" ="Lag(Appointed)",
                              "lag(Outros,1)"="Lag(Other)","lag(duracao_mes, 1)" ="Lag(Duration)",
                              "poder" ="Power","filiado" ="Affiliation", 
                              "mudanca_poder:lag(poder,1):lag(Servidor,1)" = "Lag(Career*Power)*GC",
                              "mudanca_poder:lag(poder,1):lag(Confianca,1)" = "Lag(Appointed*Power)*GC",
                              "Servidor" ="Career","Confianca" = "Appointed"),
       custom.gof.rows = list("Individual FE" = c("No","Yes", "Yes", "No", "Yes", "Yes"),
                              "Time FE" = c("No", "No", "Yes", "No", "No", "Yes")),
       digits = 3, caption = "Variation of Public Job Type and Affiliation on Dismissals and Hirings",
       caption.above = T, label = "reg", booktabs = T,longtable = T,
       custom.model.names	= c("(1)","(2)", "(3)", "(4)", "(5)", "(6)"),
       fontsize = "small")

