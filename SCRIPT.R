####################################################################################################################################
###########################       ANÁLISE DAS OCORRÊNCIAS AERONÁUTICAS NA AVIAÇÃO CIVIL BRASILEIRA       ###########################
####################################################################################################################################

######                   Dados de ocorrências aeronáuticas da aviação civil brasileira nos últimos 10 anos.                   ######
######         Dados disponíveis em https://dados.gov.br/dataset/ocorrencias-aeronauticas-da-aviacao-civil-brasileira         ######

                                                                                                    #Leiliane Oliveira em 22/01/2022
#### PACOTES NECESSÁRIOS ####
if(!require(readr)){install.packages('readr'); require(readr)}
if(!require(tidyverse)){install.packages('tidyverse'); require(tidyverse)}
if(!require(ggplot2)){install.packages('ggplot2'); require(ggplot2)}
if(!require(gtsummary)){install.packages('gtsummary'); require(gtsummary)}
if(!require(RColorBrewer)){install.packages('RColorBrewer'); require(RColorBrewer)}
if(!require(naniar)){install.packages('naniar'); require(naniar)}
if(!require(AER)){install.packages('AER'); require(AER)}

#### LIMPANDO OS OBJETOS DA MEMÓRIA  ####
rm(list=ls()) 

#### BASE DE DADOS DE OCORRÊNCIA        ##############
ocorrencia <- read_csv2('ocorrencia.csv') #Importando os dados
View(ocorrencia) #Não foi preciso informar o encoding porque o default do sistema operacional é compatível com o do arquivo.

#Verificando se as chaves são diferentes
table(ocorrencia$codigo_ocorrencia==ocorrencia$codigo_ocorrencia1)
table(ocorrencia$codigo_ocorrencia==ocorrencia$codigo_ocorrencia2)
table(ocorrencia$codigo_ocorrencia==ocorrencia$codigo_ocorrencia3)
table(ocorrencia$codigo_ocorrencia==ocorrencia$codigo_ocorrencia4) #São todas iguais, tem nomes diferentes apenas para unir às tabelas abaixo

#Verificando se há chaves duplicadas
length(unique(ocorrencia$codigo_ocorrencia))==nrow(ocorrencia)

########## Estatística Descritiva####
#Classificação das Ocorrências
prop.table(sort(table(ocorrencia$ocorrencia_classificacao),decreasing = FALSE)) 
Tab<-sort(table(ocorrencia$ocorrencia_classificacao),decreasing = FALSE)
G<-ggplot(data.frame(Tab), aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição das ocorrências",
       x = "Categoria",
       y = "Número de Aeronaves")+
  coord_flip()  # Acidentes representam 31,57% das ocorrências nos últimos 10 anos
G
ggsave("Distribuição das ocorrências.pdf", G)
rm(list=c("Tab","G"))

#País das ocorrências
prop.table(table(ocorrencia$ocorrencia_pais))

#Cidades das Ocorrências
prop.table(table(ocorrencia$ocorrencia_cidade))
length(unique(ocorrencia$ocorrencia_cidade)) #Foram registradas ocorrências em 1133 cidades brasileiras

#Estados das Ocorrências
prop.table(sort(table(ocorrencia$ocorrencia_uf),decreasing = FALSE)) 
Tab<-sort(table(ocorrencia$ocorrencia_uf),decreasing = FALSE)
G<-ggplot(data.frame(Tab), aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição das ocorrências Por Estado",
       x = "Estado",
       y = "Número de Aeronaves")+
  coord_flip()  # Acidentes representam 31,57% das ocorrências nos últimos 10 anos
G
ggsave("Distribuição das ocorrências Por Estado.pdf", G)
rm(list=c("Tab","G"))

#Aerodromo das Ocorrências
sort(prop.table(table(ocorrencia$ocorrencia_aerodromo)),decreasing = TRUE) #Cerca de 40% dos registros não tem informação do aerodromo
length(unique(ocorrencia$ocorrencia_aerodromo)) #Foram registradas ocorrências em 529=531-2 aerodromo.

#Demais Variáveis
prop.table(table(ocorrencia$investigacao_aeronave_liberada)) 
# Em apenas 0,49% das ocorrências as aeronaves não foram liberadas após a investigação, e em 34,56% das ocorrências não há registro
prop.table(table(ocorrencia$investigacao_status)) #86,85% das ocorrências tem status de investigação igual "finalizada"
prop.table(table(ocorrencia$divulgacao_relatorio_publicado)) #74,26% das ocorrências não tiveram divulgação do relatório publicado
prop.table(table(ocorrencia$total_recomendacoes)) #87,35% das ocorrências não tem nenhuma recomendação
prop.table(table(ocorrencia$total_aeronaves_envolvidas)) #98,84% das ocorrências aconteceram com apenas uma aeronave
prop.table(table(ocorrencia$ocorrencia_saida_pista)) # 91,20% das ocorências aconteceram sem saída de pista

########## Tratando as variáveis de Data####
ocorrencia$ocorrencia_data<-ocorrencia$ocorrencia_dia
ocorrencia$ocorrencia_data<- sub(pattern = "/", replacement = "-", x = ocorrencia$ocorrencia_data)
ocorrencia$ocorrencia_data<- sub(pattern = "/", replacement = "-", x = ocorrencia$ocorrencia_data)
table(nchar(x = ocorrencia$ocorrencia_data))
ocorrencia$ocorrencia_data<-strptime(ocorrencia$ocorrencia_data,format="%d-%m-%Y")

#### BASE DE DADOS DE OCORRÊNCIA_TIPO        ##############
ocorrencia_tipo <- read_csv2('ocorrencia_tipo.csv') #Importando os dados
View(ocorrencia_tipo) #Não foi preciso informar o encoding porque o default do sistema operacional é compatível com o do arquivo.

#Verificando se há chaves duplicadas
length(unique(ocorrencia_tipo$codigo_ocorrencia1))==nrow(ocorrencia_tipo) #Há casos duplicados, ATENÇÃO ao cruzar com outra base de dados.
table(sort(table(ocorrencia_tipo$codigo_ocorrencia1),decreasing = TRUE))

Exem <- ocorrencia_tipo %>% #Exemplo de ocorrência duplicada, Aparentemente uma ocorrência pode ser de vários tipos.
  filter(codigo_ocorrencia1=="66444")
View(Exem)
rm(Exem)

#Estatística Descritiva
Tab<- sort(prop.table(table(ocorrencia_tipo$ocorrencia_tipo)),decreasing = TRUE) #Existem 81 tipos de ocorrência
View(Tab)
barplot(Tab[1:10]) # os 10 tipos de ocorência mais frequente nos últimos 10 anos
rm(Tab)

Tab<- sort(prop.table(table(ocorrencia_tipo$ocorrencia_tipo_categoria)),decreasing = TRUE) #Existem 81 tipo_categoria de ocorrência
View(Tab)
barplot(Tab[1:10]) # os 10 tipos de ocorência mais frequente nos últimos 10 anos
rm(Tab)

Tab<- sort(prop.table(table(ocorrencia_tipo$taxonomia_tipo_icao)),decreasing = TRUE) #Existem 32 tipo_icao de ocorrência
View(Tab)
barplot(Tab[1:10]) # os 10 tipos de ocorência mais frequente nos últimos 10 anos
rm(Tab)

#ocorrencia_tipo_categoria possui valores iguais a ocorrencia_tipo, porém em 53% dos registros é diferente, sendo mais detalhado.
prop.table(table(ocorrencia_tipo$ocorrencia_tipo==ocorrencia_tipo$ocorrencia_tipo_categoria))

#### BASE DE DADOS DE AERONAVE        ##############
aeronave <- read_csv2('aeronave.csv') #Importando os dados
View(aeronave) #Não foi preciso informar o encoding porque o default do sistema operacional é compatível com o do arquivo.

#Verificando se há chaves duplicadas
length(unique(aeronave$codigo_ocorrencia2))==nrow(aeronave) #Há casos duplicados ATENÇÃO ao cruzar com outra base de dados.
table(sort(table(aeronave$codigo_ocorrencia2),decreasing = TRUE))

########## Estatística Descritiva das variáveis numéricas####
table(aeronave$aeronave_assentos)
table(aeronave$aeronave_ano_fabricacao)
table(aeronave$aeronave_fatalidades_total)

#Tratamento de NULL das variáveis númericas
aeronave<-aeronave%>%
 mutate(
   aeronave_assentos=case_when(
     aeronave_assentos == 'NULL' ~ NA_character_,
     TRUE ~ aeronave_assentos),
   
   aeronave_assentos=as.numeric(aeronave_assentos),
   
   aeronave_ano_fabricacao=case_when(
     aeronave_ano_fabricacao == 'NULL' ~ NA_character_,
     TRUE ~ aeronave_ano_fabricacao),
   
   aeronave_ano_fabricacao=as.numeric(aeronave_ano_fabricacao),
   
   aeronave_ano_fabricacao = case_when(
     aeronave_ano_fabricacao != 0 ~ aeronave_ano_fabricacao),
   
   aeronave_ano_fabricacao = case_when(
     aeronave_ano_fabricacao != 9999 ~ aeronave_ano_fabricacao))

#Estatísticas
Aux<- aeronave%>%
  select(aeronave_assentos,aeronave_ano_fabricacao,aeronave_fatalidades_total)
summary(Aux)
rm(Aux)

X_barra<- aeronave%>%
  select(aeronave_assentos,aeronave_fatalidades_total)%>%
  tbl_summary(statistic = all_continuous() ~ "{mean}", 
              missing = "no",
              digits = list(all_continuous()~ 2)) %>%
  modify_header(stat_0 ~ "**Média**") %>% add_n()

Desvio_Pad<- aeronave%>%
  select(aeronave_assentos,aeronave_fatalidades_total)%>%
  tbl_summary(statistic = all_continuous() ~ "{sd}", 
              missing = "no",
              digits = list(all_continuous()~ 2)) %>%
  modify_header(stat_0 ~ "**S.D.**") 

tbl_merge(list(X_barra, Desvio_Pad))
rm(list=c("X_barra", "Desvio_Pad"))

#Distribuição de Assentos
G<-ggplot(aeronave,aes(x=aeronave_assentos))+
  geom_histogram(fill='lightblue',color='black',bins=100)+
  ggtitle("Distribuição do Número de Assentos da Aeronave")
G
ggsave("Distribuição do Número de Assentos da Aeronave.pdf", G)
rm("G")

Aux<-aeronave%>%
  filter(aeronave_assentos<21)
G<-ggplot(Aux,aes(x=aeronave_assentos))+
  geom_histogram(fill='lightblue',color='black',bins=10)+
  ggtitle("Distribuição do Número de Assentos da Aeronave, até 20 assentos")  
G
ggsave("Distribuição do Número de Assentos da Aeronave Até 20 assentos.pdf", G)
rm(list=c("Aux","G"))

#Distribuição de Ano de Fabricação
G<-ggplot(aeronave,aes(x=aeronave_ano_fabricacao))+
  geom_histogram(fill='lightblue',color='black',bins=100)+
  ggtitle("Distribuição do Ano de Fabricação")
G
ggsave("Distribuição do Ano de Fabricação.pdf", G)
rm("G")

Aux<-aeronave%>%
  filter(aeronave_ano_fabricacao>1999)
G<-ggplot(Aux,aes(x=aeronave_ano_fabricacao))+
  geom_histogram(fill='lightblue',color='black',bins=10)+
  ggtitle("Distribuição do Ano de Fabricação pós 2000")  
G
ggsave("Distribuição do Ano de Fabricação pós 2000.pdf", G)
rm(list=c("Aux","G"))

#Distribuição da Fatalidade Total
Aux<-aeronave%>%
  filter(aeronave_fatalidades_total>0)
G<-ggplot(Aux,aes(x=aeronave_fatalidades_total))+
  geom_histogram(fill='lightblue',color='black',bins=10)+
  ggtitle("Distribuição da Fatalidade Total")  
G
ggsave("Distribuição da Fatalidade Total.pdf", G)
rm(list=c("Aux","G"))

########## Estatística Descritiva das variáveis categóricas####
#Primeira olhada nos dados (variáveis categóricas)
Aeronave_Descritiva_Categorica<- aeronave %>%
  select(-codigo_ocorrencia2,
         -aeronave_matricula,
         -aeronave_pmd,
         -aeronave_pmd_categoria,
         -aeronave_fatalidades_total,
         -aeronave_ano_fabricacao,
         -aeronave_assentos,
         -aeronave_voo_destino,
         -aeronave_voo_origem,
         -aeronave_tipo_icao,
         -aeronave_fabricante,
         -aeronave_modelo)%>%
  tbl_summary(
    missing =  'no') %>% 
  modify_header(update =list(label~"**Variáveis**")) %>% 
  bold_labels() %>%
  modify_footnote(everything() ~ NA_character_) #Porém como tem muitas categorias, gerar visualizações dos mais frequentes.
Aeronave_Descritiva_Categorica

#Distribuição Operador Categoria
length(unique(aeronave$aeronave_operador_categoria)) #Existem 11 categorias de operador
sort(prop.table(table(aeronave$aeronave_operador_categoria)),decreasing = TRUE) 
Tab<- sort(table(aeronave$aeronave_operador_categoria),decreasing = FALSE) 
G<-ggplot(data.frame(Tab), aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição da Categoria de Operador da Aeronave",
       x = "Categoria",
       y = "Número de Aeronaves")+
  coord_flip()  #51,48% das ocorrências não tem a categoria operador, 14,36% é REGULAR, 11,5% PARTICULAR, 7,75% TÁXI AÉREO
G
ggsave("Distribuição da Categoria de Operador da Aeronave.pdf", G)
rm(list=c("Tab","G"))

#Distribuição Tipo Veículo
length(unique(aeronave$aeronave_tipo_veiculo)) #Existem 10 tipos de veículo 
sort(prop.table(table(aeronave$aeronave_tipo_veiculo)),decreasing = TRUE) 
Tab<- sort(table(aeronave$aeronave_tipo_veiculo),decreasing = FALSE) 
G<-ggplot(data.frame(Tab), aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição da Categoria Tipo de Veículo",
       x = "Categoria",
       y = "Número de Aeronaves")+
  coord_flip()  #80,30% é avião, 11,11% Helicóptero e 5,57% ultraleve
G
ggsave("Distribuição da Categoria Tipo de Veículo.pdf", G)
rm(list=c("Tab","G"))

#Distribuição Fabricante
length(unique(aeronave$aeronave_fabricante)) #Existem 251 tipos de fabricantes
Tab<-sort(prop.table(table(aeronave$aeronave_fabricante)),decreasing = TRUE) 
Tab[1:10]
barplot(Tab[1:10]) #15,51%  é CESSNA AIRCRAFT, 11,15% EMBRAER, 11,06%NEIVA INDUSTRIA AERONAUTICA
rm(Tab)

#Distribuição Modelo
length(unique(aeronave$aeronave_modelo)) #Existem 819 modelo
Tab<-sort(prop.table(table(aeronave$aeronave_modelo)),decreasing = TRUE) 
Tab[1:10]
barplot(Tab[1:10]) #15,51%  é CESSNA AIRCRAFT, 11,15% EMBRAER, 11,06%NEIVA INDUSTRIA AERONAUTICA
rm(Tab)

#Distribuição Tipo Icao
length(unique(aeronave$aeronave_tipo_icao)) #Existem 241 tipos de icao
Tab<-sort(prop.table(table(aeronave$aeronave_tipo_icao)),decreasing = TRUE) 
Tab[1:10]
barplot(Tab[1:10]) # Os 10 tipo de icao mais frequentes
rm(Tab)

#Distribuição Tipo Motor
length(unique(aeronave$aeronave_motor_tipo)) #Existem 7 tipos de motor
sort(prop.table(table(aeronave$aeronave_motor_tipo)),decreasing = TRUE) 
Tab<- sort(table(aeronave$aeronave_motor_tipo),decreasing = FALSE) 
G<-ggplot(data.frame(Tab), aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição da Categoria Tipo de Motor",
       x = "Categoria",
       y = "Número de Aeronaves")+
coord_flip()  #PISTÃO é o tipo de motor com mais ocorrÊncias, 55,24%
G
ggsave("Distribuição da Categoria Tipo de Motor.pdf", G)
rm(list=c("Tab","G")) 

#Distribuição Motor Quantidade
length(unique(aeronave$aeronave_motor_quantidade)) #Existem 6 tipos de motor quantidade
sort(prop.table(table(aeronave$aeronave_motor_quantidade)),decreasing = TRUE) 
Tab<- sort(table(aeronave$aeronave_motor_quantidade),decreasing = FALSE) 
G<-ggplot(data.frame(Tab), aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição da Categoria Motor Quantidade",
       x = "Categoria",
       y = "Número de Aeronaves")+
  coord_flip()   # 50,42% das ocorrências foram com monomotor
G
ggsave("Distribuição da Categoria Motor Quantidade.pdf", G)
rm(list=c("Tab","G"))

#Distribuição Motor Quantidade X Tipo de Motor
G<-ggplot(aeronave,aes(x =aeronave_motor_tipo)) +
  geom_bar(aes(fill = aeronave_motor_quantidade),position = "dodge") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição da Categoria Motor Quantidade por Tipo de Motor",
       x = "Tipo Motor",
       y = "Número de Aeronaves")
G
ggsave("Motor Quantidade X Tipo de Motor.pdf", G)
rm(list=c("Tab","G"))

#Distribuição País Fabricação
length(unique(aeronave$aeronave_pais_fabricante)) #Existem 24 países farbiacantes de aeronave
sort(prop.table(table(aeronave$aeronave_pais_fabricante)),decreasing = TRUE) #98,15% da aeronaves com ocorrência são de fabricantes brasileiros

#Distribuição País de Registro
length(unique(aeronave$aeronave_pais_registro)) #Existem 24 países farbiacantes de aeronave
sort(prop.table(table(aeronave$aeronave_pais_registro)),decreasing = TRUE) #98,15% da aeronaves com ocorrência tem registro no Brasil

#País de Registro é o mesmo de País do Fabricante para todos os casos
table(aeronave$aeronave_pais_registro==aeronave$aeronave_pais_fabricante)

##Distribuição de países fabricantes desconsiderando o Brasil
Tab<- aeronave %>% 
  filter (aeronave_pais_fabricante != "BRASIL")
Tab<- sort(table(Tab$aeronave_pais_fabricante),decreasing = FALSE) 
G<-ggplot(data.frame(Tab), aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição País de Fabricação (-Brasil)",
       x = "Categoria",
       y = "Número de Aeronaves")+
  coord_flip()   
G
ggsave("Distribuição País de Fabricação (-Brasil).pdf", G)
rm(list=c("Tab","G"))

##Distribuição de países de Registro desconsiderando o Brasil
Tab<- aeronave %>% 
  filter (aeronave_pais_registro != "BRASIL")
Tab<- sort(table(Tab$aeronave_pais_registro),decreasing = FALSE) 
G<-ggplot(data.frame(Tab), aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição País de Registro (-Brasil)",
       x = "Categoria",
       y = "Número de Aeronaves")+
  coord_flip()   
G
ggsave("Distribuição País de Registro (-Brasil).pdf", G)
rm(list=c("Tab","G"))

#Distribuição Registro Categoria
length(unique(aeronave$aeronave_registro_categoria)) # Existem 10 categorias de Registro de Categoria
sort(prop.table(table(aeronave$aeronave_registro_categoria)),decreasing = TRUE) #80,30% é Avião, 11,11 Helicóptero e 5,57% ultraleve
Tab<- sort(table(aeronave$aeronave_registro_categoria),decreasing = FALSE) 
G<-ggplot(data.frame(Tab), aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição da Categoria de Registro de Categoria",
       x = "Categoria",
       y = "Número de Aeronaves")+
  coord_flip()   
G
ggsave("Distribuição da Categoria Registro de Categoria.pdf", G)
rm(list=c("Tab","G"))

#Distribuição Registro Segmento
length(unique(aeronave$aeronave_registro_segmento)) #Existem 13 categorias de Registro de Segmento
sort(prop.table(table(aeronave$aeronave_registro_segmento)),decreasing = TRUE) #30,46% é particular, 20,58% regular, 13,89% Instrução
Tab<- sort(table(aeronave$aeronave_registro_segmento),decreasing = FALSE) 
G<-ggplot(data.frame(Tab), aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição da Categoria de Registro de Segmento",
       x = "Categoria",
       y = "Número de Aeronaves")+
  coord_flip()   
G
ggsave("Distribuição da Categoria Registro de Segmento.pdf", G)
rm(list=c("Tab","G"))

#Distribuição Fase Operação
length(unique(aeronave$aeronave_fase_operacao)) #Existem 32 categorias de Fase Operação
sort(prop.table(table(aeronave$aeronave_fase_operacao)),decreasing = TRUE) #Pouso, Decolagem e Cruzeiro são aos fase de operação com mais ocorrÊncias
Tab<- sort(table(aeronave$aeronave_fase_operacao),decreasing = FALSE) 
G<-ggplot(data.frame(Tab), aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição da Categoria de Fase Operação",
       x = "Categoria",
       y = "Número de Aeronaves")+
  coord_flip()   
G
ggsave("Distribuição da Categoria Fase Operação.pdf", G)
rm(list=c("Tab","G"))

#Distribuição Tipo Operação
length(unique(aeronave$aeronave_tipo_operacao)) #Existem 10 categorias de tipo de Operação
sort(prop.table(table(aeronave$aeronave_tipo_operacao)),decreasing = TRUE) #Privada, regular e instrução o são os tipos de operação com mais ocorrências
Tab<- sort(table(aeronave$aeronave_tipo_operacao),decreasing = FALSE) 
G<-ggplot(data.frame(Tab), aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição da Categoria de Tipo de Operação",
       x = "Categoria",
       y = "Número de Aeronaves")+
  coord_flip()   
G
ggsave("Distribuição da Categoria Tipo Operação.pdf", G)
rm(list=c("Tab","G"))

#Distribuição Nível de Dano
length(unique(aeronave$aeronave_nivel_dano)) #Existem 5 categorias de nível de dano
sort(prop.table(table(aeronave$aeronave_nivel_dano)),decreasing = TRUE) 
Tab<- sort(table(aeronave$aeronave_nivel_dano),decreasing = FALSE) 
G<-ggplot(data.frame(Tab), aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição da Categoria de Nível de Dano",
       x = "Categoria",
       y = "Número de Aeronaves")+
  coord_flip()   
G
ggsave("Distribuição da Categoria Nível de Dano.pdf", G)
rm(list=c("Tab","G"))

#Distribuição Dano por tipo de motor
G<-ggplot(aeronave,aes(x =aeronave_nivel_dano)) +
  geom_bar(aes(fill = aeronave_motor_tipo),position = "dodge") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição da Categoria De Dano por Tipo de Motor",
       x = "Dano",
       y = "Número de Aeronaves")
G
ggsave("Motor Dano X Tipo de Motor.pdf", G)
rm(list=c("G"))

#Distribuição Dano por Quantidade motor
G<-ggplot(aeronave,aes(x =aeronave_nivel_dano)) +
  geom_bar(aes(fill = aeronave_motor_quantidade),position = "dodge") +
  scale_fill_brewer(palette = 2, type = "qual")+
  labs(title = "Distribuição da Categoria De Dano por Quantidade de Motor",
       x = "Dano",
       y = "Número de Aeronaves")
G
ggsave("Motor Dano X Quantidade de Motor.pdf", G)
rm(list=c("G"))

#### BASE DE DADOS DE FATOR CONTRIBUINTE        ##############
fator_contribuinte <- read_csv2('fator_contribuinte.csv') #Importando os dados

#Fator Nome
length(unique(fator_contribuinte$fator_nome)) #Existem 73 nomes de Fator
unique(fator_contribuinte$fator_nome)
Tab<- sort(prop.table(table(fator_contribuinte$fator_nome)),decreasing = TRUE)
Tab[1:10]
barplot(Tab[1:10]) # os 10 tipos de fator contribuinte mais frequente
rm(Tab)

#Fator Aspecto
length(unique(fator_contribuinte$fator_aspecto)) #Existem 12 tipos de Fator Aspecto
Tab<- sort(prop.table(table(fator_contribuinte$fator_aspecto)),decreasing = TRUE)
Tab
barplot(Tab[1:3])
rm(Tab)

#Fator Condicionante
length(unique(fator_contribuinte$fator_condicionante)) #Existem 7 fatores condicionantes
Tab<- sort(prop.table(table(fator_contribuinte$fator_condicionante)),decreasing = TRUE)
Tab
barplot(Tab[1:3])
rm(Tab)

#Fator area
length(unique(fator_contribuinte$fator_area)) #Existem 5 fatores area
Tab<- sort(prop.table(table(fator_contribuinte$fator_area)),decreasing = TRUE)
Tab
barplot(Tab[1:3])
rm(Tab)

#### BASE DE DADOS DE RECOMENDAÇÃO        ##############
recomendacao <- read_csv2('recomendacao.csv') #Importando os dados

#Verificando o número de diferentes recomendações da base
length(unique(recomendacao$recomendacao_conteudo)) #Há casos Duplicados, mais de uma recomedação por ocorrência

#Identificação dos destinários 
length(unique(recomendacao$recomendacao_destinatario_sigla))
Tab<- sort(prop.table(table(recomendacao$recomendacao_destinatario_sigla)),decreasing = TRUE)
Tab
Tab[1:10]
barplot(Tab[1:10])
rm(Tab)

#Identificando os status das recomendações
length(unique(recomendacao$recomendacao_status))
Tab<- sort(prop.table(table(recomendacao$recomendacao_status)),decreasing = TRUE)
Tab
barplot(Tab)
rm(Tab)

########## Tempo Médio de Resposta de uma recomendação ####

recomendacao$recomendacao_dia_assinatura<-strptime(recomendacao$recomendacao_dia_assinatura,format="%Y-%m-%d")
recomendacao$recomendacao_dia_feedback<-strptime(recomendacao$recomendacao_dia_feedback,format="%Y-%m-%d")
TR=data.frame(as.numeric(difftime(recomendacao$recomendacao_dia_feedback,recomendacao$recomendacao_dia_assinatura)/30)) #Tempo de Resposta em meses
names(TR)<-c("Tempo_Meses")
TR<-TR%>%
  filter(TR>0)

summary(TR) #Em média há uma demora de um ano para se ter uma resposta de recomendação

#Distribuição do Tempo de Resposta
G<-ggplot(TR,aes(x=Tempo_Meses))+
  geom_histogram(fill='lightblue',color='black',bins=20)+
  ggtitle("Distribuição do Tempo de Resposta de uma recomendação")
G
ggsave("Distribuição do Tempo de Resposta de uma recomendação.pdf", G)
rm("G")

########## Número Total de Recomendação Por OcorrÊncia ####
total_recomendacao <-  recomendacao %>% 
  filter(recomendacao_status!="NÃO CUMPRIDA")%>%
  filter(recomendacao_status!="***")%>%
  group_by(codigo_ocorrencia4) %>% 
  summarise(total_recomendacao = n())%>%
    select(codigo_ocorrencia4,total_recomendacao)   

#Distribuição do Total de Recomendação
G<-ggplot(total_recomendacao,aes(x=total_recomendacao))+
  geom_histogram(fill='lightblue',color='black',bins=20)+
  ggtitle("Distribuição do Total de Recomendação")
G
ggsave("Distribuição do Total de Rcomendação.pdf", G)
rm("G")  

#### MODELO ESTATÍSTICO ####
########## Juntando as bases de Dados ####
dados <- aeronave%>%
  transmute(
    codigo_ocorrencia = codigo_ocorrencia2,
    
    aeronave_operador_categoria = case_when(
      aeronave_operador_categoria == '***' ~ NA_character_,
      TRUE ~ aeronave_operador_categoria),
    
    aeronave_operador_categoria = as.factor(fct_lump(aeronave_operador_categoria, 
                                           3, 
                                           other_level = 'Outros')),
    
    aeronave_tipo_veiculo = case_when(
      aeronave_tipo_veiculo == '***' ~ NA_character_,
      TRUE ~ aeronave_tipo_veiculo),
    
    aeronave_tipo_veiculo = as.factor(fct_lump(aeronave_tipo_veiculo, 
                                     3, 
                                     other_level = 'Outros')),
    
    aeronave_motor_tipo = as.factor(case_when(
      aeronave_motor_tipo == '***' ~ NA_character_,
      TRUE ~ aeronave_motor_tipo)),
    
    aeronave_motor_quantidade = case_when(
      aeronave_motor_quantidade == '***' ~ NA_character_,
      TRUE ~ aeronave_motor_quantidade),
    
    aeronave_motor_quantidade = as.factor(fct_lump(aeronave_motor_quantidade, 
                                         2, 
                                         other_level = 'Outros')),
    
    aeronave_pais_fabricante = as.factor(case_when(
      aeronave_pais_fabricante == '***' ~ NA_character_,
      TRUE ~ aeronave_pais_fabricante)),
    
    aeronave_pais_fabricante = as.factor(fct_lump(aeronave_pais_fabricante, 
                                        1, 
                                        other_level = 'Outros')),
    
    aeronave_registro_segmento = case_when(
      aeronave_registro_segmento == '***' ~ NA_character_,
      TRUE ~ aeronave_registro_segmento),
    
    aeronave_registro_segmento = as.factor(fct_lump(aeronave_registro_segmento, 
                                          6, 
                                          other_level = 'Outros')),
    
    aeronave_fase_operacao = case_when(
      aeronave_fase_operacao == '***' ~ NA_character_,
      TRUE ~ aeronave_fase_operacao),
    
    aeronave_fase_operacao = as.factor(fct_lump(aeronave_fase_operacao, 
                                      9, 
                                      other_level = 'Outros')),
    
    aeronave_tipo_operacao = case_when(
      aeronave_tipo_operacao == '***' ~ NA_character_,
      TRUE ~ aeronave_tipo_operacao),
    
    aeronave_tipo_operacao = as.factor(fct_lump(aeronave_tipo_operacao, 
                                      5, 
                                      other_level = 'Outros')),
    
    aeronave_nivel_dano = as.factor(case_when(
      aeronave_nivel_dano == '***' ~ NA_character_,
      TRUE ~ aeronave_nivel_dano)),
    
    aeronave_fatalidades_total = as.numeric(aeronave_fatalidades_total),
    
    aeronave_assentos=as.numeric(aeronave_assentos),
    
    aeronave_ano_fabricacao=as.numeric(aeronave_ano_fabricacao))%>%
  
  left_join(ocorrencia%>%
              transmute(codigo_ocorrencia,
                        
                        ocorrencia_classificacao=as.factor(ocorrencia_classificacao),
                        
                        ocorrencia_aerodromo = case_when(
                          ocorrencia_aerodromo == '****'|
                            ocorrencia_aerodromo == 'NULL' ~ NA_character_,
                          ocorrencia_aerodromo == '*****'|
                            ocorrencia_aerodromo == 'NULL' ~ NA_character_,
                          TRUE ~ ocorrencia_aerodromo),
                        
                        investigacao_aeronave_liberada = case_when(
                          investigacao_aeronave_liberada == '***'|
                            investigacao_aeronave_liberada == 'NULL' ~ NA_character_,
                          TRUE ~ investigacao_aeronave_liberada),
                        
                        investigacao_status = as.factor(case_when(
                          investigacao_status == 'NULL' ~ NA_character_,
                          TRUE ~ investigacao_status)),
                        
                        divulgacao_relatorio_publicado=as.factor(divulgacao_relatorio_publicado),
                        
                        total_recomendacoes = as.numeric(total_recomendacoes),
                        
                        total_aeronaves_envolvidas = as.numeric(total_aeronaves_envolvidas),
                        
                        ocorrencia_saida_pista=as.factor(ocorrencia_saida_pista)),
            by='codigo_ocorrencia')
  
########## Análise de Dados Faltantes ####

dados.na <- dados %>% 
  sapply(., function(x) sum(is.na(x))) %>% 
  data.frame()

dados.na <- dados.na %>% mutate(variavel = rownames(dados.na),
                                quant_na = dados.na$.,
                                porc = quant_na/(dim(dados)[1])) %>% 
  select(-.) %>% 
  filter(quant_na != 0) %>% 
  arrange(porc)

View(dados.na)

########## Excluindo variáveis com mais de 25% de dados faltantes ####

dados_modelo <- dados %>% 
  select(-investigacao_aeronave_liberada,
         -ocorrencia_aerodromo,
         -aeronave_operador_categoria)#%>%
  #filter(aeronave_fatalidades_total!= 0)

table(is.na(dados_modelo))

dados_modelo<-na.omit(dados_modelo)


table(is.na(dados_modelo))

colnames(dados_modelo)<- c('codigo_ocorrencia',
                           'Tipo_Veículo',
                           'Tipo_Motor',
                           'Qtd_Motor',
                           'País_Fabricante',
                           'Registro_Segmento',
                           'Fase_Operacao',
                           'Tipo_Operacao',
                           'Nivelde_Dano',
                           'Total_Fatalidade',
                           'Número_Assentos',
                           'Ano_Fabricação',
                           'Classificação_ocorrência',
                           'Status_investigação',
                           'Relatório_Divulgado',
                           'Total_Recomendações',
                           'Total_Aeronaves_Envolvidas',
                           'Ocorrência_Saiu_Pista')

########## Relação com Total de Fatalidade ####

plot(dados_modelo[c(2:6,10)])
plot(dados_modelo[7:12])
plot(dados_modelo[c(13:18,10)])


########## Modelo de Poisson para o total Fatalidade ####
full<- glm(Total_Fatalidade ~ .
           -codigo_ocorrencia,
           family = "poisson",data=dados_modelo)

model_step <- step(full, direction = "both")
summary(model_step)

#Teste de Dispersão:
require(AER)
dispersiontest(model_step) #Temos vestígios de super-disperção

#Análise de Variância
anova(model_step, test="Chisq")

## odds
exp(model_step$coefficients)

## avaliacao

#criando a resposta
resultado <- predict(model_step, dados_modelo, type="response")
#juntando no dado original
dados_estimados <- cbind(dados_modelo, resultado)
#criando erro para estimativa
dados_estimados_est <- dados_estimados %>% 
  mutate( Erro_model = Total_Fatalidade - resultado,
          Erro_media = Total_Fatalidade - mean(Total_Fatalidade))

#RME
rme_model <- sum(dados_estimados_est$Erro_model^2)
rme_model
#e se fosse a média?
rme_mean <- sum(dados_estimados_est$Erro_media^2)
rme_mean
