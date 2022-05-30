
################## SCRIPT PARA ANÁLISE DOS DADOS DA PNS 2019 ###################


##### Limpando arquivos armazenados na memória ====
rm(list=ls(all=TRUE))

# Definindo a opção de visualização de números sem exponencial
aviso <- getOption("warn")
options(warn = -1)
options(scipen = 999)
options(warn=aviso)
rm(aviso)
 


#####=== Importanto Bibliotecas & Criando o Objeto "survey design" =============

library(PNSIBGE)
library(survey)


# Os argumentos "vars" são as variáveis que desejamos importar/ "labels" são os rótulo das variáveis/ "design" necessário para criar o design do objeto.
variaveis_selecionadas <- c("Q084", "C006", "C008", "C009", "C011", "I00102", "J001", "J007", "J008","N001", "Q00201", "Q03001", "Q085", "Q079", "Q087", "Q088", "Q092", "Q11006", "Q11604","Q08601", "Q08603", "Q08604", "Q08605", "P034", "P035", "P036", "P038", "P039", "P044","P03702", "V0026", "VDD004A", "VDF003", "Q088", "P00103", "P00403", "P04501", "P04502", "W00103","W00203","VDF004")

# usar esta opção de variáveis qdo for usar para antropometria:
# variaveis_antropometria <- c("Q084","W00103", "W00203", "C006","C008")



##### IMC ====

# Carregando e criando variavel de IMC referido nos microdados da PNS 2019 para o modulo de morador selecionado

pns2019_moradorselecionado <- get_pns(year=2019, selected=TRUE, vars = variaveis_selecionadas)

pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, IMC_referido=ifelse((P00104>=1&P00104<=599)|(P00404>=1&P00404<=299),P00104/((P00404/100)*(P00404/100)),NA))

pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, fator_IMC_referido=as.factor(ifelse(IMC_referido<=18.4,"Abaixo do peso",ifelse(IMC_referido>=18.5 & IMC_referido<=24.9,"Peso normal",ifelse(IMC_referido>=25.0 & IMC_referido<=29.9,"Sobrepeso",ifelse(IMC_referido>=30,"Obesidade",NA))))))

pns2019_moradorselecionado$variables$fator_IMC_referido <- factor(pns2019_moradorselecionado$variables$fator_IMC_referido, levels = c("Abaixo do peso","Peso normal","Sobrepeso","Obesidade"))



# Obtendo estimativas de doencas cronicas e doencas cronicas relacionadas ao fator de IMC referido

svymean(x=~Q084, design=subset(pns2019_moradorselecionado, C008>=18), na.rm=TRUE)

svytotal(x=~Q084, design=subset(pns2019_moradorselecionado, C008>=18), na.rm=TRUE)

svyby(formula=~Q084, by=~fator_IMC_referido+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, keep.names=FALSE, na.rm=TRUE)



##### Carregando e criando variavel de IMC aferido nos microdados da PNS 2019 para o modulo de antropometria ====
# fonte do valor de referência para as classificações do IMC:  https://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/index.html#InterpretedAdults acesso em 06/08/2021

pns2019_antropometria <- get_pns(year=2019, anthropometry=TRUE, vars = variaveis_antropometria)

pns2019_antropometria$variables <- transform(pns2019_antropometria$variables, IMC_aferido=ifelse((W00103>=20&W00103<=200)|(W00203>=110&W00203<=210),W00103/((W00203/100)*(W00203/100)),NA))

pns2019_antropometria$variables <- transform(pns2019_antropometria$variables, fator_IMC_aferido=as.factor(ifelse(IMC_aferido<=18.4,"Abaixo do peso",ifelse(IMC_aferido>=18.5 & IMC_aferido<=24.9,"Peso normal",ifelse(IMC_aferido>=25.0 & IMC_aferido<=29.9,"Sobrepeso",ifelse(IMC_aferido>=30,"Obesidade",NA))))))

pns2019_antropometria$variables$fator_IMC_aferido <- factor(pns2019_antropometria$variables$fator_IMC_aferido, levels = c("Abaixo do peso","Peso normal","Sobrepeso","Obesidade"))



# Obtendo estimativa de doencas cronicas relacionadas ao fator de IMC aferido

svyby(formula=~Q084, by=~fator_IMC_aferido+C006, design=subset(pns2019_antropometria, C008>=18), FUN=svymean, na.rm=TRUE)
confint(svyby(formula=~Q084, by=~fator_IMC_aferido+C006, design=subset(pns2019_antropometria, C008>=18), FUN=svymean, na.rm=TRUE))


#==============================================================================#
#                   Características sócio-demográficas
#==============================================================================#
pns2019_moradorselecionado <- get_pns(year=2019, selected=TRUE, vars = variaveis_selecionadas)

# O "recorte" de >18 é necessário porque a PNS deste ano os dados foram coletados de moradores > 15 anos.
# obtendo o percentual de participantes com PCC
media_brasileiros_dor <- svymean(x=~Q084, design=subset(pns2019_moradorselecionado, C008>=18), na.rm=TRUE)
media_brasileiros_dor
confint(media_brasileiros_dor)

total_brasileiros_dor <- svytotal(x=~Q084, design=subset(pns2019_moradorselecionado, C008>=18), na.rm=TRUE)
total_brasileiros_dor


#####=== Proporção de Homem e Mulher da pop da amostra ====  
dor_propSexo <- svyby(formula=~Q084, by=~C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
dor_propSexo
cv(dor_propSexo)
confint(dor_propSexo)


#####==== Dor por faixa etária e sexo ==== 

# Média de idade dos indivíduos com dor (sem distinção de sexo):
media_idade_dolorosos_geral <- svymean(x=~C008, design=subset(pns2019_moradorselecionado, Q084=="Sim" & C008 >= 18), na.rm=TRUE)
confint(media_idade_dolorosos_geral)
# resultado = 51.184 anos SE = 0.1938; CI(95%) = 50.80439 a 51.56399


# Média de idade dos homens com dor:
media_idade_homens_com_dor <- svymean(x=~C008, design=subset(pns2019_moradorselecionado, Q084=="Sim" & C006 == "Homem" & C008 >= 18), na.rm=TRUE)
media_idade_homens_com_dor
confint(media_idade_homens_com_dor)
# resultado: 50.096 anos SE = 0.2842; CI(95%) = 49.53863 a 50.65267

# Média de idade das mulheres com dor:
media_idade_mulheres_com_dor <- svymean(x=~C008, design=subset(pns2019_moradorselecionado, Q084=="Sim" & C006 == "Mulher" & C008 >= 18), na.rm=TRUE)
media_idade_mulheres_com_dor
confint(media_idade_mulheres_com_dor)
# resultado: 51.902, SE = 0.2516 anos ; CI(95%) = 51.40857 52.39474


# Obs IMPORTANTE: Criar a variável faixa etária depois de realizar as outras análises de prevalência 
# para não transformar a variável idade (quantitativa) para qualitativa ou categórica. 

# Criando a variavel faixa etária: 
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, faixa_etaria=as.factor(ifelse(C008>=18 & C008<=24,"De 18 a 24 anos",ifelse(C008>=25 & C008<=34,"De 25 A 34 anos",ifelse(C008>=35 & C008<=44,"De 35 a 44 anos",ifelse(C008>=45 & C008<=54,"De 45 a 54 anos",ifelse(C008>=55 & C008<=64,"De 55 a 64 anos",ifelse(C008>=65 & C008<=130,"65 anos ou mais",NA))))))))
pns2019_moradorselecionado$variables$faixa_etaria <- factor(pns2019_moradorselecionado$variables$faixa_etaria, levels=c("De 18 a 24 anos","De 25 A 34 anos","De 35 a 44 anos","De 45 a 54 anos","De 55 a 64 anos","65 anos ou mais"))

dor_faixaEtaria <- svyby(formula=~Q084, by=~faixa_etaria + C006, design=subset(pns2019_moradorselecionado, C008 >= 18), FUN=svymean, na.rm=TRUE)
dor_faixaEtaria 
confint(dor_faixaEtaria)
#svyttest(formula=as.numeric(Q084)~C008 + C006, design=subset(pns2019_moradorselecionado, C008>=18)_maior18anos) # p<.01



#####==== Dor por Cor e Raça ====
# Confere c/ artigo
cor_dor <- svyby(formula=~Q084, by=~C009+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
cor_dor
confint(cor_dor)


#####==== Dor por estado civil ==== 
dor_estadoCivil <- svyby(formula=~Q084, by=~C011+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
dor_estadoCivil
cv(dor_estadoCivil)
confint(dor_estadoCivil)
pns2019_moradorselecionado$variables$C011 <- relevel(pns2019_moradorselecionado$variables$C011, ref = "Solteiro(a)")


#####==== Área Rural e Urbana ====
dor_rural_urb<- svyby(formula=~Q084, by=~V0026 + C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
dor_rural_urb
confint(dor_rural_urb)



#####==== Prop dor Coluna por percepção do Estado de Saúde ==== 
# Classificando a condição "Muito boa" como referência
pns2019_moradorselecionado$variables$N001 <- relevel(pns2019_moradorselecionado$variables$N001, ref="Muito boa") 
percepSaude_dor <- svyby(formula=~Q084, by=~N001+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
percepSaude_dor
confint(percepSaude_dor)



#####==== Dor por nivel de instrução ==== 
#obs.: No artigo foi realizada a junção de "categorias para o nivel de instrução
##### Reagrupando os níveis da variável Factor "VDD004A" obs.: confere com o artigo:=

levels(pns2019_moradorselecionado$variables$VDD004A) <- list(
        Analfabeto_EnsinoFundamentalIncompleto = c("Sem instrução","Fundamental incompleto ou equivalente"), 
        FundamentalCompleto_MedioIncompleto =  c("Fundamental completo ou equivalente","Medio incompleto ou equivalente"), 
        MedioCompleto_SuperiorIncompleto = c("Medio completo ou equivalente","Superior incompleto ou equivalente"),
        SuperiorCompleto = c("Superior completo")
)

dor_NivelInstrucao <- svyby(formula = ~Q084, by=~VDD004A+C006, design = subset(pns2019_moradorselecionado, C008>=18), FUN = svymean, na.rm=TRUE)
dor_NivelInstrucao
confint(dor_NivelInstrucao)



#####==== Dor & Atividade Física ====
# Baseado na pergunta da questão "P034": "Nos últimos três meses, o(a) Sr(a) praticou algum tipo de exercício físico ou esporte?"
dor_ativ_fisica <- svyby(formula=~Q084, by=~P034+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
dor_ativ_fisica
confint(dor_ativ_fisica)
# svyttest(formula=as.numeric(Q084)~P034+C006, design=pns2019_moradorselecionado)



#####==== Dor & Ativo/Sedentario ====

# Criando a variável ativo_sedentario
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, ativo_sedentario=ifelse((P035>=1&P035<=7)|(P03702>1&P03702<=59),(P035*P03702),NA))
# Atribuindo os níveis para a variável ativo_sedentario (Abaixo de 150 minutos/semana = inativo; Acima de 150 min = ativo)

pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, ativo_sedentario = as.factor(ifelse(ativo_sedentario<150,"Sedentario",ifelse(ativo_sedentario>=150,"Ativo",NA))))
# fonte do valor de referência: https://health.gov/sites/default/files/2019-09/Physical_Activity_Guidelines_2nd_edition.pdf   (acesso em 09/agosto/2021) 

# Reordenando os níveis da variável ativo_sedentario (talvez não seja necessário, pois o "Ativo" alfabeticamente é anterior ao "Sedentario":
#pns2019_moradorselecionado$variables$ativo_sedentario <- factor(pns2019_moradorselecionado$variables$ativo_sedentario, levels = c("Ativo","Sedentario"))

dor_ativo_sedentario <- svyby(formula=~Q084, by=~ativo_sedentario + C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
dor_ativo_sedentario
confint(dor_ativo_sedentario)




#####==== Prop indiv. c/ PCC (Patologia Crônica de Coluna) por sexo e Tipo de atividade física ====
# ?????????
dor_TipoExercicio <- svyby(formula=~Q084, by=~P036+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, keep.names = FALSE, na.rm=TRUE)
dor_TipoExercicio



##### Proporção de indivíduos com Dor por Depressão e sexo ====
# Não tem a variável "Depressão" no artigo
dor_depressao <- svyby(formula=~Q084, by=~Q092+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
dor_depressao
confint(dor_depressao)

# ?? svyttest(formula = as.numeric(Q084)~Q092+C006, design = pns2019_moradorselecionado)
# ?? pns2019_moradorselecionado$variables$Q092 <- relevel(pns2019_moradorselecionado$variables$Q092, ref="Não")



######==== Dor & Outras Dçs Mentais  ====
# (esquizofrenia, transtorno bipolar, psicose ou TOC)
dor_doencaMental <- svyby(formula=~Q084, by=~Q11006+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
dor_doencaMental
confint(dor_doencaMental)



######==== Dor & Diabetes ==== #### 
dor_diabetes <- svyby(formula=~Q084, by=~Q03001 + C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
dor_diabetes
confint(dor_diabetes)



######==== DOR & Hipertensão ==== #### 
dor_hipertensao <- svyby(formula=~Q084, by=~Q00201 + C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
dor_hipertensao
confint(dor_hipertensao)



#####==== Dor & DPOC ====
DPOC_dor <- svyby(formula=~Q084, by=~Q11604+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
DPOC_dor
confint(DPOC_dor)



######==== DOR & Artrite_Reumatismo ==== #### 
dor_artrite_reumat <- svyby(formula=~Q084, by=~Q079 + C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
dor_artrite_reumat
confint(dor_artrite_reumat)



#####==== Dor & Dort ====
dort_dor <- svyby(formula=~Q084, by=~Q088+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
dort_dor
confint(dort_dor)


#####====  Dor & Efisema Pulmonar ====
# efisema_dor <- svyby(formula=~Q084, by=~Q11601+C006, design=dadosPNS_morador_selecionado, FUN=svymean, na.rm=TRUE)
# efisema_dor
# confint(efisema_dor)



#####==== Bronquite & Dor ====
# bronquite_dor <- svyby(formula=~Q084, by=~Q11602+C006, design=dadosPNS_morador_selecionado, FUN=svymean, na.rm=TRUE)
# bronquite_dor
# confint(bronquite_dor)



#####==== Dor & Atividade física pesada em CASA ====

#P044 = Nas atividades domésticas, o(a) Sr(a) faz faxina pesada, carrega peso ou faz outra atividade pesada que requer esforço físico intenso?
dor_trab_pesadoCasa <- svyby(formula=~Q084, by=~P044+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
dor_trab_pesadoCasa
confint(dor_trab_pesadoCasa)



#####==== Dor & Atividade Física Pesada Trabalho ====
dor_trab_pesado <- svyby(formula=~Q084, by=~P039+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
dor_trab_pesado
confint(dor_trab_pesado)



#####==== Dor Coluna & Plano de Saúde ==== 
# obs.: Não tem no artigo
planoSaude_dor <- svyby(formula=~Q084, by=~I00102+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
planoSaude_dor
confint(planoSaude_dor)



#####==== Dor Coluna & Dç crônica (fisica ou mental) mais de 6 meses ====
doencaCronica_dor <- svyby(formula=~Q084, by=~J007+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
doencaCronica_dor
confint(doencaCronica_dor)



#####==== Limitação funcional & Dor de coluna ====
# Pergunta da questão "Q087" :
# Em geral, em que grau o problema na coluna limita as suas atividades habituais
# (tais como trabalhar, realizar afazeres domésticos, etc.)?     Obs.: Pergunta específica apenas para quem tem PCC
limitacao_dor <- svyby(formula=~Q087, by=~C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE)
limitacao_dor
confint(limitacao_dor)



#####==== Distribuição da dor segundo a faixa de renda ====
# Faixa de rendimento domiciliar per capita 
# (exclusive o rendimento das pessoas cuja condição na unidade domiciliar era pensionista,
# empregado doméstico ou parente do empregado doméstico)
dor_renda <- svyby(formula=~Q084, by=~VDF004+C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, keep.names=FALSE, na.rm=TRUE)
dor_renda
confint(dor_renda)



#####==== Distribuição da dor segundo o fato de ter ou não plano de saúde ====
# Pergunta: tem algum plano de saúde médico particular, de empresa ou órgão público? Sim    Não
dor_PlanoSaude <- svyby(formula=~Q084, by=~I00102 + C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, keep.names=FALSE, na.rm=TRUE)
dor_PlanoSaude
confint(svyby(formula=~Q084, by=~I00102 + C006, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svymean, na.rm=TRUE))




#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#




#####================= MODELOS DE REGRESSÃO LOGÍSTICA ========================== =========================
# obs.: esses exemplos dos modelos de regressão logística são similares ao do 
# artigo, tanto para o modelo bruto como para o ajustado. 

# Malta DC, Oliveira MM, Andrade SSCA, Caiaffa WT, Souza MFM, Bernal RTI. "Fatores associados à dor crônica na coluna
# em adultos no Brasil." Rev Saude Publica. 2017;51 Supl 1:9s.
#==============================================================================#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Conforme o artigo: "Tendências das desigualdades sociais e demográficas na prevalência de doenças crônicas no Brasil, PNAD: 2003- 2008."
# da autora Marilisa Berti de Azevedo Barros, há uma grande diferença para a PATOLOGIA CRONICA DE COLUNA para as 
# variáveis "NIVEL DE ESCOLARIDADE (VDD004AA) & FAIXA ETÁRIA (C008)"

# IMPORTAAANNNTE:
# Para a "confecção" do modelo de Regressão Logística é importante referenciar, na variável da Patologia Crônica de Coluna, o "NÃO" como nível de referência
pns2019_moradorselecionado$variables$Q084 <- relevel(pns2019_moradorselecionado$variables$Q084, ref = "Não")
pns2019_moradorselecionado$variables$C006 <- relevel(pns2019_moradorselecionado$variables$C006, ref = "Homem")

# Verificar os valores de referência das variáveis escolaridade e nível de instrução. Se não ajustadas anterior/e, fazer isto:
# Ajuste da escolaridade:
levels(dadosPNS_2019$variables$VDD004A) <- list(
        Analfabeto_EnsinoFundamentalIncompleto = c("Sem instrução","Fundamental incompleto ou equivalente"), 
        FundamentalCompleto_MedioIncompleto =  c("Fundamental completo ou equivalente","Medio incompleto ou equivalente"), 
        MedioCompleto_SuperiorIncompleto = c("Medio completo ou equivalente","Superior incompleto ou equivalente"),
        SuperiorCompleto = c("Superior completo")
)

pns2019_moradorselecionado$variables$VDD004A <- relevel(pns2019_moradorselecionado$variables$VDD004A, ref = "Analfabeto_EnsinoFundamentalIncompleto")


# Ajuste da faixa etária: 
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, C008=as.factor(ifelse(C008>=18 & C008<=24,"De 18 a 24 anos",ifelse(C008>=25 & C008<=34,"De 25 A 34 anos",ifelse(C008>=35 & C008<=44,"De 35 a 44 anos",ifelse(C008>=45 & C008<=54,"De 45 a 54 anos",ifelse(C008>=55 & C008<=64,"De 55 a 64 anos",ifelse(C008>=65 & C008<=130,"65 anos ou mais",NA))))))))
pns2019_moradorselecionado$variables$C008 <- factor(pns2019_moradorselecionado$variables$C008, levels=c("De 18 a 24 anos","De 25 A 34 anos","De 35 a 44 anos","De 45 a 54 anos","De 55 a 64 anos","65 anos ou mais"))

# referenciando a faixa etária de 18 a 24 anos como valor de referência:
pns2019_moradorselecionado$variables$C008 <- relevel(pns2019_moradorselecionado$variables$C008, ref = "De 18 a 24 anos")



#####==== Modelo para o Sexo/Gênero
# Modelo sexo BRUTO 
# Homem
modelo_sexo_bruto_mascul <- svyglm(formula=Q084~C006, design=subset(pns2019_moradorselecionado, C008>=18),family="binomial")
modelo_sexo_bruto_mascul
exp(cbind(OR = coef(modelo_sexo_bruto_mascul), confint(modelo_sexo_bruto_mascul)))
# Mulher
modelo_sexo_bruto_fem <- svyglm(formula=Q084~C006, design=subset(pns2019_moradorselecionado, C008>=18),family="binomial")
modelo_sexo_bruto_fem
exp(cbind(OR = coef(modelo_sexo_bruto_fem), confint(modelo_sexo_bruto_fem)))

# Modelo sexo AJUSTADO por escolaridade e faixa etária
# Homem:
modelo_sexo_ajus_mascul <- svyglm(formula=Q084~C006+VDD004A+C008, design=subset(pns2019_moradorselecionado, C006 == "Homem"&C008>=18), family="binomial")
modelo_sexo_ajus_mascul
exp(cbind(OR = coef(modelo_sexo_ajus_mascul), confint(modelo_sexo_ajus_mascul)))
# Mulher:
modelo_sexo_ajus_fem <- svyglm(formula=Q084~C006+VDD004A+C008, design=subset(pns2019_moradorselecionado, C006 == "Mulher"&C008>=18), family="binomial")
modelo_sexo_ajus_fem
exp(cbind(OR = coef(modelo_sexo_ajus_fem), confint(modelo_sexo_ajus_fem)))

# OBS.: subset ??? "não faz sentido" realizar o subset, pois será "perdido" o valor Mulher para 'comparação'.




#####==== Modelo Faixa estária ==== 
# dadosPNS_2019$variables$C008 <- relevel(dadosPNS_2019$variables$C008, ref="De 18 a 24 anos")
# dadosPNS_2019$variables$VDD004AA <- relevel(dadosPNS_2019$variables$VDD004AA, ref="Analfabeto_EnsinoFundamentalIncompleto")
# BRUTO
# Homem
modelo_FaixaEtaria_bruto_masc <- svyglm(formula=Q084~C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
exp(cbind(OR = coef(modelo_FaixaEtaria_bruto_masc), confint(modelo_FaixaEtaria_bruto_masc)))
# Mulher
modelo_FaixaEtaria_bruto_fem <- svyglm(formula=Q084~C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
exp(cbind(OR = coef(modelo_FaixaEtaria_bruto_fem), confint(modelo_FaixaEtaria_bruto_fem)))

# AJUSTADO por escolaridade e faixa etária
# Homem
modelo_FaixaEtaria_ajustado_masc <- svyglm(formula=Q084~C008+VDD004A, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
exp(cbind(OR = coef(modelo_FaixaEtaria_ajustado_masc), confint(modelo_FaixaEtaria_ajustado_masc)))
# Mulher
modelo_FaixaEtaria_ajustado_fem <- svyglm(formula=Q084~C008+VDD004A, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
exp(cbind(OR = coef(modelo_FaixaEtaria_ajustado_fem), confint(modelo_FaixaEtaria_ajustado_fem)))

# Obs.: se desejar o valor de "p" aplicar a fç "summary(modelo_FaixaEtaria_ajustado_masc)"


#####==== Modelo Cor/Raça ====
dadosPNS_2019$variables$C009 <- relevel(dadosPNS_2019$variables$C009, ref = "Branca")
# BRUTO 
# Homem
modelo_cor_bruto_mascul <- svyglm(formula=Q084~C009, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_cor_bruto_mascul
exp(cbind(OR = coef(modelo_cor_bruto_mascul), confint(modelo_cor_bruto_mascul)))
# Mulher
modelo_cor_bruto_fem <- svyglm(formula=Q084~C009, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_cor_bruto_fem
exp(cbind(OR = coef(modelo_cor_bruto_fem), confint(modelo_cor_bruto_fem)))

# AJUSTADO por escolaridade e faixa etária
# Homem:
modelo_cor_ajus_mascul <- svyglm(formula=Q084~C009+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_cor_ajus_mascul
exp(cbind(OR = coef(modelo_cor_ajus_mascul), confint(modelo_cor_ajus_mascul)))
# Mulher:
modelo_cor_ajus_fem <- svyglm(formula=Q084~C009+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_cor_ajus_fem
exp(cbind(OR = coef(modelo_cor_ajus_fem), confint(modelo_cor_ajus_fem)))



#####==== Modelo estado civil ====
dadosPNS_2019$variables$C011 <- relevel(dadosPNS_2019$variables$C011, ref = "Solteiro(a)")
# BRUTO
# Homem
modelo_EstadoCivil_bruto_masc <- svyglm(formula = Q084~C011, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_EstadoCivil_bruto_masc
exp(cbind(OR=coef(modelo_EstadoCivil_bruto_masc), confint(modelo_EstadoCivil_bruto_masc)))
# Mulher
modelo_EstadoCivil_bruto_fem <- svyglm(formula = Q084~C011, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_EstadoCivil_bruto_fem
exp(cbind(OR=coef(modelo_EstadoCivil_bruto_fem), confint(modelo_EstadoCivil_bruto_fem)))

# AJUSTADO escolaridade e faixa etária
# Homem
modelo_EstadoCivil_ajust_masc <- svyglm(formula = Q084~C011+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_EstadoCivil_ajust_masc
exp(cbind(OR=coef(modelo_EstadoCivil_ajust_masc), confint(modelo_EstadoCivil_ajust_masc)))
# Mulher
modelo_EstadoCivil_ajust_fem <- svyglm(formula = Q084~C011+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_EstadoCivil_ajust_fem
exp(cbind(OR=coef(modelo_EstadoCivil_ajust_fem), confint(modelo_EstadoCivil_ajust_fem)))




#####==== Modelo Área Urbana/rural ====
dadosPNS_2019$variables$V0026 <- relevel(dadosPNS_2019$variables$V0026, ref = "Urbano")
# BRUTO
# Homem 
modelo_UrbanoRural_bruto_masc <- svyglm(formula = Q084~V0026, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_UrbanoRural_bruto_masc 
exp(cbind(OR=coef(modelo_UrbanoRural_bruto_masc), confint(modelo_UrbanoRural_bruto_masc)))
# Mulher
modelo_UrbanoRural_bruto_fem <- svyglm(formula = Q084~V0026, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_UrbanoRural_bruto_fem
exp(cbind(OR=coef(modelo_UrbanoRural_bruto_fem), confint(modelo_UrbanoRural_bruto_fem)))

# AJUSTADO escolaridade e faixa etária
# Homem 
modelo_UrbanoRural_ajust_masc <- svyglm(formula = Q084~V0026+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_UrbanoRural_ajust_masc 
exp(cbind(OR=coef(modelo_UrbanoRural_ajust_masc), confint(modelo_UrbanoRural_ajust_masc)))
# Mulher
modelo_UrbanoRural_ajust_fem <- svyglm(formula = Q084~V0026+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_UrbanoRural_ajust_fem
exp(cbind(OR=coef(modelo_UrbanoRural_ajust_fem), confint(modelo_UrbanoRural_ajust_fem)))




#####==== Modelo IMC ==== 
dadosPNS_2019$variables$IMC <- relevel(dadosPNS_2019$variables$IMC, ref = "Peso normal") 
# BRUTO 
# Homem
modelo_IMC_bruto_mascul <- svyglm(formula=Q084~IMC, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_IMC_bruto_mascul
exp(cbind(OR = coef(modelo_IMC_bruto_mascul), confint(modelo_IMC_bruto_mascul)))
# Mulher
modelo_IMC_bruto_fem <- svyglm(formula=Q084~IMC, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_IMC_bruto_fem
exp(cbind(OR = coef(modelo_IMC_bruto_fem), confint(modelo_IMC_bruto_fem)))

# AJUSTADO por escolaridade e faixa etária
# Homem:
modelo_IMC_ajus_mascul <- svyglm(formula=Q084~IMC+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_IMC_ajus_mascul
exp(cbind(OR = coef(modelo_IMC_ajus_mascul), confint(modelo_IMC_ajus_mascul)))
# Mulher:
modelo_IMC_ajus_fem <- svyglm(formula=Q084~IMC+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_IMC_ajus_fem
exp(cbind(OR = coef(modelo_IMC_ajus_fem), confint(modelo_IMC_ajus_fem)))




#####==== Modelo Percepção estado de saúde ====
dadosPNS_2019$variables$N001 <- relevel(dadosPNS_2019$variables$N001, ref="Muito boa")
# BRUTO
# Homem
modelo_Percep_Saude_bruto_masc <- svyglm(formula=Q084~N001, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
exp(cbind(OR = coef(modelo_Percep_Saude_bruto_masc), confint(modelo_Percep_Saude_bruto_masc)))
# Mulher
modelo_Percep_Saude_bruto_fem <- svyglm(formula=Q084~N001, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
exp(cbind(OR = coef(modelo_Percep_Saude_bruto_fem), confint(modelo_Percep_Saude_bruto_fem)))

# AJUSTADO por escolaridade e faixa etária 
# Homem
modelo_Percep_Saude_ajustado_masc <- svyglm(formula=Q084~N001+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
exp(cbind(OR = coef(modelo_Percep_Saude_ajustado_masc), confint(modelo_Percep_Saude_ajustado_masc)))
# Mulher
modelo_Percep_Saude_ajustado_fem <- svyglm(formula=Q084~N001+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
exp(cbind(OR = coef(modelo_Percep_Saude_ajustado_fem), confint(modelo_Percep_Saude_ajustado_fem)))



#####==== Modelo escolaridade ====
# BRUTO
# Homem
modelo_escolaridade_bruto_masc <- svyglm(formula=Q084~VDD004A, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
exp(cbind(OR = coef(modelo_escolaridade_bruto_masc), confint(modelo_escolaridade_bruto_masc)))
# Mulher
modelo_escolaridade_bruto_fem <- svyglm(formula=Q084~VDD004A, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
exp(cbind(OR = coef(modelo_escolaridade_bruto_fem), confint(modelo_escolaridade_bruto_fem)))

# AJUSTADO faixa etária 
# Homem
modelo_escolaridade_ajustado_masc <- svyglm(formula=Q084~VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
exp(cbind(OR = coef(modelo_escolaridade_ajustado_masc), confint(modelo_escolaridade_ajustado_masc)))
# Mulher
modelo_Percep_Saude_ajustado_fem <- svyglm(formula=Q084~VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
exp(cbind(OR = coef(modelo_Percep_Saude_ajustado_fem), confint(modelo_Percep_Saude_ajustado_fem)))




#####==== Modelo Ativo/sedentario ====
dadosPNS_2019$variables <- transform(dadosPNS_2019$variables, ativo_sedentario=P035*P03702)
# Atribuindo os níveis para a variável ativo_sedentario (Abaixo de 150 minutos/semana = inativo; Acima de 150 min = ativo)

dadosPNS_2019$variables <- transform(dadosPNS_2019$variables, ativo_sedentario = as.factor(ifelse(ativo_sedentario<150,"Sedentario",ifelse(ativo_sedentario>=150,"Ativo",NA))))
# fonte do valor de referência: https://health.gov/sites/default/files/2019-09/Physical_Activity_Guidelines_2nd_edition.pdf   (acesso em 09/agosto/2021) 

# Reordenando os níveis da variável ativo_sedentario (talvez não seja necessário, pois o "Ativo" alfabeticamente é anterior ao "Sedentario":
dadosPNS_2019$variables$ativo_sedentario <- factor(dadosPNS_2019$variables$ativo_sedentario, levels = c("Ativo","Sedentario"))
dadosPNS_2019$variables$ativo_sedentario <- relevel(dadosPNS_2019$variables$ativo_sedentario, ref = "Ativo")

# BRUTO
# Homem 
modelo_AtivoSedentario_bruto_masc <- svyglm(formula = Q084~ativo_sedentario, design=subset(pns2019_moradorselecionado, C006 == "Homem" & C008>=18), family="binomial")
modelo_AtivoSedentario_bruto_masc 
exp(cbind(OR=coef(modelo_AtivoSedentario_bruto_masc), confint(modelo_AtivoSedentario_bruto_masc)))
# Mulher
modelo_ativo_sedentario_bruto_fem <- svyglm(formula = Q084~ativo_sedentario, design=subset(pns2019_moradorselecionado, C006 == "Mulher"& C008>=18), family="binomial")
modelo_ativo_sedentario_bruto_fem
exp(cbind(OR=coef(modelo_ativo_sedentario_bruto_fem), confint(modelo_ativo_sedentario_bruto_fem)))


# AJUSTADO escolaridade e faixa etária
# Homem 
modelo_ativo_sedentario_ajus_masc <- svyglm(formula = Q084~ativo_sedentario+VDD004A+C008, design=subset(pns2019_moradorselecionado, C006 == "Homem"), family="binomial")
modelo_ativo_sedentario_ajus_masc 
exp(cbind(OR=coef(modelo_ativo_sedentario_ajus_masc), confint(modelo_ativo_sedentario_ajus_masc)))
# Mulher
modelo_ativo_sedentario_ajus_fem <- svyglm(formula = Q084~ativo_sedentario+VDD004A+C008, design=subset(pns2019_moradorselecionado, C006 == "Mulher"), family="binomial")
modelo_ativo_sedentario_ajus_fem
exp(cbind(OR=coef(modelo_ativo_sedentario_ajus_fem), confint(modelo_ativo_sedentario_ajus_fem)))



#####==== Modelo DORT ====
dadosPNS_2019$variables$Q088 <- relevel(dadosPNS_2019$variables$Q088, ref = "Não")
# BRUTO
# Homem 
modelo_DORT_bruto_masc <- svyglm(formula = Q084~Q088, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_DORT_bruto_masc 
exp(cbind(OR=coef(modelo_DORT_bruto_masc), confint(modelo_DORT_bruto_masc)))
# Mulher
modelo_DORT_bruto_fem <- svyglm(formula = Q084~Q088, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_DORT_bruto_fem
exp(cbind(OR=coef(modelo_DORT_bruto_fem), confint(modelo_DORT_bruto_fem)))

# AJUSTADO escolaridade e faixa etária
# Homem 
modelo_DORT_ajus_masc <- svyglm(formula = Q084~Q088+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_DORT_ajus_masc 
exp(cbind(OR=coef(modelo_DORT_ajus_masc), confint(modelo_DORT_ajus_masc)))
# Mulher
modelo_DORT_ajus_fem <- svyglm(formula = Q084~Q088+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_DORT_ajus_fem
exp(cbind(OR=coef(modelo_DORT_ajus_fem), confint(modelo_DORT_ajus_fem)))




#####==== Modelo Depressão ====
dadosPNS_2019$variables$Q092 <- relevel(dadosPNS_2019$variables$Q092, ref = "Não")
# BRUTO 
modelo_depre_bruto_mascul <- svyglm(formula=Q084~Q092, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_depre_bruto_mascul
exp(cbind(OR = coef(modelo_depre_bruto_mascul), confint(modelo_depre_bruto_mascul)))
# Mulher
modelo_depre_bruto_fem <- svyglm(formula=Q084~Q092, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_depre_bruto_fem
exp(cbind(OR = coef(modelo_depre_bruto_fem), confint(modelo_depre_bruto_fem)))

# AJUSTADO por escolaridade e faixa etária
# Homem:
modelo_depre_ajus_mascul <- svyglm(formula=Q084~Q092+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_depre_ajus_mascul
exp(cbind(OR = coef(modelo_depre_ajus_mascul), confint(modelo_depre_ajus_mascul)))
# Mulher:
modelo_depre_ajus_fem <- svyglm(formula=Q084~Q092+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_depre_ajus_fem
exp(cbind(OR = coef(modelo_depre_ajus_fem), confint(modelo_depre_ajus_fem)))




#####==== Modelo Dç Mental ====
# transtorno de ansiedade, síndrome do pânico, esquizofrenia, transtorno bipolar, psicose ou TOC 
dadosPNS_2019$variables$Q11006 <- relevel(dadosPNS_2019$variables$Q11006, ref = "Não")
# BRUTO
# Homem 
modelo_DçMental_bruto_masc <- svyglm(formula = Q084~Q11006, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_DçMental_bruto_masc 
exp(cbind(OR=coef(modelo_DçMental_bruto_masc), confint(modelo_DçMental_bruto_masc)))
# Mulher
modelo_DçMental_bruto_fem <- svyglm(formula = Q084~Q11006, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_DçMental_bruto_fem
exp(cbind(OR=coef(modelo_DçMental_bruto_fem), confint(modelo_DçMental_bruto_fem)))

# AJUSTADO escolaridade e faixa etária
# Homem 
modelo_DçMental_ajus_masc <- svyglm(formula = Q084~Q11006+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_DçMental_ajus_masc 
exp(cbind(OR=coef(modelo_DçMental_ajus_masc), confint(modelo_DçMental_ajus_masc)))
# Mulher
modelo_DçMental_ajus_fem <- svyglm(formula = Q084~Q11006+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_DçMental_ajus_fem
exp(cbind(OR=coef(modelo_DçMental_ajus_fem), confint(modelo_DçMental_ajus_fem)))




#####==== Modelo DPOC ====
dadosPNS_2019$variables$Q11604 <- relevel(dadosPNS_2019$variables$Q11604, ref = "Não")
# BRUTO
# Homem:
modelo_DPOC_bruto_mascul <- svyglm(formula = Q084 ~ Q11604, design=subset(dadosPNS_2019, C006 == "Homem"), family = "binomial")
modelo_DPOC_bruto_mascul
exp(cbind(OR=coef(modelo_DPOC_bruto_mascul), confint(modelo_DPOC_bruto_mascul)))
# Mulher:
modelo_DPOC_bruto_fem <- svyglm(formula = Q084~Q11604, design=subset(dadosPNS_2019, C006 == "Mulher"), family = "binomial")
modelo_DPOC_bruto_fem
exp(cbind(OR=coef(modelo_DPOC_bruto_fem), confint(modelo_DPOC_bruto_fem)))

# AJUSTADO por escolariade e faixa etária
# Homem
modelo_DPOC_ajus_mascul <- svyglm(formula = Q084~Q11604+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family = "binomial")
modelo_DPOC_ajus_mascul
exp(cbind(OR=coef(modelo_DPOC_ajus_mascul), confint(modelo_DPOC_ajus_mascul)))
# Mulher
modelo_DPOC_ajus_fem <- svyglm(formula = Q084~Q11604+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family = "binomial")
modelo_DPOC_ajus_fem
exp(cbind(OR=coef(modelo_DPOC_ajus_fem), confint(modelo_DPOC_ajus_fem)))




#####==== Modelo Hipertensão ====
dadosPNS_2019$variables$Q00201 <- relevel(dadosPNS_2019$variables$Q00201, ref = "Não")
# BRUTO
# Homem 
modelo_Hipertensao_bruto_masc <- svyglm(formula = Q084~Q00201, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_Hipertensao_bruto_masc 
exp(cbind(OR=coef(modelo_Hipertensao_bruto_masc ), confint(modelo_Hipertensao_bruto_masc )))
# Mulher
modelo_Hipertensao_bruto_fem <- svyglm(formula = Q084~Q00201, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_Hipertensao_bruto_fem
exp(cbind(OR=coef(modelo_Hipertensao_bruto_fem), confint(modelo_Hipertensao_bruto_fem)))

# AJUSTADO por nivel de instrução e faixa etária
# Homem 
modelo_Hipertensao_ajus_masc <- svyglm(formula = Q084~Q00201+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_Hipertensao_ajus_masc 
exp(cbind(OR=coef(modelo_Hipertensao_ajus_masc), confint(modelo_Hipertensao_ajus_masc)))
# Mulher
modelo_Hipertensao_ajus_fem <- svyglm(formula = Q084~Q00201+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_Hipertensao_ajus_fem
exp(cbind(OR=coef(modelo_Hipertensao_ajus_fem), confint(modelo_Hipertensao_ajus_fem)))




#####==== Modelo Diabetes ====
dadosPNS_2019$variables$Q03001 <- relevel(dadosPNS_2019$variables$Q03001, ref = "Não") 
# BRUTO 
# Homem
modelo_Diabetes_bruto_mascul <- svyglm(formula=Q084~Q03001, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_Diabetes_bruto_mascul
exp(cbind(OR = coef(modelo_Diabetes_bruto_mascul), confint(modelo_Diabetes_bruto_mascul)))
# Mulher
modelo_Diabetes_bruto_fem <- svyglm(formula=Q084~Q03001, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_Diabetes_bruto_fem
exp(cbind(OR = coef(modelo_Diabetes_bruto_fem), confint(modelo_Diabetes_bruto_fem)))

# AJUSTADO por escolaridade e faixa etária
# Homem:
modelo_Diabetes_ajus_mascul <- svyglm(formula=Q084~Q03001+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_Diabetes_ajus_mascul
exp(cbind(OR = coef(modelo_Diabetes_ajus_mascul), confint(modelo_Diabetes_ajus_mascul)))
# Mulher:
modelo_Diabetes_ajus_fem <- svyglm(formula=Q084~Q03001+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_Diabetes_ajus_fem
exp(cbind(OR = coef(modelo_Diabetes_ajus_fem), confint(modelo_Diabetes_ajus_fem)))



#####==== Modelo Artrite ====
dadosPNS_2019$variables$Q079 <- relevel(dadosPNS_2019$variables$Q079, ref = "Não")
# BRUTO
# Homem 
modelo_Artrite_bruto_masc <- svyglm(formula = Q084~Q079, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_Artrite_bruto_masc 
exp(cbind(OR=coef(modelo_Artrite_bruto_masc), confint(modelo_Artrite_bruto_masc)))
# Mulher
modelo_Artrite_bruto_fem <- svyglm(formula = Q084~Q079, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_Artrite_bruto_fem
exp(cbind(OR=coef(modelo_Artrite_bruto_fem), confint(modelo_Artrite_bruto_fem)))

# AJUSTADO por nivel de instrução e faixa etária
# Homem 
modelo_Artrite_ajus_masc <- svyglm(formula = Q084~Q079+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_Artrite_ajus_masc 
exp(cbind(OR=coef(modelo_Artrite_ajus_masc), confint(modelo_Artrite_ajus_masc)))
# Mulher
modelo_Artrite_ajus_fem <- svyglm(formula = Q084~Q079+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_Artrite_ajus_fem
exp(cbind(OR=coef(modelo_Artrite_ajus_fem), confint(modelo_Artrite_ajus_fem)))



#####==== Modelo Atividade física pesada em casa ====
dadosPNS_2019$variables$P044 <- relevel(dadosPNS_2019$variables$P044, ref = "Não")
# BRUTO
# Homem 
modelo_AtivPesadaCasa_bruto_masc <- svyglm(formula = Q084~P044, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_AtivPesadaCasa_bruto_masc 
exp(cbind(OR=coef(modelo_AtivPesadaCasa_bruto_masc), confint(modelo_AtivPesadaCasa_bruto_masc)))
# Mulher
modelo_AtivPesadaCasa_bruto_fem <- svyglm(formula = Q084~P044, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_AtivPesadaCasa_bruto_fem
exp(cbind(OR=coef(modelo_AtivPesadaCasa_bruto_fem), confint(modelo_AtivPesadaCasa_bruto_fem)))

# AJUSTADO por nivel de instrução e faixa etária
# Homem 
modelo_AtivPesadaCasa_ajus_masc <- svyglm(formula = Q084~P044+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_AtivPesadaCasa_ajus_masc 
exp(cbind(OR=coef(modelo_AtivPesadaCasa_ajus_masc), confint(modelo_AtivPesadaCasa_ajus_masc)))
# Mulher
modelo_AtivPesadaCasa_ajus_fem <- svyglm(formula = Q084~P044+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_AtivPesadaCasa_ajus_fem
exp(cbind(OR=coef(modelo_AtivPesadaCasa_ajus_fem), confint(modelo_AtivPesadaCasa_ajus_fem)))



#####==== Modelo Atividade física pesada trabalho ====
dadosPNS_2019$variables$P039 <- relevel(dadosPNS_2019$variables$P039, ref = "Não")
# BRUTO
# Homem 
modelo_AtivPesadaTrab_bruto_masc <- svyglm(formula = Q084~P039, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_AtivPesadaTrab_bruto_masc 
exp(cbind(OR=coef(modelo_AtivPesadaTrab_bruto_masc), confint(modelo_AtivPesadaTrab_bruto_masc)))
# Mulher
modelo_AtivPesadaTrab_bruto_fem <- svyglm(formula = Q084~P039, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_AtivPesadaTrab_bruto_fem
exp(cbind(OR=coef(modelo_AtivPesadaTrab_bruto_fem), confint(modelo_AtivPesadaTrab_bruto_fem)))

# AJUSTADO por nivel de instrução e faixa etária
# Homem 
modelo_AtivPesadaTrab_ajus_masc <- svyglm(formula = Q084~P039+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Homem"), family="binomial")
modelo_AtivPesadaTrab_ajus_masc 
exp(cbind(OR=coef(modelo_AtivPesadaTrab_ajus_masc), confint(modelo_AtivPesadaTrab_ajus_masc)))
# Mulher
modelo_AtivPesadaTrab_ajus_fem <- svyglm(formula = Q084~P039+VDD004A+C008, design=subset(dadosPNS_2019, C006 == "Mulher"), family="binomial")
modelo_AtivPesadaTrab_ajus_fem
exp(cbind(OR=coef(modelo_AtivPesadaTrab_ajus_fem), confint(modelo_AtivPesadaTrab_ajus_fem)))


#####==== Dç Crônica (física ou mental/mais de 6 meses duração) ====
dadosPNS_2019$variables$J007 <- relevel(dadosPNS_2019$variables$J007, ref = "Não")

# Bruto
# Homem
modelo_Cronicas_FisMental_bruto_masc <- svyglm(Q084~J007, design=subset(dadosPNS_2019, C006=="Homem"), family="binomial")
exp(cbind(OR=coef(modelo_Cronicas_FisMental_bruto_masc),confint(modelo_Cronicas_FisMental_bruto_masc)))
# Mulher
modelo_Cronicas_FisMental_bruto_fem <- svyglm(Q084~J007, design=subset(dadosPNS_2019, C006=="Mulher"), family="binomial")
exp(cbind(OR=coef(modelo_Cronicas_FisMental_bruto_fem),confint(modelo_Cronicas_FisMental_bruto_fem)))


# Ajustado
# Homem
modelo_Cronicas_FisMental_ajus_masc <- svyglm(Q084~J007+VDD004A+C008, design=subset(dadosPNS_2019, C006=="Homem"), family="binomial")
exp(cbind(OR=coef(modelo_Cronicas_FisMental_ajus_masc),confint(modelo_Cronicas_FisMental_ajus_masc)))

# Mulhwe
modelo_Cronicas_FisMental_ajus_fem <- svyglm(Q084~J007+VDD004A+C008, design=subset(dadosPNS_2019, C006=="Mulher"), family="binomial")
exp(cbind(OR=coef(modelo_Cronicas_FisMental_ajus_fem),confint(modelo_Cronicas_FisMental_ajus_fem)))




# obs.: Para estimar a matrix de correlação (Thomas Lumley - https://stackoverflow.com/questions/34418822/pearson-correlation-coefficient-in-rs-survey-package)

# obs.: P/ estimar a matrix de correlação:
# You can use svyvar to estimate the variance-covariance matrix, and then scale it to the correlation:

# objeto <- "formato survey design"
# v <- svyvar(~variavel1+variavel2, objeto)

# as.matrix(v)
# cov2cor(as.matrix(v))




