# R script for the analyses of covid test results in the Childlimar cohort of children

# Manuscript: "SARS-COV-2 testing and positivity in children: The role of socioeconomic factors and migratory status in the Lisbon Metropolitan Area, Portugal."
# Authors: Iolanda Banha Alves1*, Sofia Seabra1, Thierry Mertens1, Maria do Rosário O. Martins1
# 1Global Health and Tropical Medicine (GHTM), Institute of Hygiene and Tropical Medicine (IHMT), NOVA University of Lisbon, Lisbon, Portugal.
# *Corresponding author

# Author of the script: Sofia G. Seabra (sgseabra@ihmt.unl.pt)

# 1. Analyses of dataframe of CHILDREN (one children per row) 
# 1.1. Import dataframe
# 1.2. Descriptive statistics
# 1.3. Plots
# 2. CROSSTABS WITH MIGRATORY STATUS
# 3. HEATMAP of association coefficient between pairs of categorical variables
# 4. ASSOCIATION ANALYSES
# 5. BIVARIATE REGRESSION MODELS
# 6. MULTIVARIABLE REGRESSION 
# 6.1. Forest plots - Prevalence ratio Multivariable Poisson regression
# 7. Analyses of dataframe of COVID test results (some children have several tests)
# 7.1 IMPORT DATAFRAME of COVID test results 
# 7.2 ADD NEW VARIABLES TO DATAFRAME
# 7.3.ASSOCIATION OF MIGRANT STATUS AND TEST TYPE (Antigen or PCR)
# 7.4 SARS-CoV-2 variants
# 7.5.PLOT NUMBER OF TESTS PER DATE, PER RESULT, PER STATUS WITH PEAKS OF WAVES
# 7.6.TOTAL NUMBER OF TESTS AND OF POSITIVES PER CHILD
# 7.7. CHILDREN WITH POSITIVE TESTS / NEGATIVE TESTS


# R packages
library(openxlsx)
library(tidyverse)
library(dplyr)
library(lubridate)
library(Hmisc)
library(grid)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(scales)
library(ggthemes)
library(ggplot2)
library(plotly)
library(naniar)
library(DescTools)
library(Amelia)
library(lubridate)
library(stringr)
library(fastDummies)
library(sandwich)  # For robust standard errors
library(lmtest) 
library(MASS)
library(ResourceSelection)
library(RColorBrewer)
library(vcd)
library("epitools")
library(car)


# Set working directory
setwd('/')

####################################################################################################
####################################################################################################
# 1. Analyses of dataframe of CHILDREN (one children per row) 
####################################################################################################


##################
# 1.1 IMPORT DATAFRAME of Children
df_criancas<-read.xlsx("./", sheet = 1, startRow = 1, colNames = TRUE)

colnames(df_criancas)
glimpse(df_criancas)

# Convert "999" to NA
df_criancas[, 2:251] <- lapply(df_criancas[, 2:251], function(x) replace(x, x == 999, NA))

table(df_criancas$Pelo_menos_1_teste)

# Convert the Pelo_menos_1_positivo to NA if they have 0 in Pelo_menos_1_teste
table(df_criancas$Pelo_menos_1_teste, df_criancas$Pelo_menos_1_positivo, useNA = "always")
df_criancas$Pelo_menos_1_positivo[df_criancas$Pelo_menos_1_teste %in% c(0)]<-NA
table(df_criancas$Pelo_menos_1_teste, df_criancas$Pelo_menos_1_positivo, useNA = "always")

# Check variables and convert "Do not know/did not answer" to NA
df_criancas$mae_amamentou_crianca[df_criancas$mae_amamentou_crianca %in% c(3)] <-NA # Substitui o valor 3 por NA
df_criancas$desporto[df_criancas$desporto %in% c(3)] <-NA
df_criancas$escolaridade_mae[df_criancas$escolaridade_mae %in% c(7)] <-NA
df_criancas$situacao_emprego[df_criancas$situacao_emprego %in% c(10)] <-NA

# Convert variables
df_criancas$concelho_residencia_cat <- df_criancas$concelho_residencia
table(df_criancas$concelho_residencia)
df_criancas$concelho_residencia_cat[df_criancas$concelho_residencia_cat %in% seq(5,15)] <- "5"
table(df_criancas$concelho_residencia_cat)

df_criancas$Densidade_Habitacao_cat_1_5 <- df_criancas$Densidade_Habitação
table(df_criancas$Densidade_Habitacao_cat_1_5, df_criancas$Estatuto_Migrante_Crianca)
df_criancas$Densidade_Habitacao_cat_1_5[as.numeric(df_criancas$Densidade_Habitacao_cat_1_5) <= 1.5 ] <- "0"
df_criancas$Densidade_Habitacao_cat_1_5[as.numeric(df_criancas$Densidade_Habitacao_cat_1_5) > 1.5 ] <- "1"
table(df_criancas$Densidade_Habitacao_cat_1_5, df_criancas$Densidade_Habitação)
table(df_criancas$Densidade_Habitacao_cat_1_5, df_criancas$Estatuto_Migrante_Crianca)


df_criancas$rendimento_familiar_cat2 <- df_criancas$rendimento_familiar
table(df_criancas$rendimento_familiar_cat2, df_criancas$Estatuto_Migrante_Crianca)
df_criancas$rendimento_familiar_cat2[as.numeric(df_criancas$rendimento_familiar_cat2) == 7 ] <- NA
df_criancas$rendimento_familiar_cat2[as.numeric(df_criancas$rendimento_familiar_cat2) <= 3 ] <- "1"
df_criancas$rendimento_familiar_cat2[as.numeric(df_criancas$rendimento_familiar_cat2) >  3 ] <- "2"
table(df_criancas$rendimento_familiar_cat2, df_criancas$rendimento_familiar)
table(df_criancas$rendimento_familiar_cat2, df_criancas$Estatuto_Migrante_Crianca)

# Convert to date format 
df_criancas[,"data_nascimento_crianca"] <- openxlsx::convertToDateTime(df_criancas[,"data_nascimento_crianca"]) # Convert numeric to Date format

glimpse(df_criancas)
length(unique(df_criancas$record_id))

write.xlsx(df_criancas, "./df_criancas.xlsx", fileEncoding = "latin1")

#########################################
# 1.2. DESCRIPTIVE STATISTICS
#########################################
glimpse(df_criancas)
colnames(df_criancas)

#########################################
# 1.2.1. FREQUENCY TABLES
#########################################

variables_all_cat<- c("Estatuto_Migrante_Crianca","Pelo_menos_1_teste","Pelo_menos_1_positivo",
                      "Coorte","sexo_crianca",
                      "peso_nascenca_cat","duracao_gravidez_cat","calend_rio_vacinal","Pelo_menos_1_diag_asma",
                      "Pelo_menos_1_diag_obesid", "Pelo_menos_1_diag_exc_pes", "Pelo_menos_1_diag_neuro", "mae_amamentou_crianca",
                      "tempo_amament_exlusiva_cat", "tempo_amam_total_cat",
                      "naturalidade_crianca", "concelho_residencia","pais_crianca_1_ger", 
                      "Pais_crianca_1_ger_PT_UE_NEU","Renda_Pais_Criança","Imig_1ª_ger","Estatuto_Migrante_Criança", "Com_quem_vive_a_crianca_2",
                      "adultos","menores","numero_quartos" ,"numero_casas_de_banho", "Densidade_Habitação_cat", "Densidade_Habitacao_cat_1_5",
                      "Lingua_falada_em_casa_2", "local_crianca_dia" ,"Local_criança_dia_cat" , "brincadeiras_tempo_Cat",
                      "desporto","atividade",  "saude_crianca" , "rel_crianca" , "sexo_cuidador" , "Faixa_etária_cuidador",
                      "Pais_Cuidador","Cuidador_EU_NEU" , "Cuidador_PT_EU_NEU" , "Renda_País_Cuidador",
                      "escolaridade_mae", "Escolaridade_mae_cat" , "situacao_emprego", "Situação_Emprego_Cat","Rendimento_Familiar_Cat" ,"rendimento_familiar_cat2"
                      )


sink(file="./Routput_frequency_tables.txt", append=FALSE, type="output")

for(i in variables_all_cat){
  print("__________________________")
  print(i)
  print(table(df_criancas[,i]))
  print(prop.table(table(df_criancas[,i]))*100)
}

sink()

#########################################
# 1.2.2. DESCRIPTIVE STATISTICS - QUANTITATIVE VARIABLES
#########################################

variables_all_quantitative<- c("peso_crian_a_dclinic","duracao_gravidez_dclinc","tempo_amamentacao_exclusiva",
                               "tempo_amamentacao_total","tempo_residencia_portugal_crianca","Densidade_Habitação", "brincadeiras_tempo",
                               "tempo_semana" ,"idade_cuidador" ,"mae_tempo_portugal" ,"mae_idade_chegada",
                               "pai_tempo_portugal" , "pai_idade_chegada")

sink(file="./Routput_descriptive_quantitative.txt", append=FALSE, type="output")

for(i in variables_all_quantitative){
  print("__________________________")
  print(i)
  num_obs <- sum(!is.na(df_criancas[,i]))      # number of observations, excluding NA
  media <- mean(df_criancas[,i], na.rm = TRUE) 
  desvio <- sd(df_criancas[,i], na.rm = TRUE)  
  mediana <- median(df_criancas[,i], na.rm = TRUE) 
  iqr_val <- IQR(df_criancas[,i], na.rm = TRUE)   
  
  # Mostrar resultados
  stats <- data.frame(Num_Obs = num_obs, 
                      Media = media, 
                      Desvio_Padrao = desvio, 
                      Mediana = mediana, 
                      IQR = iqr_val)
  
  print(stats)
  
}

sink()

#######################
# 1.2.3. PLOTS
######################

######################
# PLOT At least 1 test per children

table(df_criancas$Estatuto_Migrante_Crianca, df_criancas$Pelo_menos_1_teste)

png(file = "./plot_pelo_menos_1teste_by_status.png", bg="white", width = 5, height = 3,  
    units = "in", res = 200)
ggplot(df_criancas,aes(x=factor(Estatuto_Migrante_Crianca), fill=factor(Pelo_menos_1_teste))) + 
  geom_bar(stat="count", position="stack") +
  scale_x_discrete(labels=c("1"="Immigrant", "0"="Non-immigrant")) +
  xlab("") + ylab("Number of children") +
  theme_light() +
   scale_fill_manual(values=c("#f3d9ba",
                             "#92add0"),
                    labels=c("No test done", "At least 1 test done"),
                    name="") 
dev.off()

######################
# PLOT At least 1 positive per children

table(df_criancas$Estatuto_Migrante_Crianca, df_criancas$Pelo_menos_1_positivo)

png(file = "./plot_pelo_menos_1positivo_by_status.png", bg="white", width = 5, height = 3,  
    units = "in", res = 200)
df_criancas %>% 
  drop_na(Pelo_menos_1_positivo) %>%
  ggplot(aes(x=factor(Estatuto_Migrante_Crianca), fill=factor(Pelo_menos_1_positivo))) + 
  geom_bar(stat="count", position="stack") +
  scale_x_discrete(labels=c("1"="Immigrant", "0"="Non-immigrant")) +
  xlab("") + ylab("Number of children that \n performed at least one test") +
  theme_light() +
  scale_fill_manual(values=c("#f3d9ba",
                             "#92add0"),
                    labels=c("All negative", "At least 1 positive"),
                    name="") 
dev.off()

df_criancas_new <- df_criancas %>%
  mutate(Test_Status = case_when(
    Pelo_menos_1_teste == 0 ~ "No test done",
    Pelo_menos_1_positivo == 0 ~ "All tests negative",
    Pelo_menos_1_positivo == 1 ~ "At least 1 positive test",
    TRUE ~ "At least 1 test done")) %>%
  mutate(Test_Status = factor(Test_Status, levels = c("No test done", "All tests negative", "At least 1 positive test")))

    
levels(as.factor(df_criancas_new$Test_Status))


png(file = "./plot_test_done_and_result_by_status.png", bg="white", width = 5, height = 3,  
    units = "in", res = 200)

df_criancas_new %>% 
  drop_na(Test_Status) %>%
  ggplot(aes(x=factor(Estatuto_Migrante_Crianca), fill=factor(Test_Status))) + 
  geom_bar(stat="count", position="stack") +
  scale_x_discrete(labels=c("1"="Immigrant", "0"="Non-immigrant")) +
  xlab("") + ylab("Number of children") +
  theme_light() +
  scale_fill_manual(values=c("gray","#f3d9ba",
                             "#92add0"), name="") 

dev.off()


############################################################################
############################################################################
# 2. CROSSTABS WITH MIGRATORY STATUS

# List of categorical variables
variables_association<- c("Pelo_menos_1_teste","Pelo_menos_1_positivo","Estatuto_Migrante_Crianca", "sexo_crianca", "Coorte","concelho_residencia_cat",
                          "peso_nascenca_cat", "duracao_gravidez_cat", 
                          "Pelo_menos_1_diag_asma", "Pelo_menos_1_diag_obesid", "Pelo_menos_1_diag_exc_pes", "Pelo_menos_1_diag_neuro", 
                          "mae_amamentou_crianca","tempo_amament_exlusiva_cat", "tempo_amam_total_cat",  
                          "Pais_crianca_1_ger_PT_UE_NEU", "Renda_Pais_Criança", "Imig_1ª_ger", "Densidade_Habitação_cat", "Densidade_Habitacao_cat_1_5",
                          "Local_criança_dia_cat", "brincadeiras_tempo_Cat", "desporto", "tempo_semana_desporto_cat", 
                          "saude_crianca", "sexo_cuidador", "Faixa_etária_cuidador", 
                          "Cuidador_EU_NEU", "Cuidador_PT_EU_NEU", "Renda_País_Cuidador", 
                          "Escolaridade_mae_cat", "situacao_emprego","Situação_Emprego_Cat", "Rendimento_Familiar_Cat", "rendimento_familiar_cat2",
                          "tem_medico_familia", "seguido_no_pediatra", "crianca_utiliza",  "traz_crianca_centrosaude")

sink(file="./Routput_crosstabs_with_MigrantStatus.txt", append=FALSE, type="output")

for(i in variables_association){
  print(noquote(i))
  print(data.frame(table(df_criancas[,i])))

  absolute_freq <- table(df_criancas[,i],df_criancas$Estatuto_Migrante_Crianca)
  colnames(absolute_freq)<-c("Non-immigrant","Immigrant")
  print(absolute_freq)
  
   
  # Compute column-wise proportions
  relative_freq <- prop.table(absolute_freq, margin = 2)*100
  colnames(relative_freq)<-c("Non-immigrant","Immigrant")
  print(round(relative_freq, 1))  # Round for readability
  
}

sink()

############################################################################
############################################################################
## 3. HEATMAP of association coefficient between pairs of categorical variables
####################

variables_heatmap<- c("Estatuto_Migrante_Crianca", "sexo_crianca", "Coorte",
                      "duracao_gravidez_cat", "peso_nascenca_cat", "mae_amamentou_crianca",
                      "Local_criança_dia_cat",
                      "sexo_cuidador", "Escolaridade_mae_cat",
                      "Situação_Emprego_Cat", "rendimento_familiar_cat2","Densidade_Habitacao_cat_1_5")

vars<- c("Pelo_menos_1_teste", "Pelo_menos_1_positivo",variables_heatmap)

n <- length(vars)
cramer_matrix <- matrix(NA, n, n, dimnames = list(vars, vars))



# Compute Cramér's V for each pair of variables
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    if (i == j) {
      cramer_matrix[i, j] <- 1  # Perfect association with itself
    } else {
      tbl <- table(df_criancas[[vars[i]]], df_criancas[[vars[j]]])  # Create contingency table
      cramer_matrix[i, j] <- assocstats(tbl)$cramer  # Compute Cramér's V
    }
  }
}

# Convert the matrix to long format
cramer_data <- melt(cramer_matrix)

# Create the heatmap
png(file = "./plot_heatmap_association.png", bg="white", width = 8, height = 6,  
    units = "in", res = 200)
ggplot(cramer_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightgray", high = "black", na.value = "transparent") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0),  # Labels horizontais
    axis.text.y = element_text(hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(title = "",
       fill = "Cramér's V",
       x="", y="") +
  scale_x_discrete(position = "top",labels = c("Pelo_menos_1_teste" = "At least one test",
                                               "Pelo_menos_1_positivo" = " At least one positive",
                                               "Estatuto_Migrante_Crianca" = "Migratory status",
                                               "sexo_crianca" = "Sex",
                                               "Coorte" = " Cohort",
                                               "duracao_gravidez_cat" = "Pregnancy duration",
                                               "peso_nascenca_cat" = "Weight at birth",
                                               "mae_amamentou_crianca" = "Breastfed",
                                               "Local_criança_dia_cat" = "Day spent at",
                                               "sexo_cuidador"  = "Sex of caregiver",
                                               "Escolaridade_mae_cat"  = "Educational level of mother",
                                               "Situação_Emprego_Cat" = "Employment status of mother",
                                               "rendimento_familiar_cat2" = "Family income",
                                               "Densidade_Habitacao_cat_1_5" = "Household density"
                                               )) +
  scale_y_discrete(labels = c("Pelo_menos_1_teste" = "At least one test",
                              "Pelo_menos_1_positivo" = " At least one positive",
                              "Estatuto_Migrante_Crianca" = "Migratory status",
                              "sexo_crianca" = "Sex",
                              "Coorte" = " Cohort",
                              "duracao_gravidez_cat" = "Pregnancy duration",
                              "peso_nascenca_cat" = "Weight at birth",
                              "mae_amamentou_crianca" = "Breastfed",
                              "Local_criança_dia_cat" = "Day spent at",
                              "sexo_cuidador"  = "Sex of caregiver",
                              "Escolaridade_mae_cat"  = "Educational level of mother",
                              "Situação_Emprego_Cat" = "Employment status of mother",
                              "rendimento_familiar_cat2" = "Family income",
                              "Densidade_Habitacao_cat_1_5" = "Household density"
  )) 
dev.off()

########################################
# 4. ASSOCIATION ANALYSES - with Pelo_menos_1_teste or Pelo_menos_1_positivo
#########################################
# 4.1. CONFIDENCE INTERVALS, ODDS RATIO, CHISQ
##########################################

### VARIABLE: Pelo_menos_1_teste
sink(file="./Routput_CI_OR_CHISQ_Pelo_menos_1_teste.txt", append=FALSE, type="output")

# total
t<-as.data.frame(table(df_criancas$Pelo_menos_1_teste))
t_reactive <- t %>% 
  spread(key=Var1, value=Freq) %>% 
  mutate(p_reactive=100*binconf(`1`,(`1`+ `0`)))
colnames(t_reactive)<-c("No_test","At_least_1_test","Percentage_at_least_1_test")
print(noquote("Total"))
print(t_reactive)

for(i in variables_association){
  print(noquote(i))
  t<-as.data.frame(table(df_criancas[,i]))
  p<-as.data.frame(100*prop.table(table(df_criancas[,i])))
  freq<-cbind(t,p[,"Freq"])
  colnames(freq)<-c(i,"N","%")
  
  t<-as.data.frame(table(df_criancas$Pelo_menos_1_teste,df_criancas[,i]))
  t_reactive <- t %>% 
    spread(key=Var1, value=Freq) %>% 
    mutate(Total=`0`+`1`) %>% 
    mutate(p_reactive=100*binconf(`1`,(`1`+ `0`)))
  colnames(t_reactive)<-c(i,"No_test","At_least_1_test","Total","Percentage_at_least_1_test")
  
  
  m<-as.matrix(table(df_criancas[,i],df_criancas$Pelo_menos_1_teste))
  m # "positive" has to come on second column (negative-0, positive-1); control in first row;
  if(any(m==0)){
    tbl<-cbind(freq,t_reactive)
    print(tbl)
    print("values of 0 in matrix - odds ratio not calculated")
    
  } else {
    or.out <- epitools::oddsratio(m)
    tbl<-cbind(freq,t_reactive,or.out$measure,or.out$p.value)
    print(tbl)
  }
  chisq<-chisq.test(m)
  print("chisq$p.value")
  print(chisq$p.value)
  
  fisher_test<-fisher.test(m, hybrid=TRUE, simulate.p.value = FALSE) # hybrid chi-square approximation for table larger than 2x2; Monte Carlo simulation for table larger than 2x2
  print("fisher_test$p.value")
  print(fisher_test$p.value)
}

sink()

### VARIABLE: Pelo_menos_1_positivo
table(df_criancas$Pelo_menos_1_positivo, useNA = "always")
table(df_criancas$Pelo_menos_1_positivo,df_criancas$Estatuto_Migrante_Crianca)
sink(file="./Routput_CI_OR_CHISQ_Pelo_menos_1_positivo.txt", append=FALSE, type="output")

# total
t<-as.data.frame(table(df_criancas$Pelo_menos_1_positivo))
t_reactive <- t %>% 
  spread(key=Var1, value=Freq)  %>% 
  mutate(p_reactive=100*binconf(`1`,(`1`+ `0`)))
colnames(t_reactive)<-c("No_positive","At_least_1_positive","Percentage_at_least_1_positive")
print(noquote("Total"))
print(t_reactive)


for(i in variables_association){
  print(noquote(i))
  t<-as.data.frame(table(df_criancas[,i]))
  p<-as.data.frame(100*prop.table(table(df_criancas[,i])))
  freq<-cbind(t,p[,"Freq"])
  colnames(freq)<-c(i,"N","%")
  
  t<-as.data.frame(table(df_criancas$Pelo_menos_1_positivo,df_criancas[,i]))
  t_reactive <- t %>% 
    spread(key=Var1, value=Freq) %>% 
    mutate(Total=`0`+`1`) %>% 
    mutate(p_reactive=100*binconf(`1`,(`1`+ `0`)))
  colnames(t_reactive)<-c(i,"No_positive","At_least_1_positive","Total","Percentage_at_least_1_positive")

  m<-as.matrix(table(df_criancas[,i],df_criancas$Pelo_menos_1_positivo))
  m # "positive" has to come on second column (negative-0, positive-1); control in first row;
  if(any(m==0)){
    tbl<-cbind(freq,t_reactive)
    print(tbl)
    print("values of 0 in matrix - odds ratio not calculated")
    
  } else {
    or.out <- epitools::oddsratio(m)
    tbl<-cbind(freq,t_reactive,or.out$measure,or.out$p.value)
    print(tbl)
  }
  chisq<-chisq.test(m)
  print("chisq$p.value")
  print(chisq$p.value)
  
  fisher_test<-fisher.test(m, hybrid=TRUE, simulate.p.value = FALSE) # hybrid chi-square approximation for table larger than 2x2; Monte Carlo simulation for table larger than 2x2
  print("fisher_test$p.value")
  print(fisher_test$p.value)
}

sink()


##################################
# ASSOCIATON - only imigrants

variables_migrants <- c("Pelo_menos_1_teste","Pelo_menos_1_positivo","Imig_1ª_ger","pais_crianca_1_ger",
                        "Pais_Cuidador","Lingua_falada_em_casa_2","mae_causas_imigracao___1")


table(df_criancas$pais_crianca_1_ger)
table(df_criancas$Lingua_falada_em_casa_2)
table(df_criancas$Pais_Cuidador)

df_migrants <- df_criancas %>%
  filter(Estatuto_Migrante_Crianca == "1") %>%
  dplyr::select(all_of(variables_migrants))

table(df_migrants$pais_crianca_1_ger)
table(df_migrants$Imig_1ª_ger, df_migrants$pais_crianca_1_ger)

df_migrants$pais_crianca_1_ger[df_migrants$pais_crianca_1_ger %in% c("Bielorrússia","Cabo Verde", "EUA",
                                                                     "Guiné Bissau", "Índia", "Malawi", "Namíbia",
                                                                     "Panamá", "Paquistão","Senegal","Venezuela")]<-"others"

df_migrants$pais_crianca_1_ger[df_migrants$pais_crianca_1_ger %in% c("Espanha", "Franca","Portugal","Reino Unido")] <- NA
                                                                     
table(df_migrants$pais_crianca_1_ger)


### VARIABLE: Pelo_menos_1_teste
sink(file="./Routput_CI_OR_CHISQ_Pelo_menos_1_teste_IMMIGRANTS.txt", append=FALSE, type="output")

# total
t<-as.data.frame(table(df_migrants$Pelo_menos_1_teste))
t_reactive <- t %>% 
  spread(key=Var1, value=Freq) %>% 
  mutate(p_reactive=100*binconf(`1`,(`1`+ `0`)))
colnames(t_reactive)<-c("No_test","At_least_1_test","Percentage_at_least_1_test")
print(noquote("Total"))
print(t_reactive)

for(i in variables_migrants){
  print(noquote(i))
  t<-as.data.frame(table(df_migrants[,i]))
  p<-as.data.frame(100*prop.table(table(df_migrants[,i])))
  freq<-cbind(t,p[,"Freq"])
  colnames(freq)<-c(i,"N","%")
  
  t<-as.data.frame(table(df_migrants$Pelo_menos_1_teste,df_migrants[,i]))
  t_reactive <- t %>% 
    spread(key=Var1, value=Freq) %>% 
    mutate(Total=`0`+`1`) %>% 
    mutate(p_reactive=100*binconf(`1`,(`1`+ `0`)))
  colnames(t_reactive)<-c(i,"No_test","At_least_1_test","Total","Percentage_at_least_1_test")
  
  
  m<-as.matrix(table(df_migrants[,i],df_migrants$Pelo_menos_1_teste))
  m # "positive" has to come on second column (negative-0, positive-1); control in first row;
  if(any(m==0)){
    tbl<-cbind(freq,t_reactive)
    print(tbl)
    print("values of 0 in matrix - odds ratio not calculated")
    
  } else {
    or.out <- epitools::oddsratio(m)
    tbl<-cbind(freq,t_reactive,or.out$measure,or.out$p.value)
    print(tbl)
  }
  chisq<-chisq.test(m)
  print("chisq$p.value")
  print(chisq$p.value)
  
  fisher_test<-fisher.test(m, hybrid=TRUE, simulate.p.value = FALSE) # hybrid chi-square approximation for table larger than 2x2; Monte Carlo simulation for table larger than 2x2
  print("fisher_test$p.value")
  print(fisher_test$p.value)
}

sink()

### VARIABLE: Pelo_menos_1_positivo
table(df_migrants$Pelo_menos_1_positivo, useNA = "always")
table(df_migrants$Pelo_menos_1_positivo,df_migrants$Estatuto_Migrante_Crianca)

sink(file="./Routput_CI_OR_CHISQ_Pelo_menos_1_positivo_IMMIGRANTS.txt", append=FALSE, type="output")

# total
t<-as.data.frame(table(df_migrants$Pelo_menos_1_positivo))
t_reactive <- t %>% 
  spread(key=Var1, value=Freq)  %>% 
  mutate(p_reactive=100*binconf(`1`,(`1`+ `0`)))
colnames(t_reactive)<-c("No_positive","At_least_1_positive","Percentage_at_least_1_positive")
print(noquote("Total"))
print(t_reactive)


for(i in variables_migrants){
  print(noquote(i))
  t<-as.data.frame(table(df_migrants[,i]))
  p<-as.data.frame(100*prop.table(table(df_migrants[,i])))
  freq<-cbind(t,p[,"Freq"])
  colnames(freq)<-c(i,"N","%")
  
  t<-as.data.frame(table(df_migrants$Pelo_menos_1_positivo,df_migrants[,i]))
  t_reactive <- t %>% 
    spread(key=Var1, value=Freq) %>% 
    mutate(Total=`0`+`1`) %>% 
    mutate(p_reactive=100*binconf(`1`,(`1`+ `0`)))
  colnames(t_reactive)<-c(i,"No_positive","At_least_1_positive","Total","Percentage_at_least_1_positive")
  
  m<-as.matrix(table(df_migrants[,i],df_migrants$Pelo_menos_1_positivo))
  m # "positive" has to come on second column (negative-0, positive-1); control in first row;
  if(any(m==0)){
    tbl<-cbind(freq,t_reactive)
    print(tbl)
    print("values of 0 in matrix - odds ratio not calculated")
    
  } else {
    or.out <- epitools::oddsratio(m)
    tbl<-cbind(freq,t_reactive,or.out$measure,or.out$p.value)
    print(tbl)
  }
  chisq<-chisq.test(m)
  print("chisq$p.value")
  print(chisq$p.value)
  
  fisher_test<-fisher.test(m, hybrid=TRUE, simulate.p.value = FALSE) # hybrid chi-square approximation for table larger than 2x2; Monte Carlo simulation for table larger than 2x2
  print("fisher_test$p.value")
  print(fisher_test$p.value)
}

sink()


##### MEDIANA DE TEMPO DE RESIDÊNCIA DOS IMIRANTES 1ª geracao
variables_migrants_time <- c("Pelo_menos_1_teste","Pelo_menos_1_positivo","tempo_residencia_portugal_crianca")

df_migrants_time <- df_criancas %>%
  filter(Estatuto_Migrante_Crianca == "1") %>%
  filter(Imig_1ª_ger == "1") %>%
  dplyr::select(all_of(variables_migrants_time))

table(df_migrants_time$tempo_residencia_portugal_crianca)
df_migrants_time %>%
   summarise(
    Count = sum(!is.na(tempo_residencia_portugal_crianca)), 
    Median = median(tempo_residencia_portugal_crianca, na.rm = TRUE),
    Min = min(tempo_residencia_portugal_crianca, na.rm = TRUE),
    Max = max(tempo_residencia_portugal_crianca, na.rm = TRUE))

# - TESTADOS E NÃO TESTADO
grouped_stats <- df_migrants_time %>%
  group_by(Pelo_menos_1_teste) %>%
  summarise(
    Count = sum(!is.na(tempo_residencia_portugal_crianca)), 
    Median = median(tempo_residencia_portugal_crianca, na.rm = TRUE),
    Min = min(tempo_residencia_portugal_crianca, na.rm = TRUE),
    Max = max(tempo_residencia_portugal_crianca, na.rm = TRUE))
    
grouped_stats

wilcox.test(tempo_residencia_portugal_crianca ~ Pelo_menos_1_teste, data = df_migrants_time) 
boxplot(tempo_residencia_portugal_crianca ~ Pelo_menos_1_teste, data = df_migrants_time)
 
# - POSITVOS E NÃO POSITIVOS

grouped_stats <- df_migrants_time %>%
  group_by(Pelo_menos_1_positivo) %>%
  summarise(
    Count = sum(!is.na(tempo_residencia_portugal_crianca)), 
    Median = median(tempo_residencia_portugal_crianca, na.rm = TRUE),
    Min = min(tempo_residencia_portugal_crianca, na.rm = TRUE),
    Max = max(tempo_residencia_portugal_crianca, na.rm = TRUE))

grouped_stats

df_migrants_time_without_na=df_migrants_time[which(!is.na(df_migrants_time$tempo_residencia_portugal_crianca) & !is.na(df_migrants_time$Pelo_menos_1_positivo)),]

wilcox.test(tempo_residencia_portugal_crianca ~ na.omit(Pelo_menos_1_positivo), data = df_migrants_time_without_na) 
boxplot(tempo_residencia_portugal_crianca ~ Pelo_menos_1_positivo, data = df_migrants_time)


#########################################
#########################################
# 5. BIVARIATE REGRESSION MODELS
##########################################

variables_regression<- c("Estatuto_Migrante_Crianca", "sexo_crianca", "Coorte",
                         "duracao_gravidez_cat", "peso_nascenca_cat", "mae_amamentou_crianca",
                         "Local_criança_dia_cat",
                         "sexo_cuidador","Escolaridade_mae_cat","Situação_Emprego_Cat","rendimento_familiar_cat2",
                         "Densidade_Habitacao_cat_1_5",
                         "concelho_residencia_cat"
                         )



### DEPENDENT VARIABLE: Pelo_menos_1_teste - Robust poisson ()

sink(file="./Routput_Robust_Poisson_regression_Pelo_menos_1_teste.txt", append=FALSE, type="output")

# Poisson regression model with robust standard errors

print("### ROBUST POISSON REGRESSION - dependent: Pelo_menos_1_teste")

for(i in variables_regression){
  
  variables_list<-c(i,"Pelo_menos_1_teste")
  df_regress<-df_criancas
  df_regress<-df_regress[,variables_list]
  print("VARIABLES")
  print(colnames(df_regress))
  
  df_regress<-na.omit(df_regress) #remove rows with NAs
  if (i %in% variables_all_cat) {
    df_regress[,i]<-as.factor(df_regress[,i])
  } 
  df_regress<-as.data.frame(df_regress)
  
  nrow(df_regress)
  summary(df_regress)
  
  set.seed(123)
  model <- glm(as.numeric(Pelo_menos_1_teste) ~ . , family = poisson(link = "log"), 
               data = df_regress)
  print("Poisson regression")
  print(summary(model))
  
  # Robust standard errors
  cov.model <- vcovHC(model, type="HC0") # HC0: Heteroskedasticity-consistent covariance matrix
  print("Robust Poisson regression")
  standard_error<-coeftest(model, vcov = cov.model)
  print(coeftest(model, vcov = cov.model))
  
  # or use:
  robust.std.err <- sqrt(diag(cov.model))
  robust.estimate <- cbind(Estimate= coef(model),
                           LL = coef(model) - 1.96 * robust.std.err,
                           UL = coef(model) + 1.96 * robust.std.err,
                           "Robust SE" = robust.std.err,
                           "z value" = coef(model)/robust.std.err,
                           "Pr(>|z|)" = 2 * pnorm(abs(coef(model)/robust.std.err), lower.tail=FALSE)
  )
  print("Robust Poisson regression with 95% CI of the estimate")
  print(robust.estimate)
  
  print("Prevalence ratio and CI 95%")
  pr <- exp(coef(model))  
  ic_inf <- exp(coef(model) - 1.96 * robust.std.err)  
  ic_sup <- exp(coef(model) + 1.96 * robust.std.err) 
  
  result <- data.frame(
    Variable = names(pr),
    PR = pr,
    IC_95_inf = ic_inf,
    IC_95_sup = ic_sup
  )
  
  print(result)
  
}

sink()

### DEPENDENT VARIABLE: Pelo_menos_1_positivo - ROBUST POISSON REGRESSION

sink(file="./Routput_Robust_Poisson_regression_Pelo_menos_1_positivo.txt", append=FALSE, type="output")

# Poisson regression model with robust standard errors

print("### ROBUST POISSON REGRESSION - dependent: Pelo_menos_1_positivo")

for(i in variables_regression){
  
  variables_list<-c(i,"Pelo_menos_1_positivo")
  df_regress<-df_criancas
  df_regress<-df_regress[,variables_list]
  print("VARIABLES")
  print(colnames(df_regress))
  
  df_regress<-na.omit(df_regress) #remove rows with NAs
  if (i %in% variables_all_cat) {
    df_regress[,i]<-as.factor(df_regress[,i])
  } 
  df_regress<-as.data.frame(df_regress)
  
  nrow(df_regress)
  summary(df_regress)
  
  set.seed(123)
  model <- glm(as.numeric(Pelo_menos_1_positivo) ~ . , family = poisson(link = "log"), 
               data = df_regress)
  print("Poisson regression")
  print(summary(model))
  
  # Robust standard errors
  cov.model <- vcovHC(model, type="HC0") # HC0: Heteroskedasticity-consistent covariance matrix
  print("Robust Poisson regression")
  standard_error<-coeftest(model, vcov = cov.model)
  print(coeftest(model, vcov = cov.model))
  
  # or use:
  robust.std.err <- sqrt(diag(cov.model))
  robust.estimate <- cbind(Estimate= coef(model),
                           LL = coef(model) - 1.96 * robust.std.err,
                           UL = coef(model) + 1.96 * robust.std.err,
                           "Robust SE" = robust.std.err,
                           "z value" = coef(model)/robust.std.err,
                           "Pr(>|z|)" = 2 * pnorm(abs(coef(model)/robust.std.err), lower.tail=FALSE)
  )
  print("Robust Poisson regression with 95% CI of the estimate")
  print(robust.estimate)
  
  print("Prevalence ratio and CI 95%")
  pr <- exp(coef(model))  
  ic_inf <- exp(coef(model) - 1.96 * robust.std.err)  
  ic_sup <- exp(coef(model) + 1.96 * robust.std.err) 
  
  result <- data.frame(
    Variable = names(pr),
    PR = pr,
    IC_95_inf = ic_inf,
    IC_95_sup = ic_sup
  )
  
  print(result)
  
  
}

sink()

###############################################################
# 6. MULTIVARIABLE REGRESSION 
##############################################################
table(df_criancas$Coorte)
table(df_criancas$Densidade_Habitacao_cat_1_5)
##### Pelo_menos_1_teste

variables_multivariable<-c("Pelo_menos_1_teste","Estatuto_Migrante_Crianca", "Coorte",
                           "tempo_amament_exlusiva_cat", "Local_criança_dia_cat",
                             "Situação_Emprego_Cat",
                           "Densidade_Habitacao_cat_1_5")


variables_multivariable_cat<-c("Pelo_menos_1_teste","Estatuto_Migrante_Crianca", "Coorte",
                               "tempo_amament_exlusiva_cat", "Local_criança_dia_cat",
                               "Situação_Emprego_Cat",
                               "Densidade_Habitacao_cat_1_5")

sink(file="./Routput_Robust_Poisson_MULTIVARIABLE_regression_Pelo_menos_1_teste.txt", append=FALSE, type="output")

print("### MULTIVARIABLE ROBUST POISSON REGRESSION - dependent: Pelo_menos_1_teste")

for(i in list(variables_multivariable)){
  
  df_regress<-df_criancas
  df_regress<-df_regress[,i]
  print(colnames(df_regress))
  
  df_regress<-na.omit(df_regress) #remove rows with NAs
  for(i in variables_multivariable) {
    df_regress[,i]<-as.factor(df_regress[,i])
  } 
  df_regress<-as.data.frame(df_regress)
  print(nrow(df_regress))
  
  nrow(df_regress)
  summary(df_regress)
  
  model <- glm(as.numeric(Pelo_menos_1_teste) ~ ., 
               data = df_regress, family = poisson(link = "log"))
  
  print(summary(model))
  
  print("VIF values")
  vif_values <- vif(model)
  print(vif_values)
  
  # Robust standard errors
  cov.model <- vcovHC(model, type="HC0") # HC0: Heteroskedasticity-consistent covariance matrix
  print("Robust Poisson regression")
  print(coeftest(model, vcov = cov.model))
  
  # or use:
  robust.std.err <- sqrt(diag(cov.model))
  robust.estimate <- cbind(Estimate= coef(model),
                           LL = coef(model) - 1.96 * robust.std.err,
                           UL = coef(model) + 1.96 * robust.std.err,
                           "Robust SE" = robust.std.err,
                           "z value" = coef(model)/robust.std.err,
                           "Pr(>|z|)" = 2 * pnorm(abs(coef(model)/robust.std.err), lower.tail=FALSE)
  )
  print("Robust Poisson regression with 95% CI of the estimate")
  print(robust.estimate)                     
  
  print("Prevalence ratio and CI 95%")
  pr <- exp(coef(model))  
  ic_inf <- exp(coef(model) - 1.96 * robust.std.err)  
  ic_sup <- exp(coef(model) + 1.96 * robust.std.err) 
  
  result <- data.frame(
    Variable = names(pr),
    PR = pr,
    IC_95_inf = ic_inf,
    IC_95_sup = ic_sup
  )
  
  print(result)
  print("### Hosmer and Lemeshow goodness of fit (GOF) test")
  
  print(hoslem.test(model$y,fitted(model),g=10)$p.value)
  
}


sink()


##### Pelo_menos_1_positivo

variables_multivariable<-c("Pelo_menos_1_positivo","Estatuto_Migrante_Crianca", 
                           "peso_nascenca_cat","Situação_Emprego_Cat","rendimento_familiar_cat2",
                           "Densidade_Habitacao_cat_1_5")


variables_multivariable_cat<-c("Pelo_menos_1_positivo","Estatuto_Migrante_Crianca", 
                               "peso_nascenca_cat","Situação_Emprego_Cat","rendimento_familiar_cat2",
                               "Densidade_Habitacao_cat_1_5")




sink(file="./Routput_Robust_Poisson_MULTIVARIABLE_regression_Pelo_menos_1_positivo.txt", append=FALSE, type="output")

print("### MULTIVARIABLE ROBUST POISSON REGRESSION - dependent: Pelo_menos_1_positivo")

for(i in list(variables_multivariable)){
  
  df_regress<-df_criancas
  df_regress<-df_regress[,i]
  print(colnames(df_regress))
  
  df_regress<-na.omit(df_regress) #remove rows with NAs
  for(i in variables_multivariable_cat) {
    df_regress[,i]<-as.factor(df_regress[,i])
  } 
  df_regress<-as.data.frame(df_regress)
  print(nrow(df_regress))
  
  nrow(df_regress)
  summary(df_regress)
  
  model <- glm(as.numeric(Pelo_menos_1_positivo) ~ ., 
               data = df_regress, family = poisson(link = "log"))
  
  print(summary(model))
  
  print("VIF values")
  vif_values <- vif(model)
  print(vif_values)

  # Robust standard errors
  cov.model <- vcovHC(model, type="HC0") # HC0: Heteroskedasticity-consistent covariance matrix
  print("Robust Poisson regression")
  print(coeftest(model, vcov = cov.model))
  
  # or use:
  robust.std.err <- sqrt(diag(cov.model))
  robust.estimate <- cbind(Estimate= coef(model),
                           LL = coef(model) - 1.96 * robust.std.err,
                           UL = coef(model) + 1.96 * robust.std.err,
                           "Robust SE" = robust.std.err,
                           "z value" = coef(model)/robust.std.err,
                           "Pr(>|z|)" = 2 * pnorm(abs(coef(model)/robust.std.err), lower.tail=FALSE)
  )
  print("Robust Poisson regression with 95% CI of the estimate")
  print(robust.estimate)                     
  
  print("Prevalence ratio and CI 95%")
  pr <- exp(coef(model))  
  ic_inf <- exp(coef(model) - 1.96 * robust.std.err)  
  ic_sup <- exp(coef(model) + 1.96 * robust.std.err) 
  
  result <- data.frame(
    Variable = names(pr),
    PR = pr,
    IC_95_inf = ic_inf,
    IC_95_sup = ic_sup
  )
  
  print(result)
  
  
  print("### Hosmer and Lemeshow goodness of fit (GOF) test")
  
  print(hoslem.test(model$y,fitted(model),g=10)$p.value)
  
}


sink()

####################################################
# 6.1. Forest plots - Prevalence ratio Multivariable Poisson regression
####################################################

jpeg(file = "./forest_plot_PR.jpg", bg="white", antialias = "default",
     width = 8, height = 8,  
     units = "in", res = 300)

t<-read.xlsx("./PR_table_pelo_menos_1_teste.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
colnames(t)
t$Category

t$Category<-factor(t$Category, levels=rev(c("Migratory status (Immigrant compared to Non-immigrant)",
                                            "Cohort (2020 compared to 2018)",
                                            "Breastfed (No compared to Yes)",
                                            "Day spent at (Home compared to School)",
                                            "Occupational status of mother (Employed compared to Non-employed)",
                                            "Household density (> 1.5 compared to <= 1.5)")))

p1 <- ggplot(data= t, aes(x=Category,y=PR)) + 
  geom_point(aes(x=Category,y=PR),position=position_dodge(width=0.8)) +
  geom_errorbar(aes(x=Category,ymin=IC_95_inf, ymax=IC_95_sup), 
                width=0.2, size=0.5,position=position_dodge(width=0.8))+
  geom_hline(yintercept = 1,linetype = 'dotted') +
  ylim(0.7, 1.3) +
  xlab("") + ylab("Prevalence ratio") +
  ggtitle("")+
  coord_flip() + 
  scale_colour_grey(start = 0.7, end = 0) +
  labs(tag = "A") +
  theme_bw()

t<-read.xlsx("./PR_table_pelo_menos_1_positivo.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
colnames(t)
t$Category

t$Category<-factor(t$Category, levels=rev(c("Migratory status (Immigrant compared to Non-immigrant)",
                                            "Weight at birth (>= 2.5 compared to < 2.5 Kg)" ,
                                            "Occupational status of mother (Employed compared to Non-employed)",
                                            "Family income (> 1000 compared to <= 1000 euros)",
                                            
                                            "Household density (> 1.5 compared to <= 1.5)")))


p2 <- ggplot(data= t, aes(x=Category,y=as.numeric(PR))) + 
  geom_point(aes(x=Category,y=as.numeric(PR)),position=position_dodge(width=0.8)) +
  geom_errorbar(aes(x=Category,ymin=as.numeric(IC_95_inf), ymax=as.numeric(IC_95_sup)), 
                width=0.2, size=0.5,position=position_dodge(width=0.8))+
  geom_hline(yintercept = 1, linetype = 'dotted') +
  ylim(0.7, 1.3) +
  xlab("") + ylab("Prevalence ratio") +
  ggtitle("")+
  coord_flip() + 
  scale_colour_grey(start = 0.7, end = 0) +
  labs(tag = "B") +
  theme_bw()


plot_grid(p1, p2, nrow = 2,rel_heights = c(1, 1)) 

dev.off()
####################################################################################################
####################################################################################################
# 7.  Analyses of dataframe of COVID test results (some children have several tests)
####################################################################################################

##################
# 7.1 IMPORT DATAFRAME of COVID test results 

df_covid<-read.xlsx("./", sheet = 1, startRow = 1, colNames = TRUE)
nrow(df_covid)
glimpse(df_covid)
df_covid[,"Birth_Date"] <- openxlsx::convertToDateTime(df_covid[,"Birth_Date"]) # Convert numeric to Date format
df_covid[,"Test_Date"] <- openxlsx::convertToDateTime(df_covid[,"Test_Date"]) # Convert numeric to Date format
glimpse(df_covid)

# Check duplicated rows
table(duplicated(df_covid)) # there are 186 rows that have duplicates

duplicated_rows <- df_covid |>
  group_by_all() |>
  filter(n() > 1) |>
  ungroup()
nrow(duplicated_rows)
write.xlsx(duplicated_rows, "./duplicate_rows.xlsx", fileEncoding = "latin1")

# keep only unique/distinct rows (keep the first row)
df_covid <- df_covid %>%
  distinct()
nrow(df_covid)

colnames(df_covid)
glimpse(df_covid)

length(unique(df_covid$Record_id))

##################
# 7.2 ADD NEW VARIABLES TO DATAFRAME

# Add Migrant status
colnames(df_criancas)
colnames(df_covid)

df_covid <- df_covid %>%
  left_join(df_criancas %>% dplyr::select(record_id, Estatuto_Migrante_Crianca), by = c("Record_id" = "record_id"))

# FLOOR DATE (first day of the week, month...)
df_covid$day_test<-floor_date(df_covid$Test_Date, "day")
df_covid$week_test<-floor_date(df_covid$Test_Date, "week")
df_covid$month_test<-floor_date(df_covid$Test_Date, "month")
df_covid$year_test<-as.numeric(format(floor_date(df_covid$Test_Date, "year"),'%Y'))
table(df_covid$week_test)

# number of the week in the year
df_covid$number_year_week <- paste0(df_covid$year_test, ".", strftime(df_covid$week_test, format = "%U"))

# age at time of test (in years)
df_covid$age_test <- as.numeric(df_covid$day_test - df_covid$Birth_Date) %/% 365.25


# PLOTS TO CHECK - number of tests by result, by date, by status
ggplot(data= df_covid, aes(x=week_test, fill=Test_Result)) + 
  geom_bar(position="stack") +
  facet_wrap(~ Estatuto_Migrante_Crianca, ncol=1) +  # Create a panel for each variable
  theme_light()

ggplot(data= df_covid, aes(x=month_test, fill=Test_Result)) + 
  geom_bar(position="stack") +
  facet_wrap(~ Estatuto_Migrante_Crianca, ncol=1) +  # Create a panel for each variable
  theme_light()

# ADD Resultado_binario
table(df_covid$Test_Result,useNA="always")
df_covid$Test_Result[df_covid$Test_Result %in% c("Indeterminado")]<-NA
table(df_covid$Test_Result,useNA="always")

df_covid$Result_binary[df_covid$Test_Result %in% c("Positivo")]<-1
df_covid$Result_binary[df_covid$Test_Result %in% c("Negativo")]<-0
table(df_covid$Result_binary,useNA="always")


##################
# WRITE TO EXCEL FILE
write.xlsx(df_covid, "./df_covid.xlsx", fileEncoding = "latin1")


#############
# 7.3. ASSOCIATION OF MIGRANT STATUS AND TEST TYPE (Antigen or PCR)
colnames(df_covid)
table(df_covid$Type_Test)

sink(file="./Routput_CI_OR_CHISQ_TestType.txt", append=FALSE, type="output")

for(i in "Type_Test"){
  print(noquote(i))
  
  t<-as.data.frame(table(df_covid$Estatuto_Migrante_Crianca))
  p<-as.data.frame(100*prop.table(table(df_covid$Estatuto_Migrante_Crianca)))
  freq<-cbind(t,p[,"Freq"])
  colnames(freq)<-c("Estatuto_Migrante_Crianca","N","%")
  
  t<-as.data.frame(table(df_covid[,i],df_covid$Estatuto_Migrante_Crianca))
  t_reactive <- t %>% 
    spread(key=Var1, value=Freq) %>% 
    mutate(p_reactive=100*binconf(`RT PCR`,(`RT PCR`+ `Imunocromatografia`)))
  colnames(t_reactive)<-c("Migrant_status","Rapid Antigen Test","RT-PCR","Percentage_RT-PCR")
  
 
  m<-as.matrix(table(df_covid$Estatuto_Migrante_Crianca,df_covid[,i]))
  m # "positive" has to come on second column (negative-0, positive-1); control in first row;
  if(any(m==0)){
    tbl<-cbind(freq,t_reactive)
    print(tbl)
    print("values of 0 in matrix - odds ratio not calculated")
    
  } else {
    or.out <- epitools::oddsratio(m)
    tbl<-cbind(freq,t_reactive,or.out$measure,or.out$p.value)
    print(tbl)
  }
  chisq<-chisq.test(m)
  print("chisq$p.value")
  print(chisq$p.value)
  
  fisher_test<-fisher.test(m, hybrid=TRUE, simulate.p.value = FALSE) # hybrid chi-square approximation for table larger than 2x2; Monte Carlo simulation for table larger than 2x2
  print("fisher_test$p.value")
  print(fisher_test$p.value)
}

sink()


########################
# 7.4 SARS-CoV-2 variants

# From: https://insaflu.insa.pt/covid19/
# LX V Tejo
# In the variants excel the first weeks missing (2020 and begining 2021) were added with NA in the "variant" variable
df_variants<-read.xlsx("./Frequencia variantes sars-cov-2_lX V TEJO_v2.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
colnames(df_variants)
glimpse(df_variants)

levels(factor(df_variants$variant))
df_variants$variant[df_variants$variant %in% c("Delta ")]<-"Delta"

# REMOVE WEEKS AT THE END NOT PRESENT IN df_covid
first_date_df <- head(df_covid[order(df_covid$number_year_week),]["number_year_week"],1)[1,1]
last_date_df <- tail(df_covid[order(df_covid$number_year_week),]["number_year_week"],1)[1,1]
first_date_df
last_date_df

table(df_covid$number_year_week)

first_date_variants <- head(df_variants$number_year_week,1)
last_date_variants <- tail(df_variants$number_year_week,1)
first_date_variants
last_date_variants

#### Filter df_variants to exclude weeks not present in df_covid
df_variants <- subset(df_variants, as.numeric(number_year_week) <= as.numeric(last_date_df))


png(file = "./plot_variants_per_week.png", bg="white", width = 15, height = 5,  
    units = "in", res = 200)

# Calculate relative frequencies
df_variants_relative <- df_variants %>%
  group_by(factor(number_year_week)) %>%
  mutate(relative_freq = n / sum(n)) %>%
  ungroup()

extended_palette <- colorRampPalette(brewer.pal(12, "Set3"))(12)  # Interpolate to 14 colors

ggplot(df_variants_relative, aes(x = factor(number_year_week), y = relative_freq, fill = variant)) +
  geom_bar(stat = "identity") +  
  xlab("Week") + ylab("Relative frequency") +
   theme_light() +
  theme(axis.text=element_text(size=5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=extended_palette,
                    name="SARS-CoV-2 variants") 

dev.off()

df_dominant_variant <- df_variants %>% 
  group_by(number_year_week) %>%
  slice_max(n, with_ties = FALSE) %>%  # Select the row with the maximum n for each group
  ungroup()  # Remove grouping if needed

colnames(df_dominant_variant)<- c("number_year_week","Dominant_variant","n")



ggplot(df_dominant_variant, aes(x = factor(number_year_week), y = Dominant_variant, fill = n)) +
  geom_tile(color = "white") +  # Add borders between tiles
  scale_fill_gradient(low = "white", high = "blue") +  # Customize the color scale
  labs(
    title = "Heatmap of Variants per Week",
    x = "Week",
    y = "Variant",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
  

##################################################################
##################################################################
# 7.5.PLOT NUMBER OF TESTS PER DATE, PER RESULT, PER STATUS WITH PEAKS OF WAVES
##################################################################
# PER WEEK

###############
# DATES OF PEAKS OF COVID WAVES
levels(factor(df_covid$week_test))

first_wave<-  ISOdate(year = 2020, month = 03, day = 23)
second_wave<-ISOdate(year = 2020, month = 11, day = 16)
third_wave<-ISOdate(year = 2021, month = 01, day = 18)
fourth_wave<-ISOdate(year = 2021, month = 07, day = 12)
fifth_wave<-ISOdate(year = 2022, month = 01, day = 24)
sixth_wave<-ISOdate(year = 2022, month = 05, day = 15)

colnames(df_covid)

png(file = "./plot_result_test_week_by_status.png", bg="white", width = 5, height = 3,  
    units = "in", res = 200)

migrant_status <- as_labeller(
  c(`1` = "Immigrant children", `0` = "Non-immigrant children"))
p1 <- df_covid %>% 
  drop_na(Test_Result) %>%
  ggplot(aes(x=week_test, fill=Test_Result)) + 
  geom_bar(position="stack") +
  geom_segment(aes(x = first_wave, xend = first_wave, y = 80, yend = 79),
               color = "#f28500", linewidth = 0.4,
               arrow = arrow(length = unit(0.1, "inches"), type = "closed", ends = "last", angle = 15)) +
  geom_segment(aes(x = second_wave, xend = second_wave, y = 80, yend = 79),
               color = "#f28500", linewidth = 0.4,
               arrow = arrow(length = unit(0.1, "inches"), type = "closed", ends = "last", angle = 15)) +
  geom_segment(aes(x = third_wave, xend = third_wave, y = 80, yend = 79),
               color = "#f28500", linewidth = 0.4,
               arrow = arrow(length = unit(0.1, "inches"), type = "closed", ends = "last", angle = 15)) +
  geom_segment(aes(x = fourth_wave, xend = fourth_wave, y = 80, yend = 79),
               color = "#f28500", linewidth = 0.4,
               arrow = arrow(length = unit(0.1, "inches"), type = "closed", ends = "last", angle = 15)) +
  geom_segment(aes(x = fifth_wave, xend = fifth_wave, y = 80, yend = 79),
               color = "#f28500", linewidth = 0.4,
               arrow = arrow(length = unit(0.1, "inches"), type = "closed", ends = "last", angle = 15)) +
  geom_segment(aes(x = sixth_wave, xend = sixth_wave, y = 80, yend = 79),
               color = "#f28500", linewidth = 0.4,
               arrow = arrow(length = unit(0.1, "inches"), type = "closed", ends = "last", angle = 15)) +
  
  coord_cartesian(clip = "off") +

  
  scale_y_continuous(expand = c(0, 0)) +
  
  facet_wrap(~ Estatuto_Migrante_Crianca, ncol=1, labeller = migrant_status) +  # Create a panel for each variable
  xlab("") + ylab("Number of tests") +
  theme_light() +
  theme(legend.position = "top") +
  labs(tag = "A") +
  scale_fill_manual(values=c("#f3d9ba",
                             "#92add0"),
                    labels=c("Negative", "Positive"),
                    name="Test result") 
p1
dev.off()

p2 <- ggplot(df_variants_relative, aes(x = factor(number_year_week), y = relative_freq, fill = variant)) +
  geom_bar(stat = "identity") +  
  xlab("Week") + ylab("Relative\n frequency") +
  theme_light() +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank()) +
  theme(legend.position = "bottom") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=extended_palette,
                    name="") +
  labs(tag = "B")

png(file = "./plot_result_test_week_by_status_variants.png", bg="white", width = 10, height = 8,  
    units = "in", res = 200)

plot_grid(p1, p2, nrow = 2,rel_heights = c(1.5, 1)) 

dev.off()

##### BY Migrant and BY Test_Type
png(file = "./plot_result_test_week_by_status_by_TestType.png", bg="white", width = 8, height = 10,  
    units = "in", res = 200)

labeller_names <- as_labeller(
  c(`1` = "Immigrant children", `0` = "Non-immigrant children", 
    `Imunocromatografia` ="Rapid antigen test", `RT PCR` ="RT-PCR"))

df_covid %>% 
  drop_na(Test_Result) %>%
  ggplot(aes(x=week_test, fill=Test_Result)) + 
  geom_bar(position="stack") +
  
    scale_y_continuous(expand = c(0, 0)) +
  
  facet_wrap(~ Estatuto_Migrante_Crianca + Type_Test, ncol=1,labeller = labeller_names) +  # Create a panel for each variable
  xlab("") + ylab("Number of tests") +
  theme_light() +
  theme(legend.position = "top") +
  labs(tag = "A") +
  scale_fill_manual(values=c("#f3d9ba",
                             "#92add0"),
                    labels=c("Negative", "Positive"),
                    name="Test result") 
dev.off()

######################
# PLOT NUMBER OF TESTS PER DATE, PER RESULT, PER STATUS WITH MEASURES-TESTS AND MEASURES-SCHOOLS
# PER WEEK

measure_1 <-ISOdate(year = 2020, month = 03, day = 09) # "COVID-19 Diagnosis: RT-PCR"
measure_2 <-ISOdate(year = 2020, month = 05, day = 27) # "RT-PCR & Rapid Antigen Tests"
measure_3 <-ISOdate(year = 2021, month = 03, day = 12) # "Self-use Rapid Antigen Tests enter market"
measure_4 <-ISOdate(year = 2021, month = 06, day = 30) # "Free Rapid Antigen Tests in pharmacies"

measure_5 <-ISOdate(year = 2020, month = 03, day = 13) # "First school closure"
measure_6 <-ISOdate(year = 2020, month = 05, day = 18) # "Phased school reopening: Childcare"
measure_7 <-ISOdate(year = 2020, month = 06, day = 01) # "Phased school reopening: Preschool"
measure_8 <-ISOdate(year = 2021, month = 01, day = 22) # "Second school closure"
measure_9 <-ISOdate(year = 2021, month = 03, day = 15) # "Phased school reopening: Childcare & Preschool"


measure_10 <-ISOdate(year = 2020, month = 05, day = 08) # "Migrants have equal access to SNS"
measure_11 <-ISOdate(year = 2020, month = 07, day = 13) # "Target support for migrants"

png(file = "./plot_result_test_week_by_status_with_measuresTests_Schools.png", bg="white", width = 10, height = 8,  
    units = "in", res = 200)

migrant_status <- as_labeller(
  c(`1` = "Immigrant children", `0` = "Non-immigrant children"))
p1 <- df_covid %>% 
  drop_na(Test_Result) %>%
  ggplot(aes(x=week_test, fill=Test_Result)) + 
  geom_vline(aes(xintercept=c(measure_1)), linetype = "dashed", linewidth=0.5, color = "darkgrey") +
  geom_vline(aes(xintercept=c(measure_2)), linetype = "dashed", linewidth=0.5, color = "darkgrey") +
  geom_vline(aes(xintercept=c(measure_3)), linetype = "dashed", linewidth=0.5, color = "darkgrey") +
  geom_vline(aes(xintercept=c(measure_4)), linetype = "dashed", linewidth=0.5, color = "darkgrey") +
  geom_bar(position="stack") +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = c(0, 0)) +
  
  facet_wrap(~ Estatuto_Migrante_Crianca, ncol=1, labeller = migrant_status) +  # Create a panel for each variable
  xlab("") + ylab("Number of tests") +
  theme_light() +
  theme(legend.position = "top") +
  labs(tag = "A") +
  scale_fill_manual(values=c("#f3d9ba",
                             "#92add0"),
                    labels=c("Negative", "Positive"),
                    name="Test result") 

p2 <- df_covid %>% 
  drop_na(Test_Result) %>%
  ggplot(aes(x=week_test, fill=Test_Result)) + 
  geom_vline(aes(xintercept=c(measure_5)), linetype = "dashed", linewidth=0.5, color = "darkgrey") +
  geom_vline(aes(xintercept=c(measure_6)), linetype = "dashed", linewidth=0.5, color = "darkgrey") +
  geom_vline(aes(xintercept=c(measure_7)), linetype = "dashed", linewidth=0.5, color = "darkgrey") +
  geom_vline(aes(xintercept=c(measure_8)), linetype = "dashed", linewidth=0.5, color = "darkgrey") +
  geom_vline(aes(xintercept=c(measure_9)), linetype = "dashed", linewidth=0.5, color = "darkgrey") +
  geom_bar(position="stack") +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = c(0, 0)) +
  
  facet_wrap(~ Estatuto_Migrante_Crianca, ncol=1, labeller = migrant_status) +  # Create a panel for each variable
  xlab("") + ylab("Number of tests") +
  theme_light() +
  theme(legend.position = "top") +
  labs(tag = "B") +
  scale_fill_manual(values=c("#f3d9ba",
                             "#92add0"),
                    labels=c("Negative", "Positive"),
                    name="Test result") 
plot_grid(p1, p2, nrow = 2,rel_heights = c(1, 1)) 

dev.off()

geom_text(aes(x = measure_1, 
              y = 75,  # Ajustar a posição y dos textos
              label = "COVID-19 Diagnosis: RT-PCR"), 
          angle = 90, hjust = 1, vjust = 1.5, size = 2, color = "black")  

geom_text(aes(x = measure_1, 
              y = 100,  # Ajust position of y
              label = "1"), 
          angle = 0, hjust = 1, vjust = 1.5, size = 2, color = "black")  


#################
# TEST RESULT PER MONTH
png(file = "./plot_result_test_month_by_status.png", bg="white", width = 5, height = 3,  
    units = "in", res = 200)

migrant_status <- as_labeller(
  c(`1` = "Immigrant children", `0` = "Non-immigrant children"))
df_covid %>% 
  drop_na(Test_Result) %>%
  ggplot(aes(x=month_test, fill=Test_Result)) + 
  geom_bar(position="stack") +
  facet_wrap(~ Estatuto_Migrante_Crianca, ncol=1, labeller = migrant_status) +  # Create a panel for each variable
  xlab("Week") + ylab("Number of tests") +
  theme_light() +
  scale_fill_manual(values=c("#f3d9ba",
                             "#92add0"),
                    labels=c("Negative", "Positive"),
                    name="Test result") 
dev.off()


######################
# 7.6. TOTAL NUMBER OF TESTS AND OF POSITIVES PER CHILD

colnames(df_covid)
table(df_covid$Test_Result)
df_crianca_teste <- df_covid %>%
  group_by(Record_id) %>%
  summarise(Total_tests = n(),
            Total_positives = sum(Test_Result == "Positivo"),
            Migrant = first(Estatuto_Migrante_Crianca))

table(df_crianca_teste$Total_tests)

wilcox.test(Total_tests ~ Migrant, data = df_crianca_teste) 
boxplot(Total_tests ~ Migrant, data = df_crianca_teste)


png(file = "./plot_number_tests_by_status.png", bg="white", width = 8, height = 3,  
    units = "in", res = 200)
ggplot(df_crianca_teste,aes(x=Total_tests, fill=factor(Migrant))) + 
  geom_bar(stat="count", position="dodge") +
  xlab("Total number of tests performed") + ylab("Number of children") +
  theme_light() +
  scale_fill_manual(values=c("#92add0","#f3d9ba"),
                    labels=c("Non-immigrant","Immigrant"),
                    name="") +
  scale_x_continuous(breaks=seq(0,40,by=1))
dev.off()

######################
# PLOT PROPORTION OF POSITIVES PER DATE PER STATUS
# PER WEEK
png(file = "./plot_prop_positives_week_by_status.png", bg="white", width = 8, height = 5,  
    units = "in", res = 200)

migrant_status <- as_labeller(
  c(`1` = "Immigrant children", `0` = "Non-immigrant children"))

df_date_results<- as.data.frame(table(df_covid$week_test,df_covid$Test_Result,df_covid$Estatuto_Migrante_Crianca))
df_date_results$Var1<-as.Date(df_date_results$Var1)
colnames(df_date_results) <- c("Data_teste_serologico","Resultado_teste_serologico","Estatuto_migrante","Number_of_tests")

df_week_tests<-df_date_results %>% 
  mutate(week=floor_date(Data_teste_serologico, "week")) %>%
  group_by(Resultado_teste_serologico,Estatuto_migrante,week) %>%
  summarise(Number_of_tests=sum(Number_of_tests))

df_reactive<-df_week_tests %>% 
  spread(key=Resultado_teste_serologico, value=Number_of_tests) %>% 
  mutate(p_reactive=binconf(Positivo,(Positivo + Negativo)))

ggplot(data= df_reactive, aes(week,p_reactive[,1]*100)) + 
  geom_line() +
  geom_point() +
  xlab("Week") + ylab("Percentage of positives") +
  facet_wrap(~ Estatuto_migrante, ncol=1, labeller = migrant_status) +
   theme_light()

  
dev.off()

png(file = "./plot_prop_positives_week_by_status_coloured_lines.png", bg="white", width = 8, height = 3,  
    units = "in", res = 200)

ggplot(data = df_reactive, aes(x = week, y = p_reactive[,1] * 100, color = Estatuto_migrante)) +
  geom_line() +
  geom_point() +
  xlab("Week") + 
  ylab("Percentage of positives") +
  theme_light() +
  scale_color_manual(
    values = c("#f3d9ba","#92add0"),  # Define custom colors
    labels = c( "Non-immigrant","Immigrant"),
    name = ""
  ) +
  theme(legend.position = "right")  # Adjust legend position if needed

dev.off()
 
# PER MONTH
png(file = "./plot_prop_positives_month_by_status.png", bg="white", width = 8, height = 5,  
    units = "in", res = 200)

migrant_status <- as_labeller(
  c(`1` = "Immigrant children", `0` = "Non-immigrant children"))

df_date_results<- as.data.frame(table(df_covid$month_test,df_covid$Test_Result,df_covid$Estatuto_Migrante_Crianca))
df_date_results$Var1<-as.Date(df_date_results$Var1)
colnames(df_date_results) <- c("Data_teste_serologico","Resultado_teste_serologico","Estatuto_migrante","Number_of_tests")

df_month_tests<-df_date_results %>% 
  mutate(month=floor_date(Data_teste_serologico, "month")) %>%
  group_by(Resultado_teste_serologico,Estatuto_migrante,month) %>%
  summarise(Number_of_tests=sum(Number_of_tests))

df_reactive<-df_month_tests %>% 
  spread(key=Resultado_teste_serologico, value=Number_of_tests) %>% 
  mutate(p_reactive=binconf(Positivo,(Positivo + Negativo)))

write.xlsx(df_reactive, "./proportion_positive_per_month_per_status.xlsx")

df_reactive %>%
  filter(p_reactive[,1] != 0) %>%
    summarise(min = min(p_reactive[,1]),
            max = max(p_reactive[,1]))


ggplot(data= df_reactive, aes(month,p_reactive[,1]*100)) + 
  geom_line() +
  geom_point() +
   xlab("Month") + ylab("Percentage of positives") +
  ylim(0,100) +
  facet_wrap(~ Estatuto_migrante, ncol=1, labeller = migrant_status) +
  theme_light() 

dev.off()
#geom_errorbar(aes(ymin=p_reactive[,2]*100, ymax=p_reactive[,3]*100), width=.1) +
  


################################################################
################################################################
## 7.7. CHILDREN WITH POSITIVE TESTS / NEGATIVE TESTS
################################################################

# CREATE NEW DATAFRAME WITH TEST RESULTS of df_covid, added to the REPLICATED NUMBER OF ROWS in df_criancas 
# (match ID in df_covid to the left column in df_criancas (record_id)
colnames(df_criancas)
colnames(df_covid)

df_covid$record_id<-df_covid$ID

df<-df_covid %>%
  dplyr::select(Record_id,Test_Date, Type_Test, Test_Result,day_test,week_test,month_test,year_test,age_test,number_year_week,Result_binary) %>%
  left_join(df_criancas, by = c("Record_id" = "record_id"))


length(unique(df$Record_id))

glimpse(df)


#####################
## NUMBER OF DAYS BETWEEN POSITIVE TESTS

# filter records for Positive tests
rn<-which(df$Test_Result == "Positivo")
df_positive<-df[rn,]


# sort by record_id and day_test
df_positive_sorted <- df_positive[order(df_positive$Record_id, as.Date(df_positive$day_test)), ]

length(unique(df_positive_sorted))

# Calculate the day difference between the postitive tests
df_positive_sorted$diff <- unlist(tapply(df_positive_sorted$day_test, INDEX = df_positive_sorted$Record_id,
                                 FUN = function(x) c(NA, `units<-`(round(diff(x), digits=0), "days"))))

table(df_positive_sorted$diff)

png(file = "./plot_days_between_positive_test.png", bg="white", width = 5, height = 3,  
    units = "in", res = 200)
df_positive_sorted %>% 
  drop_na(diff) %>%
  ggplot(aes(x=diff)) + 
  geom_histogram(bins=40) +
  facet_wrap(~ Estatuto_Migrante_Crianca, ncol=1, labeller = migrant_status) + 
  xlab("Number of days from the previous positive test") + 
    ylab("Number of tests") +
  scale_x_continuous(breaks=seq(0,600,by=60)) +
  theme_light() 
dev.off()

rn<-which(df_positive_sorted$diff!=0)
df_positive_sorted[rn,]
nrow(df_positive_sorted[rn,])


##########
# CHILDREN WITH MORE THAN ONE POSITIVE TEST WITH MORE THAN 10 days difference
table(df_positive_sorted$diff)
df_positive_sorted$diff 

# children with interval between positive tests lower or equal to 10 days
rn<-which(df_positive_sorted$diff <= 10)
df_positive_sorted[rn,]
nrow(df_positive_sorted[rn,])


# children with interval between positive tests higher than 10 days
rn<-which(df_positive_sorted$diff > 10)
df_positive_sorted[rn,]
id_higher_10daysdiff<-df_positive_sorted[rn,"Record_id"]

# week of the test for all the records of children with interval between positive tests higher than 10 days
positives_weeks <- df %>%
  dplyr::select(Record_id, Test_Result, week_test, number_year_week) %>%
  filter(Record_id %in% id_higher_10daysdiff)
colnames(df)
  
# Join information from dominant variant on the week of test
# (match number_year_week in df_dominant_variant to the left column in positives_weeks (number_year_week)

positives_weeks_variants<-positives_weeks %>%
  dplyr::select(Record_id,Test_Result,number_year_week) %>%
  left_join(df_dominant_variant, by = 'number_year_week') %>%
  arrange(Record_id, number_year_week)

# write table - For each child (record_id) with more than one positive test (with interval higher than 10 days) 
# we report the dominant SARS-CoV-2 variant/lineage on the week that children did the test. 
write.xlsx(positives_weeks_variants, "./positives_weeks_variants.xlsx")

################################################################
################################################################
## CHILDREN WITH ALL TESTS NEGATIVE
################################################################

df_all_negative <- df %>%
  group_by(Record_id) %>%
  filter(all(Result_binary == 0))

df_negative_sorted <- df_all_negative[order(df_all_negative$Record_id, as.Date(df_all_negative$day_test)), ]

length(unique(df_negative_sorted))

df_negative_sorted$diff <- unlist(tapply(df_negative_sorted$day_test, INDEX = df_negative_sorted$Record_id,
                                         FUN = function(x) c(NA, `units<-`(round(diff(x), digits=0), "days"))))

png(file = "./plot_days_between_test_all_negative.png", bg="white", width = 5, height = 3,  
    units = "in", res = 200)
df_negative_sorted %>% 
  drop_na(diff) %>%
  ggplot(aes(x=diff)) + 
  geom_histogram(bins=40) +
  facet_wrap(~ Estatuto_Migrante_Crianca, ncol=1, labeller = migrant_status) + 
  ggtitle("Children that had all tests negative") +
  xlab("Number of days from the previous negative test") + 
  ylab("Number of tests") +
  scale_x_continuous(breaks=seq(0,800,by=60)) +
  theme_light() 
dev.off()

#####################
## NUMBER OF TESTS PER CHLIDREN THAT ONLY HAD NEGATIVE TESTS

nrow(df_negative_sorted)

df_N_all_negative_status<-as.data.frame(table(df_negative_sorted$Record_id,df_negative_sorted$Estatuto_Migrante_Crianca))
colnames(df_N_all_negative_status) <- c("Record_id","Estatuto_Migrante_Crianca","Number_of_negative_tests")

# zeros are artificial (mean that the child is not either immigrant or non-immigrant) - converto to NA
table(df_N_all_negative_status$Number_of_negative_tests, useNA = "always")
df_N_all_negative_status$Number_of_negative_tests[df_N_all_negative_status$Number_of_negative_tests %in% c(0)]<-NA
table(df_N_all_negative_status$Number_of_negative_tests, useNA = "always")


png(file = "./plot_number_tests_per_children_with_only_negative_tests.png", bg="white", width = 4, height = 3,  
    units = "in", res = 200)
df_N_all_negative_status %>% 
  drop_na(Number_of_negative_tests) %>%
  ggplot(aes(x=Number_of_negative_tests)) + 
  geom_bar() +
  facet_wrap(~ Estatuto_Migrante_Crianca, ncol=1, labeller = migrant_status) + 
  ggtitle("Children that had all tests negative") +
  xlab("Number of negative tests") + 
  ylab("Number of children") +
  theme_light() 
dev.off()

rn<-which(df_N_all_negative_status$Number_of_negative_tests > 10)
df_N_all_negative_status[rn,]
rn<-which(df$Record_id %in% c(df_N_all_negative_status[rn,]$Record_id))
df[rn,]


####################################
####################################
# AGE OF CHILDREN

table(df$Test_Result,df$age_test)

png(file = "./plot_result_test_by_age.png", bg="white", width = 6, height = 3,  
    units = "in", res = 200)

migrant_status <- as_labeller(
  c(`1` = "Immigrant children", `0` = "Non-immigrant children"))

df %>% 
  drop_na(Test_Result) %>%
  ggplot(aes(x=factor(age_test), fill=Test_Result)) + 
  geom_bar(position="stack") +
  xlab("Age of children at time of test (years)") + ylab("Relative frequency") +
  theme_light() +
  theme(axis.text=element_text(size=1)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ Estatuto_Migrante_Crianca, ncol=1, labeller = migrant_status) +  # Create a panel for each variable
  xlab("Age at time of test") + ylab("Number of tests") +
  theme_light() +
  scale_fill_manual(values=c("#f3d9ba",
                             "#92add0"),
                    labels=c("Negative", "Positive"),
                    name="Test result") 

dev.off()

rn<-which(df$age_test == 0 )
df[rn,]
