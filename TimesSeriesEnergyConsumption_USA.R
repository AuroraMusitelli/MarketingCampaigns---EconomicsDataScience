###################################################################  
#########   Progetto Introduzione alle Serie Storiche    ##########          
############   Aurora Musitelli, Matricola: 856741   ##############     
###################################################################  


### Caricamento pacchetti utilizzati
### Gestione delle date
library(lubridate)  
library(tsbox)      
### TS modelling Forecasting Principles and Practice 
library(forecast)
library(tsibble)
### Data Forecasting Principles and Practice 
library(fpp)
library(fpp2)
### Performance dei Modelli
library(performance)
### Tidyverse
library(tidyverse)
### Tidyverse plot
library(ggplot2)
library(ggpubr)
library(dplyr)
### Test ADF
library(urca)




##################################
##### 1.Importazione dei dati ####
##################################

### Importo il dataset 
dataUSA <- read.table("DATI4_Consumi energetici ed emissioni settori USA 1973-2024.txt",
                   header=TRUE,
                   sep = "",  
                   stringsAsFactors = FALSE)

### Visualizzazione della struttura del dataset 
View(dataUSA)
str(dataUSA)      
summary(dataUSA)

# Valori mancanti
(valori_mancanti <- dataUSA[apply(is.na(dataUSA), 1, any), ])   #righe con valori mancanti
(colonne_mancanti <- colSums(is.na(dataUSA)))                   #colonne con valori mancanti

# Settori e fonti energetiche con valori mancanti 
if ("Sector" %in% colnames(dataUSA) & "Source" %in% colnames(dataUSA)) {
  settori_fonti_mancanti <- unique(valori_mancanti[, c("Sector", "Source")])
  print("Sector e Source con valori mancanti:")
  print(settori_fonti_mancanti)
} else {
  print("Nessun problema")
}




#####################################################
##### 2.Analisi esplorativa delle serie storiche ####
#####################################################

### Rotazione/pivoting del dataset iniziale
datapivot <- dataUSA %>%
  pivot_wider(names_from = Sector,     
              values_from = c(EmissCO2_MillMetricTons, EnerCons_TrillBTU))  
view(datapivot)



### FONTE FOSSILE (Petrolio, Carbone, Gas Naturale) ###
#------------------------------------------------------
### Data handling/management con Tidyverse
dataF <- datapivot %>%
  arrange(Source) %>%      #Riordino righe in senso alfabetico 
  mutate(Month = ymd(Month),
         Date_idx = yearmonth(Month),
         Source = case_when(Source == "Total Fossil Fuels" ~ "Tot_Fossile",
                             TRUE ~ Source)) %>%
  filter(Source %in% c("Tot_Fossile")) %>%
  # Seleziono le colonne di interesse
  dplyr::select(Month,Date_idx, Source, 
         Industriale = EnerCons_TrillBTU_Industrial,
         Energia_Elettrica = `EnerCons_TrillBTU_Electric Power`,
         Residenziale = EnerCons_TrillBTU_Residential) %>%
  as_tsibble(index = Date_idx, key = Source)


### Trasformazione in oggetti classe time series
y <- dataF %>% ts_ts()
# Estraggo le singole serie storiche 
y1 <- dataF %>% dplyr::select(Industriale) %>% ts_ts()
y2 <- dataF %>% select(Energia_Elettrica) %>% ts_ts()
y3 <- dataF %>% select(Residenziale) %>% ts_ts()


### Rappresentazione serie storica multipla
autoplot(y, facets=TRUE) +
  labs(title = "Energia consumata nei 3 settori",
       subtitle = "Dati US EIA",
       x = "Anno", y = "Trillion di BTU")


### Osservo i grafici singoli per fonte energetica, contributo di ogni fonte energetica
PCG <- datapivot %>%
  arrange(Source) %>%      
  mutate(Month = ymd(Month),
         Date_idx = yearmonth(Month),
         Source = case_when(Source == "Petroleum" ~ "Petrolio",
                            Source == "Coal" ~ "Carbone",
                            Source == "Natural Gas Excluding Supplemental Gaseous Fuels" ~ "Gas_naturale",
                            TRUE ~ Source)) %>%
  filter(Source %in% c("Petrolio", "Carbone", "Gas_naturale")) %>%
  select(Month,Date_idx, Source, 
         Industriale = EnerCons_TrillBTU_Industrial,
         Energia_Elettrica = `EnerCons_TrillBTU_Electric Power`,
         Residenziale = EnerCons_TrillBTU_Residential) %>%
  as_tsibble(index = Date_idx, key = Source)

# Estraggo le singole serie storiche 
c <- PCG %>% ts_ts()
c1 <- PCG %>% select(Industriale) %>% ts_ts()
c2 <- PCG %>% select(Energia_Elettrica) %>% ts_ts()
c3 <- PCG %>% select(Residenziale) %>% ts_ts()

### Rappresentazione serie storica multiple 
autoplot(c1) + labs(title = "Energia consumata: settore industriale", subtitle = "Dati US EIA", x = "Anno", y = "Trillion di BTU")
autoplot(c2) + labs(title = "Energia consumata: settore Energia Elettrica", subtitle = "Dati US EIA", x = "Anno", y = "Trillion di BTU")
autoplot(c3) + labs(title = "Energia consumata: settore Residenziale", subtitle = "Dati US EIA", x = "Anno", y = "Trillion di BTU")



### FONTE RINNOVABILE (Energia Solare, Energia Eolica, Energia Geotermica, Energia Biomassa, Energia Idroelettrica) ###
#----------------------------------------------------------------------------------------------------------------------
### Data handling/management con Tidyverse
dataR <- datapivot %>%
  arrange(Source) %>%      
  mutate(Month = ymd(Month),
         Date_idx = yearmonth(Month),
         Source = case_when(Source == "Total Renewable Energy" ~ "Tot_Rinnovabile",
                            TRUE ~ Source)) %>%
  filter(Source %in% c("Tot_Rinnovabile")) %>%
  # Seleziono le colonne di interesse
  select(Month,Date_idx, Source, 
         Industriale = EnerCons_TrillBTU_Industrial,
         Energia_Elettrica = `EnerCons_TrillBTU_Electric Power`,
         Residenziale = EnerCons_TrillBTU_Residential) %>%
  as_tsibble(index = Date_idx, key = Source)


### Trasformazione in oggetti classe time series
z <- dataR %>% ts_ts()
# Estraggo le singole serie storiche 
z1 <- dataR %>% select(Industriale) %>% ts_ts()
z2 <- dataR %>% select(Energia_Elettrica) %>% ts_ts()
z3 <- dataR %>% select(Residenziale) %>% ts_ts()

### Rappresentazione serie storica multipla
autoplot(z, facets=TRUE) +
  labs(title = "Energia consumata nei 3 settori",
       subtitle = "Dati US EIA",
       x = "Anno", y = "Trillion di BTU")


### Osservo i grafici singoli per fonte energetica, contributo di ogni fonte energetica
SEGBI <- datapivot %>%
  arrange(Source) %>%      
  mutate(Month = ymd(Month),
         Date_idx = yearmonth(Month),
         Source = case_when(Source == "Solar Energy" ~ "Energia_solare",
                            Source == "Wind Energy" ~ "Energia_eolica",
                            Source == "Geothermal Energy" ~ "Energia_geotermica",
                            Source == "Biomass Energy" ~ "Energia_biomassa",
                            Source == "Conventional Hydroeletric Power" ~ "Energia_idroelettrica",
                            TRUE ~ Source)) %>%
  filter(Source %in% c("Energia_solare", "Energia_eolica", "Energia_geotermica", "Energia_biomassa", "Energia_idroelettrica")) %>%
  select(Month,Date_idx, Source, 
         Industriale = EnerCons_TrillBTU_Industrial,
         Energia_Elettrica = `EnerCons_TrillBTU_Electric Power`,
         Residenziale = EnerCons_TrillBTU_Residential) %>%
  as_tsibble(index = Date_idx, key = Source)

# Estraggo le singole serie storiche 
q <- SEGBI %>% ts_ts()
q1 <- SEGBI %>% select(Industriale) %>% ts_ts()
q2 <- SEGBI %>% select(Energia_Elettrica) %>% ts_ts()
q3 <- SEGBI %>% select(Residenziale) %>% ts_ts()

### Rappresentazione serie storica multiple 
autoplot(q1) + labs(title = "Energia consumata: settore industriale", subtitle = "Dati US EIA", x = "Anno", y = "Trillion di BTU")
autoplot(q2) + labs(title = "Energia consumata: settore Energia Elettrica", subtitle = "Dati US EIA", x = "Anno", y = "Trillion di BTU")
autoplot(q3) + labs(title = "Energia consumata: settore Residenziale", subtitle = "Dati US EIA", x = "Anno", y = "Trillion di BTU")




###################################################
##### Analisi distribuzione empirica dei dati #####
###################################################
cc <- c("Dens"="red","Norm"="blue")   #colori istogrammi


### FONTE FOSSILE (Petrolio, Carbone, Gas Naturale) ###
#------------------------------------------------------
## Settore Industriale 
f1 <- dataF %>%
  filter(Source == "Tot_Fossile") %>%
  ggplot(data = ., aes(x = Industriale)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black",
                 bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(dataF$Industriale,na.rm=T),
                            sd = sd(dataF$Industriale,na.rm = T)),
                aes(col="Norm"),
                size=1.5) + 
  labs(title = "Istogramma",
       subtitle = "Fonte fossile: settore Industriale",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

### Box-plot settore Industriale
dataF %>%
  filter(Source == "Tot_Fossile") %>%
  ggplot(aes(x = Industriale)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       subtitle = "Fonte fossile: settore Industriale",
       x = "Trillion di BTU")

## Analisi outlier settore Industriale (presenza di outlier)
clean1F <- tsclean(y1)
# Confronto grafico tra serie originale e serie ripulita
ts_plot1 <- data.frame(
  Month = as.Date(time(y1), origin = "1973-01-01"), 
  Original = as.numeric(y1),
  Cleaned = as.numeric(clean1F))
# Grafico 
ggplot(ts_plot1, aes(x = Month)) +
  geom_line(aes(y = Original, color = "Originale")) +
  geom_line(aes(y = Cleaned, color = "No outlier")) +
  labs(title = "Outlier detection",
       subtitle = "Fonte fossile: settore Industriale",
       x = "Anno",
       y = "Trillion di BTU") +
  scale_color_manual(values = c("Originale" = "black", "No outlier" = "green")) +
  theme_minimal()


## Settore Energia_Elettrica 
f2 <- dataF %>%
  filter(Source == "Tot_Fossile") %>%
  ggplot(data = ., aes(x = Energia_Elettrica)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black",
                 bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(dataF$Energia_Elettrica,na.rm=T),
                            sd = sd(dataF$Energia_Elettrica,na.rm = T)),
                aes(col="Norm"),
                size=1.5) + 
  labs(title = "Istogramma",
       subtitle = "Fonte fossile: settore Energia Elettrica",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

### Box-plot settore Energia Elettrica
dataF %>%
  filter(Source == "Tot_Fossile") %>%
  ggplot(aes(x = Energia_Elettrica)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       subtitle = "Fonte fossile: settore Energia Elettrica",
       x = "Trillion di BTU")


## Settore Residenziale
f3 <- dataF %>%
  filter(Source == "Tot_Fossile") %>%
  ggplot(data = ., aes(x = Residenziale)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black",
                 bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(dataF$Residenziale,na.rm=T),
                            sd = sd(dataF$Residenziale,na.rm = T)),
                aes(col="Norm"),
                size=1.5) + 
  labs(title = "Istogramma",
       subtitle = "Fonte fossile: settore Residenziale",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

### Box-plot settore Residenziale
dataF %>%
  filter(Source == "Tot_Fossile") %>%
  ggplot(aes(x = Residenziale)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       subtitle = "Fonte fossile: settore Residenziale",
       x = "Trillion di BTU")

## Visualizzazione dei grafici
ggarrange(f1,f2,f3, nrow = 1)



### FONTE RINNOVABILE (Energia Solare, Energia Eolica, Energia Geotermica, Energia Biomassa, Energia Idroelettrica) ###
#----------------------------------------------------------------------------------------------------------------------
## Settore Industriale 
r1 <- dataR %>%
  filter(Source == "Tot_Rinnovabile") %>%
  ggplot(data = ., aes(x = Industriale)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black",
                 bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(dataR$Industriale,na.rm=T),
                            sd = sd(dataR$Industriale,na.rm = T)),
                aes(col="Norm"),
                size=1.5) + 
  labs(title = "Istogramma",
       subtitle = "Fonte rinnovabile: settore Industriale",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

### Box-plot settore Industriale
dataR %>%
  filter(Source == "Tot_Rinnovabile") %>%
  ggplot(aes(x = Industriale)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       subtitle = "Fonte rinnovabile: settore Industriale",
       x = "Trillion di BTU")


## Settore Energia_Elettrica 
r2 <- dataR %>%
  filter(Source == "Tot_Rinnovabile") %>%
  ggplot(data = ., aes(x = Energia_Elettrica)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black",
                 bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(dataR$Energia_Elettrica,na.rm=T),
                            sd = sd(dataR$Energia_Elettrica,na.rm = T)),
                aes(col="Norm"),
                size=1.5) + 
  labs(title = "Istogramma",
       subtitle = "Fonte rinnovabile: settore Energia Elettrica",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

### Box-plot settore Energia Elettrica
dataR %>%
  filter(Source == "Tot_Rinnovabile") %>%
  ggplot(aes(x = Energia_Elettrica)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       subtitle = "Fonte rinnovabile: settore Energia Elettrica",
       x = "Trillion di BTU")

## Analisi outlier settore Energia Elettrica (presenza di outlier)
clean2R <- tsclean(z2)
# Confronto grafico tra serie originale e serie ripulita
ts_plot2 <- data.frame(
  Month = as.Date(time(z2), origin = "1973-01-01"), 
  Original = as.numeric(z2),
  Cleaned = as.numeric(clean2R))
# Grafico 
ggplot(ts_plot2, aes(x = Month)) +
  geom_line(aes(y = Original, color = "Originale")) +
  geom_line(aes(y = Cleaned, color = "No outlier")) +
  labs(title = "Outlier detection",
       subtitle = "Fonte rinnovabile: settore Energia Elettrica",
       x = "Anno",
       y = "Trillion di BTU") +
  scale_color_manual(values = c("Originale" = "black", "No outlier" = "green")) +
  theme_minimal()


## Settore Residenziale
r3 <- dataR %>%
  filter(Source == "Tot_Rinnovabile") %>%
  ggplot(data = ., aes(x = Residenziale)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black",
                 bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(dataR$Residenziale,na.rm=T),
                            sd = sd(dataR$Residenziale,na.rm = T)),
                aes(col="Norm"),
                size=1.5) + 
  labs(title = "Istogramma",
       subtitle = "Fonte rinnovabile: settore Residenziale",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

### Box-plot settore Residenziale
dataR %>%
  filter(Source == "Tot_Rinnovabile") %>%
  ggplot(aes(x = Residenziale)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       subtitle = "Fonte rinnovabile: settore Residenziale",
       x = "Trillion di BTU")

## Visualizzazione dei grafici
ggarrange(r1,r2,r3, nrow = 1)



########################################
### Test di normalità di Bera-Jarque ###
########################################
library(tseries)

### FONTE FOSSILE (Petrolio, Carbone, Gas Naturale) ###
#------------------------------------------------------
## Settore Industriale 
dataF %>%
  filter(Source == "Tot_Fossile") %>%
  pull(Industriale) %>%
  jarque.bera.test()

## Settore Energia_Elettrica 
dataF %>%
  filter(Source == "Tot_Fossile") %>%
  pull(Energia_Elettrica) %>%
  jarque.bera.test() 

## Settore Residenziale
dataF %>%
  filter(Source == "Tot_Fossile") %>%
  pull(Residenziale) %>%
  jarque.bera.test() 



### FONTE RINNOVABILE (Energia Solare, Energia Eolica, Energia Geotermica, Energia Biomassa, Energia Idroelettrica) ###
#----------------------------------------------------------------------------------------------------------------------
## Settore Industriale 
dataR %>%
  filter(Source == "Tot_Rinnovabile") %>%
  pull(Industriale) %>%
  jarque.bera.test()   

## Settore Energia_Elettrica 
dataR %>%
  filter(Source == "Tot_Rinnovabile") %>%
  pull(Energia_Elettrica) %>%
  jarque.bera.test() 

## Settore Residenziale
dataR %>%
  filter(Source == "Tot_Rinnovabile") %>%
  pull(Residenziale) %>%
  jarque.bera.test() 




#################################################
##### Analisi della persistenza: ACF e PACF #####
#################################################


### FONTE FOSSILE (Petrolio, Carbone, Gas Naturale) ###
#------------------------------------------------------
##### AutoCorrelation Functions
af1 <- ggAcf(clean1F,lag.max = 100) + 
  labs(title = "ACF",
       subtitle = "Energia consumata: settore industriale")
af2 <- ggAcf(y2,lag.max = 100) + 
  labs(title = "ACF",
       subtitle = "Energia consumata: settore energia elettrica")
af3 <- ggAcf(y3,lag.max = 100) + 
  labs(title = "ACF",
       subtitle = "Energia consumata: settore residenziale")
pacomb <- ggarrange(af1,af2,af3,ncol = 2,nrow = 2)
annotate_figure(pacomb, top = text_grob("ACF fonte Fossile", color = "blue", face = "bold", size = 14))

##### Partial AutoCorrelation Functions
pf1 <- ggPacf(clean1F,lag.max = 36) + 
  labs(title = "PACF",
       subtitle = "Energia consumata: settore industriale")
pf2 <- ggPacf(y2,lag.max = 36) + 
  labs(title = "PACF",
       subtitle = "Energia consumata: settore energia elettrica")
pf3 <- ggPacf(y3,lag.max = 36) + 
  labs(title = "PACF",
       subtitle = "Energia consumata: settore residenziale")
pfcomb <- ggarrange(pf1,pf2,pf3,ncol = 2,nrow = 2)
annotate_figure(pfcomb, top = text_grob("PACF fonte Fossile", color = "blue", face = "bold", size = 14))

##### Test di Ljung-Box 
library(feasts)
lags <- c(1,2,3,6,12,18,24,36)   
LBQ_pv <- matrix(data = NA, nrow = length(lags), ncol = 4)
LBQ_pv[,1] <- lags
for (i in 1:length(lags)) {
  LBQ_pv[i,2] <- ljung_box(x = y1, lag = lags[i])[2]
  LBQ_pv[i,3] <- ljung_box(x = y2, lag = lags[i])[2]
  LBQ_pv[i,4] <- ljung_box(x = y3, lag = lags[i])[2]
}
LBQ_pv <- data.frame(LBQ_pv)
colnames(LBQ_pv) <- c("lag","Industriale","Energia Elettrica","Residenziale")
LBQ_pv



### FONTE RINNOVABILE (Energia Solare, Energia Eolica, Energia Geotermica, Energia Biomassa, Energia Idroelettrica) ###
#----------------------------------------------------------------------------------------------------------------------
##### AutoCorrelation Functions
ar1 <- ggAcf(z1,lag.max = 120) + 
  labs(title = "ACF",
       subtitle = "Energia consumata: settore industriale")
ar2 <- ggAcf(clean2R,lag.max = 150) + 
  labs(title = "ACF",
       subtitle = "Energia consumata: settore energia elettrica")
ar3 <- ggAcf(z3,lag.max = 150) + 
  labs(title = "ACF",
       subtitle = "Energia consumata: settore residenziale")
pacomb <- ggarrange(ar1,ar2,ar3,ncol = 2,nrow = 2)
annotate_figure(pacomb, top = text_grob("ACF fonte Rinnovabile", color = "green", face = "bold", size = 14))

##### Partial AutoCorrelation Functions
pr1 <- ggPacf(z1,lag.max = 36) + 
  labs(title = "PACF",
       subtitle = "Energia consumata: settore industriale")
pr2 <- ggPacf(clean2R,lag.max = 36) + 
  labs(title = "PACF",
       subtitle = "Energia consumata: settore energia elettrica")
pr3 <- ggPacf(z3,lag.max = 36) + 
  labs(title = "PACF",
       subtitle = "Energia consumata: settore residenziale")
prcomb<-ggarrange(pr1,pr2,pr3,ncol = 2,nrow = 2)
annotate_figure(prcomb, top = text_grob("PACF fonte Rinnovabile", color = "green", face = "bold", size = 14))

##### Test di Ljung-Box
lags <- c(1,2,3,6,12,18,24,36)
LBQ_pv <- matrix(data = NA, nrow = length(lags), ncol = 4)
LBQ_pv[,1] <- lags
for (i in 1:length(lags)) {
  LBQ_pv[i,2] <- ljung_box(x = z1, lag = lags[i])[2]
  LBQ_pv[i,3] <- ljung_box(x = z2, lag = lags[i])[2]
  LBQ_pv[i,4] <- ljung_box(x = z3, lag = lags[i])[2]
}
LBQ_pv <- data.frame(LBQ_pv)
colnames(LBQ_pv) <- c("lag","Industriale","Energia Elettrica","Residenziale")
LBQ_pv




###############################
## Trasformazioni di BOX-COX ##   
###############################


### FONTE FOSSILE (Petrolio, Carbone, Gas Naturale) ###
#------------------------------------------------------
## METODO GUERRERO  e METODO MASSIMA VEROSIMIGLIANZA 
# Settore Industriale
lambdaF_guer1 <- forecast::BoxCox.lambda(clean1F,method = "guerrero",lower = -3,upper = 3)
lambdaF_guer1  

lambdaF_loglik1 <- forecast::BoxCox.lambda(clean1F,method = "loglik",lower = -3,upper = 3)
lambdaF_loglik1  

# Massima verosimiglianza per vari modelli lineari 
m_IF_grezzi <- tslm(clean1F ~ 1)                                     # Dati grezzi (ripuliti da outlier)
m_IF_trend <- tslm(clean1F ~ trend)                                  # Dati detrendizzati
m_IF_seas <- tslm(clean1F ~ fourier(clean1F,K = 6))                  # Dati destagionalizzati con Fourier
m_IF_trendseas <- tslm(clean1F ~ trend + fourier(clean1F,K = 6))     # Dati detrend e destagionalizzati (serie residuale)
par(mfrow=c(2,2))
IF_grezzi <- MASS::boxcox(m_IF_grezzi,lambda=seq(-3,3,by=.01))
title(main = "Log-lik FossileIndustriale (no model)")
IF_trend <- MASS::boxcox(m_IF_trend,lambda=seq(-3,3,by=.01))
title(main = "Log-lik FossileIndustriale (LM con trend)")
IF_seas <- MASS::boxcox(m_IF_seas,lambda=seq(-3,3,by=.01))
title(main = "Log-lik FossileIndustriale (LM con armoniche stagionali)")
IF_trendseas <- MASS::boxcox(m_IF_trendseas,lambda=seq(-3,3,by=.01))
title(main = "Log-lik FossileIndustriale (LM con trend e armoniche stagionali)")
par(mfrow=c(1,1))

# Trasformazione logaritmica log(clean1F)
ts_plot1 <- ts_plot1 %>% mutate(Month = ymd(Month), Date_idx = yearmonth(Month))  %>% as_tsibble(index = Date_idx)
datalogF <- ts_plot1 %>% mutate(log_y1 = log(Cleaned))  #dati ripuliti da outlier
log_y1 <- datalogF %>% select(log_y1) %>% ts_ts()
# Istogramma
logF <- ggplot(data = datalogF, aes(x = log_y1)) + 
  geom_histogram(aes(y =..density..),
                 colour="white", fill = "black", bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm, args = list(mean = mean(datalogF$log_y1,na.rm=T), sd = sd(datalogF$log_y1,na.rm = T)),
                aes(col="Norm"), size=1.5) +
  labs(title = "Istogramma di log(EnerCons)", x = latex2exp::TeX("log(Trillion BTU)"), y = "Densità", 
       subtitle = "Fonte fossile: settore Industriale") + 
  scale_color_manual("Curve",
                     values = c("Dens" = "green", "Norm" = "blue"),
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

## Test Bera-Jarque
datalogF %>%
  pull(log_y1) %>%
  jarque.bera.test()   
## Test Shapiro-Wilks
datalogF %>%
  pull(log_y1) %>%
  shapiro.test()


# Trasformazione in scala inversa 1/(clean1F)
inv1F <- ts_plot1 %>% mutate(y1inv = 1/(Cleaned))
y1inv <- inv1F %>% select(y1inv) %>% ts_ts()
# Istogramma
Finv1 <- ggplot(data = inv1F, aes(x = y1inv)) + 
  geom_histogram(aes(y =..density..),
                 colour="white", fill = "black", bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm, args = list(mean = mean(inv1F$y1inv,na.rm=T), sd = sd(inv1F$y1inv,na.rm = T)),
                aes(col="Norm"), size=1.5) +
  labs(title = "Istogramma 1/(EnerCons)", x = latex2exp::TeX("1/(Trillion BTU)"), y = "Densità",
       subtitle = "Fonte fossile: settore Industriale") + 
  scale_color_manual("Curve",
                     values = c("Dens" = "green", "Norm" = "blue"),  
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

## Test Bera-Jarque
inv1F %>%
  pull(y1inv) %>%
  jarque.bera.test()   
## Test Shapiro-Wilks
inv1F %>%
  pull(y1inv) %>%
  shapiro.test()

# Visualizzazione dei grafici
ggarrange(f1,logF,Finv1, nrow = 1)


############################################
##### Analisi della stazionarietà: ADF #####
############################################

##### Test ADF sulla serie storica
# Si trend, Si costante
ADF_y1_const_trend <- urca::ur.df(y = clean1F, type = "trend", selectlags = "AIC")
summary(ADF_y1_const_trend)




## METODO GUERRERO  e METODO MASSIMA VEROSIMIGLIANZA 
## Settore Energia_Elettrica 
lambdaF_guer2 <- forecast::BoxCox.lambda(y2,method = "guerrero",lower = -3,upper = 3)
lambdaF_guer2  

lambdaF_loglik2 <- forecast::BoxCox.lambda(y2,method = "loglik",lower = -3,upper = 3)
lambdaF_loglik2

# Massima verosimiglianza per vari modelli lineari 
m_EF_grezzi <- tslm(y2 ~ 1)                                # Dati grezzi 
m_EF_trend <- tslm(y2 ~ trend)                             # Dati detrendizzati
m_EF_seas <- tslm(y2 ~ fourier(y2,K = 6))                  # Dati destagionalizzati con Fourier
m_EF_trendseas <- tslm(y2 ~ trend + fourier(y2,K = 6))     # Dati detrend e destagionalizzati (serie residuale)
par(mfrow=c(2,2))
EF_grezzi <- MASS::boxcox(m_EF_grezzi,lambda=seq(-3,3,by=.01))
title(main = "Log-lik FossileElettrico (no model)")
EF_trend <- MASS::boxcox(m_EF_trend,lambda=seq(-3,3,by=.01))
title(main = "Log-lik FossileElettrico (LM con trend)")
EF_seas <- MASS::boxcox(m_EF_seas,lambda=seq(-3,3,by=.01))
title(main = "Log-lik FossileElettrico (LM con armoniche stagionali)")
EF_trendseas <- MASS::boxcox(m_EF_trendseas,lambda=seq(-3,3,by=.01))
title(main = "Log-lik FossileElettrico (LM con trend e armoniche stagionali)")
par(mfrow=c(1,1))


# Trasformazione logaritmica log(y2)
datalog2F <- dataF %>% mutate(log_y2 = log(Energia_Elettrica))  
log_y2 <- datalog2F %>% select(log_y2) %>% ts_ts()
# Istogramma
log2F <- ggplot(data = datalog2F, aes(x = log_y2)) + 
  geom_histogram(aes(y =..density..),
                 colour="white", fill = "black", bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm, args = list(mean = mean(datalog2F$log_y2,na.rm=T), sd = sd(datalog2F$log_y2,na.rm = T)),
                aes(col="Norm"), size=1.5) +
  labs(title = "Istogramma di log(EnerCons)", x = latex2exp::TeX("log(Trillion BTU)"), y = "Densità", 
       subtitle = "Fonte fossile: settore Energia Elettrica") + 
  scale_color_manual("Curve",
                     values = c("Dens" = "green", "Norm" = "blue"),
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

## Test Bera-Jarque
datalog2F %>%
  filter(Source == "Tot_Fossile") %>%
  pull(log_y2) %>%
  jarque.bera.test()   
## Test Shapiro-Wilks
datalog2F %>%
  filter(Source == "Tot_Fossile") %>%
  pull(log_y2) %>%
  shapiro.test()


# Trasformazione radice quadrata inversa (1/sqrt(y2))
dataInvF <- dataF %>% mutate(sqrt_y2 = (1/sqrt(Energia_Elettrica)))
sqrt_y2 <- dataInvF %>% select(sqrt_y2) %>% ts_ts()
# Istogramma
invF <- ggplot(data = dataInvF, aes(x = sqrt_y2)) + 
  geom_histogram(aes(y =..density..),
                 colour="white", fill = "black", bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm, args = list(mean = mean(dataInvF$sqrt_y2,na.rm=T), sd = sd(dataInvF$sqrt_y2,na.rm = T)),
                aes(col="Norm"), size=1.5) +
  labs(title = "Istogramma di 1/sqrt(EnerCons)", x = latex2exp::TeX("1/sqrt(Trillion BTU)"), y = "Densità", 
       subtitle = "Fonte fossile: settore Energia Elettrica") + 
  scale_color_manual("Curve",
                     values = c("Dens" = "green", "Norm" = "blue"), 
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

## Test Bera-Jarque
dataInvF %>%
  filter(Source == "Tot_Fossile") %>%
  pull(sqrt_y2) %>%
  jarque.bera.test()   
## Test Shapiro-Wilks
dataInvF %>%
  filter(Source == "Tot_Fossile") %>%
  pull(sqrt_y2) %>%
  shapiro.test()

# Visualizzazione dei grafici
ggarrange(f2,log2F,invF,nrow = 1)


############################################
##### Analisi della stazionarietà: ADF #####
############################################

##### Test ADF sulle serie storiche
# Si trend, Si costante
ADF_y2_const_trend <- urca::ur.df(y = y2, type = "trend", selectlags = "AIC")
summary(ADF_y2_const_trend)




## METODO GUERRERO  e METODO MASSIMA VEROSIMIGLIANZA 
## Settore Residenziale
lambdaF_guer3 <- forecast::BoxCox.lambda(y3,method = "guerrero",lower = -3,upper = 3)
lambdaF_guer3  

lambdaF_loglik3 <- forecast::BoxCox.lambda(y3,method = "loglik",lower = -3,upper = 3)
lambdaF_loglik3

# Massima verosimiglianza per vari modelli lineari 
m_RF_grezzi <- tslm(y3 ~ 1)                                # Dati grezzi 
m_RF_trend <- tslm(y3 ~ trend)                             # Dati detrendizzati
m_RF_seas <- tslm(y3 ~ fourier(y3,K = 6))                  # Dati destagionalizzati con Fourier
m_RF_trendseas <- tslm(y3 ~ trend + fourier(y3,K = 6))     # Dati detrend e destagionalizzati (serie residuale)
par(mfrow=c(2,2))
RF_grezzi <- MASS::boxcox(m_RF_grezzi,lambda=seq(-3,3,by=.01))
title(main = "Log-lik FossileResidenziale (no model)")
RF_trend <- MASS::boxcox(m_RF_trend,lambda=seq(-3,3,by=.01))
title(main = "Log-lik FossileResidenziale (LM con trend)")
RF_seas <- MASS::boxcox(m_RF_seas,lambda=seq(-3,3,by=.01))
title(main = "Log-lik FossileResidenziale (LM con armoniche stagionali)")
RF_trendseas <- MASS::boxcox(m_RF_trendseas,lambda=seq(-3,3,by=.01))
title(main = "Log-lik FossileResidenziale (LM con trend e armoniche stagionali)")
par(mfrow=c(1,1))


# Trasformazione logaritmica log(y3)
datalog3F <- dataF %>% mutate(logy3 = log(Residenziale))
logy3 <- datalog3F %>% select(logy3) %>% ts_ts()

# Istogramma
log3F <- ggplot(data = datalog3F, aes(x = logy3)) + 
  geom_histogram(aes(y =..density..),
                 colour="white", fill = "black", bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm, args = list(mean = mean(datalog3F$logy3,na.rm=T), sd = sd(datalog3F$logy3,na.rm = T)),
                aes(col="Norm"), size=1.5) +
  labs(title = "Istogramma di log(EnerCons)", x = latex2exp::TeX("log(Trillion BTU)"), y = "Densità", 
       subtitle = "Fonte fossile: settore Residenziale") + 
  scale_color_manual("Curve",
                     values = c("Dens" = "green", "Norm" = "blue"), 
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

## Test Bera-Jarque
datalog3F %>%
  filter(Source == "Tot_Fossile") %>%
  pull(logy3) %>%
  jarque.bera.test()   
## Test Shapiro-Wilks
datalog3F %>%
  filter(Source == "Tot_Fossile") %>%
  pull(logy3) %>%
  shapiro.test()


# Trasformazione radice quadrata sqrt(y3)
datasqrt3F <- dataF %>% mutate(sqrty3 = sqrt(Residenziale))
sqrty3 <- datasqrt3F %>% select(sqrty3) %>% ts_ts()

# Istogramma
sqrt3F <- ggplot(data = datasqrt3F, aes(x = sqrty3)) + 
  geom_histogram(aes(y =..density..),
                 colour="white", fill = "black", bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm, args = list(mean = mean(datasqrt3F$sqrty3,na.rm=T), sd = sd(datasqrt3F$sqrty3,na.rm = T)),
                aes(col="Norm"), size=1.5) +
  labs(title = "Istogramma di sqrt(EnerCons)", x = latex2exp::TeX("sqrt(Trillion BTU)"), y = "Densità", 
       subtitle = "Fonte fossile: settore Residenziale") + 
  scale_color_manual("Curve",
                     values = c("Dens" = "green", "Norm" = "blue"),
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

## Test Bera-Jarque
datasqrt3F %>%
  filter(Source == "Tot_Fossile") %>%
  pull(sqrty3) %>%
  jarque.bera.test()   
## Test Shapiro-Wilks
datasqrt3F %>%
  filter(Source == "Tot_Fossile") %>%
  pull(sqrty3) %>%
  shapiro.test()

# Visualizzazione dei grafici
ggarrange(f3,log3F,sqrt3F,nrow = 1)


############################################
##### Analisi della stazionarietà: ADF #####
############################################

##### Test ADF sulle serie storiche
# No trend, No costante (osservando la serie storica originale si parte da questo)
ADF_y3_const_trend <- urca::ur.df(y = y3, type = "none", selectlags = "AIC")
summary(ADF_y3_const_trend)




### FONTE RINNOVABILE (Energia Solare, Energia Eolica, Energia Geotermica, Energia Biomassa, Energia Idroelettrica) ###
#----------------------------------------------------------------------------------------------------------------------

## METODO GUERRERO  e METODO MASSIMA VEROSIMIGLIANZA 
# Settore Industriale
lambdaR_guer1 <- forecast::BoxCox.lambda(z1,method = "guerrero",lower = -3,upper = 3)
lambdaR_guer1  

lambdaR_loglik1 <- forecast::BoxCox.lambda(z1,method = "loglik",lower = -3,upper = 3)
lambdaR_loglik1  

#Massima verosimiglianza per vari modelli lineari 
m_IF_grezzi <- tslm(z1 ~ 1)                                # Dati grezzi 
m_IF_trend <- tslm(z1 ~ trend)                             # Dati detrendizzati
m_IF_seas <- tslm(z1 ~ fourier(z1,K = 6))                  # Dati destagionalizzati con Fourier
m_IF_trendseas <- tslm(z1 ~ trend + fourier(z1,K = 6))     # Dati detrend e destagionalizzati (serie residuale)
par(mfrow=c(2,2))
IF_grezzi <- MASS::boxcox(m_IF_grezzi,lambda=seq(-3,3,by=.01))
title(main = "Log-lik RinnovabileIndustriale (no model)")
IF_trend <- MASS::boxcox(m_IF_trend,lambda=seq(-3,3,by=.01))
title(main = "Log-lik RinnovabileIndustriale (LM con trend)")
IF_seas <- MASS::boxcox(m_IF_seas,lambda=seq(-3,3,by=.01))
title(main = "Log-lik RinnovabileIndustriale (LM con armoniche stagionali)")
IF_trendseas <- MASS::boxcox(m_IF_trendseas,lambda=seq(-3,3,by=.01))
title(main = "Log-lik RinnovabileIndustriale (LM con trend e armoniche stagionali)")
par(mfrow=c(1,1))


# Trasformazione al quadrato(z1)
dataR_2 <- dataR %>% mutate(z1_2 = (Industriale)^2)  
z1_2 <- dataR_2 %>% select(z1_2) %>% ts_ts()
# Istogramma
R_2 <- ggplot(data = dataR_2, aes(x = z1_2)) + 
  geom_histogram(aes(y =..density..),
                 colour="white", fill = "black", bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm, args = list(mean = mean(dataR_2$z1_2,na.rm=T), sd = sd(dataR_2$z1_2,na.rm = T)),
                aes(col="Norm"), size=1.5) +
  labs(title = "Istogramma di (EnerCons)^2", x = latex2exp::TeX("(Trillion BTU)^2"), y = "Densità", 
       subtitle = "Fonte rinnovabile: settore Industriale") + 
  scale_color_manual("Curve",
                     values = c("Dens" = "green", "Norm" = "blue"),
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

## Test Bera-Jarque
dataR_2 %>%
  filter(Source == "Tot_Rinnovabile") %>%
  pull(z1_2) %>%
  jarque.bera.test()   
## Test Shapiro-Wilks
dataR_2 %>%
  filter(Source == "Tot_Rinnovabile") %>%
  pull(z1_2) %>%
  shapiro.test()

# Visualizzazione dei grafici
ggarrange(r1,R_2, nrow = 1)


############################################
##### Analisi della stazionarietà: ADF #####
############################################
library(urca)

##### Test ADF sulle serie storiche
# Si trend, Si costante
ADF_z1_const_trend <- urca::ur.df(y = z1, type = "trend", selectlags = "AIC")
summary(ADF_z1_const_trend)




## METODO GUERRERO  e METODO MASSIMA VEROSIMIGLIANZA 
## Settore Energia_Elettrica 
lambdaR_guer2 <- forecast::BoxCox.lambda(clean2R,method = "guerrero",lower = -3,upper = 3)
lambdaR_guer2  

lambdaR_loglik2 <- forecast::BoxCox.lambda(clean2R,method = "loglik",lower = -3,upper = 3)
lambdaR_loglik2  


# Massima verosimiglianza per vari modelli lineari 
m_IF_grezzi <- tslm(clean2R ~ 1)                                     # Dati grezzi (ripuliti da outlier)
m_IF_trend <- tslm(clean2R ~ trend)                                  # Dati detrendizzati
m_IF_seas <- tslm(clean2R ~ fourier(clean2R,K = 6))                  # Dati destagionalizzati con Fourier
m_IF_trendseas <- tslm(clean2R ~ trend + fourier(clean2R,K = 6))     # Dati detrend e destagionalizzati (serie residuale)
par(mfrow=c(2,2))
IF_grezzi <- MASS::boxcox(m_IF_grezzi,lambda=seq(-3,3,by=.01))
title(main = "Log-lik RinnovabileEnergiaElettrica (no model)")
IF_trend <- MASS::boxcox(m_IF_trend,lambda=seq(-3,3,by=.01))
title(main = "Log-lik RinnovabileEnergiaElettrica (LM con trend)")
IF_seas <- MASS::boxcox(m_IF_seas,lambda=seq(-3,3,by=.01))
title(main = "Log-lik RinnovabileEnergiaElettrica (LM con armoniche stagionali)")
IF_trendseas <- MASS::boxcox(m_IF_trendseas,lambda=seq(-3,3,by=.01))
title(main = "Log-lik RinnovabileEnergiaElettrica (LM con trend e armoniche stagionali)")
par(mfrow=c(1,1))


# Trasformazione radice quadrata sqrt(clean2R)
ts_plot2 <- ts_plot2 %>% mutate(Month = ymd(Month), Date_idx = yearmonth(Month))  %>% as_tsibble(index = Date_idx)
datasqrtR <- ts_plot2 %>% mutate(sqrt_z2 = sqrt(Cleaned))  #dati ripuliti da outlier
sqrt_z2 <- datasqrtR %>% select(sqrt_z2) %>% ts_ts()
# Istogramma
sqrt2R <- ggplot(data = datasqrtR, aes(x = sqrt_z2)) + 
  geom_histogram(aes(y =..density..),
                 colour="white", fill = "black", bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm, args = list(mean = mean(datasqrtR$sqrt_z2,na.rm=T), sd = sd(datasqrtR$sqrt_z2,na.rm = T)),
                aes(col="Norm"), size=1.5) +
  labs(title = "Istogramma di sqrt(EnerCons)", x = latex2exp::TeX("sqrt(Trillion BTU)"), y = "Densità", 
       subtitle = "Fonte rinnovabile: settore Energia Elettrica") + 
  scale_color_manual("Curve",
                     values = c("Dens" = "green", "Norm" = "blue"),
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

## Test Bera-Jarque
datasqrtR %>%
  pull(sqrt_z2) %>%
  jarque.bera.test()   
## Test Shapiro-Wilks
datasqrtR %>%
  pull(sqrt_z2) %>%
  shapiro.test()


# Trasformazione radice quadrata inversa 1/sqrt(clean2R)
datainvsqrtR <- ts_plot2 %>% mutate(sqrtinv_z2 = 1/(sqrt(Cleaned)))  #dati ripuliti da outlier
sqrtinv_z2 <- datainvsqrtR %>% select(sqrtinv_z2) %>% ts_ts()
# Istogramma
sqrt2invR <- ggplot(data = datainvsqrtR, aes(x = sqrtinv_z2)) + 
  geom_histogram(aes(y =..density..),
                 colour="white", fill = "black", bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm, args = list(mean = mean(datainvsqrtR$sqrtinv_z2,na.rm=T), sd = sd(datainvsqrtR$sqrtinv_z2,na.rm = T)),
                aes(col="Norm"), size=1.5) +
  labs(title = "Istogramma di 1/sqrt(EnerCons)", x = latex2exp::TeX("1/sqrt(Trillion BTU)"), y = "Densità", 
       subtitle = "Fonte rinnovabile: settore Energia Elettrica") + 
  scale_color_manual("Curve",
                     values = c("Dens" = "green", "Norm" = "blue"),
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

## Test Bera-Jarque
datainvsqrtR %>%
  pull(sqrtinv_z2) %>%
  jarque.bera.test()   
## Test Shapiro-Wilks
datainvsqrtR %>%
  pull(sqrtinv_z2) %>%
  shapiro.test()

# Viasualizzazione dei grafici
ggarrange(r2,sqrt2R,sqrt2invR,nrow = 1)


############################################
##### Analisi della stazionarietà: ADF #####
############################################
library(urca)

##### Test ADF sulle serie storiche
# Si trend, Si costante
ADF_z2_const_trend <- urca::ur.df(y = clean2R, type = "trend", selectlags = "AIC")
summary(ADF_z2_const_trend)




## METODO GUERRERO  e METODO MASSIMA VEROSIMIGLIANZA 
## Settore Residenziale
lambdaR_guer3 <- forecast::BoxCox.lambda(z3,method = "guerrero",lower = -3,upper = 3)
lambdaR_guer3  

lambdaR_loglik3 <- forecast::BoxCox.lambda(z3,method = "loglik",lower = -3,upper = 3)
lambdaR_loglik3  


#Massima verosimiglianza per vari modelli lineari 
m_IF_grezzi <- tslm(z3 ~ 1)                                # Dati grezzi 
m_IF_trend <- tslm(z3 ~ trend)                             # Dati detrendizzati
m_IF_seas <- tslm(z3 ~ fourier(z3,K = 6))                  # Dati destagionalizzati con Fourier
m_IF_trendseas <- tslm(z3 ~ trend + fourier(z3,K = 6))     # Dati detrend e destagionalizzati (serie residuale)
par(mfrow=c(2,2))
IF_grezzi <- MASS::boxcox(m_IF_grezzi,lambda=seq(-3,3,by=.01))
title(main = "Log-lik RinnovabileResidenziale (no model)")
IF_trend <- MASS::boxcox(m_IF_trend,lambda=seq(-3,3,by=.01))
title(main = "Log-lik RinnovabileResidenziale (LM con trend)")
IF_seas <- MASS::boxcox(m_IF_seas,lambda=seq(-3,3,by=.01))
title(main = "Log-lik RinnovabileResidenziale (LM con armoniche stagionali)")
IF_trendseas <- MASS::boxcox(m_IF_trendseas,lambda=seq(-3,3,by=.01))
title(main = "Log-lik RinnovabileResidenziale (LM con trend e armoniche stagionali)")
par(mfrow=c(1,1))


# Trasformazione logaritmica log(z3)
dataR_3 <- dataR %>% mutate(logz3 = log(Residenziale))  
logz3 <- dataR_3 %>% select(logz3) %>% ts_ts()
# Istogramma
logz3R <- ggplot(data = dataR_3, aes(x = logz3)) + 
  geom_histogram(aes(y =..density..),
                 colour="white", fill = "black", bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm, args = list(mean = mean(dataR_3$logz3,na.rm=T), sd = sd(dataR_3$logz3,na.rm = T)),
                aes(col="Norm"), size=1.5) +
  labs(title = "Istogramma di log(EnerCons)", x = latex2exp::TeX("log(Trillion BTU)"), y = "Densità", 
       subtitle = "Fonte rinnovabile: settore Residenziale") + 
  scale_color_manual("Curve",
                     values = c("Dens" = "green", "Norm" = "blue"),
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

## Test Bera-Jarque
dataR_3 %>%
  filter(Source == "Tot_Rinnovabile") %>%
  pull(logz3) %>%
  jarque.bera.test()   
## Test Shapiro-Wilks
dataR_3 %>%
  filter(Source == "Tot_Rinnovabile") %>%
  pull(logz3) %>%
  shapiro.test()


# Trasformazione in scala inversa 1/(z3)
datainvR_3 <- dataR %>% mutate(invz3 = 1/(Residenziale))  
invz3 <- datainvR_3 %>% select(invz3) %>% ts_ts()
# Istogramma
invz3R <- ggplot(data = datainvR_3, aes(x = invz3)) + 
  geom_histogram(aes(y =..density..),
                 colour="white", fill = "black", bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm, args = list(mean = mean(datainvR_3$invz3,na.rm=T), sd = sd(datainvR_3$invz3,na.rm = T)),
                aes(col="Norm"), size=1.5) +
  labs(title = "Istogramma di 1/(EnerCons)", x = latex2exp::TeX("1/(Trillion BTU)"), y = "Densità", 
       subtitle = "Fonte rinnovabile: settore Residenziale") + 
  scale_color_manual("Curve",
                     values = c("Dens" = "green", "Norm" = "blue"),
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

## Test Bera-Jarque
datainvR_3 %>%
  filter(Source == "Tot_Rinnovabile") %>%
  pull(invz3) %>%
  jarque.bera.test()   
## Test Shapiro-Wilks
datainvR_3 %>%
  filter(Source == "Tot_Rinnovabile") %>%
  pull(invz3) %>%
  shapiro.test()

# Visualizzazione dei grafici
ggarrange(r3,logz3R,invz3R, nrow = 1)


############################################
##### Analisi della stazionarietà: ADF #####
############################################
library(urca)

##### Test ADF sulle serie storiche
# No trend, No costante
ADF_z3 <- urca::ur.df(y = z3, type = "none", selectlags = "AIC")
summary(ADF_z3)   #la serie risulta non stazionaria 
# No trend, Si costante
ADF_z3_const <- urca::ur.df(y = z3, type = "drift", selectlags = "AIC")
summary(ADF_z3_const) #la serie riulta non stazionaria, anche includendo una costante
# Si trend, Si costante
ADF_z3_const_trend <- urca::ur.df(y = z3, type = "trend", selectlags = "AIC")
summary(ADF_z3_const_trend)   #la serie riulta non stazionaria, anche includendo il trend

 


##############################
##### 3.Detrendizzazione #####
##############################

## FONTE FOSSILE: Settore Industriale
##### Detrend polinomiale
m1 <- tslm(clean1F ~ trend)
m2 <- tslm(clean1F ~ trend + I(trend^2))
m3 <- tslm(clean1F ~ trend + I(trend^2) + I(trend^3))
m4 <- tslm(clean1F ~ trend + I(trend^2) + I(trend^3) + I(trend^4))
m5 <- tslm(clean1F ~ trend + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5))
## Full sample performances 
perf_m1 <- model_performance(model = m1)
perf_m2 <- model_performance(model = m2)
perf_m3 <- model_performance(model = m3)
perf_m4 <- model_performance(model = m4)
perf_m5 <- model_performance(model = m5)
perf <- as.data.frame(rbind(perf_m1,perf_m2,perf_m3,perf_m4,perf_m5))
cbind(Model = c("M1","M2","M3","M4","M5"),perf)
## Aggiungo residui e valori stimati
y1_detr_par <- ts_plot1 %>%
  mutate(
    Month = as.Date(time(clean1F)), # Conversione a Date
    clean1F = as.numeric(clean1F),  # Conversione a numerico
    y1_detr_m1 = as.numeric(m1$residuals), trend_m1 = as.numeric(m1$fitted.values),
    y1_detr_m2 = as.numeric(m2$residuals), trend_m2 = as.numeric(m2$fitted.values),
    y1_detr_m3 = as.numeric(m3$residuals), trend_m3 = as.numeric(m3$fitted.values),
    y1_detr_m4 = as.numeric(m4$residuals), trend_m4 = as.numeric(m4$fitted.values))
## Sovrapposizione dei trend stimati alla serie originale
y1_detr_par %>%
  pivot_longer(cols = c(trend_m1, trend_m2, trend_m3, trend_m4, y1_detr_m4),
               names_to = "model", values_to = "trend") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y = clean1F), color = "black", size = 1.0) + # Serie originale
  geom_line(aes(y = trend, col = model), size = 1.0) +       # Trend stimati
  scale_color_manual(values = c("trend_m1" = "red", 
                                "trend_m2" = "green", 
                                "trend_m3" = "blue", 
                                "trend_m4" = "purple",
                                "y1_detr_m4" = "orange"),
                     labels = c("Trend Lineare", 
                                "Trend Quadratico", 
                                "Trend Cubico", 
                                "Trend Quarto Grado",
                                "Serie detrendizzata m4")) +
  labs(title = "Consumo Fossile settore Industriale",
       x = "Date",
       y = "Energia consumata",
       color = "Modello") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")
## Distribuzione finale della serie detrendizzata
y1_detr_par %>%
  ggplot(data = ., aes(x = y1_detr_m4)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black",
                 bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(y1_detr_par$y1_detr_m4,na.rm=T),
                            sd = sd(y1_detr_par$y1_detr_m4,na.rm = T)),
                aes(col="Norm"),
                size=1.5) + 
  labs(title = "Istogramma",
       subtitle = "Fossile_Industriale: serie detrendizzata",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))


## FONTE RINNOVABILE: Settore Industriale
##### Detrend polinomiale
a1 <- tslm(z1 ~ trend)
a2 <- tslm(z1 ~ trend + I(trend^2))
a3 <- tslm(z1 ~ trend + I(trend^2) + I(trend^3))
a4 <- tslm(z1 ~ trend + I(trend^2) + I(trend^3) + I(trend^4))
## Full sample performances 
perf_a1 <- model_performance(model = a1)
perf_a2 <- model_performance(model = a2)
perf_a3 <- model_performance(model = a3)
perf_a4 <- model_performance(model = a4)
perf <- as.data.frame(rbind(perf_a1,perf_a2,perf_a3,perf_a4))
cbind(Model = c("M1","M2","M3","M4"),perf)
## Aggiungo residui e valori stimati
z1_detr_par <- dataR %>%
  mutate(
    Month = as.Date(time(z1)), # Conversione a Date
    z1 = as.numeric(z1), # Conversione a numerico
    z1_detr_a1 = as.numeric(a1$residuals), trend_m1 = as.numeric(a1$fitted.values),
    z1_detr_a2 = as.numeric(a2$residuals), trend_m2 = as.numeric(a2$fitted.values),
    z1_detr_a3 = as.numeric(a3$residuals), trend_m3 = as.numeric(a3$fitted.values),
    z1_detr_a4 = as.numeric(a4$residuals), trend_m4 = as.numeric(a4$fitted.values))
## Sovrapposizione dei trend stimati alla serie originale
z1_detr_par %>%
  pivot_longer(cols = c(trend_m1, trend_m2, trend_m3, trend_m4, z1_detr_a4),
               names_to = "model", values_to = "trend") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y = z1), color = "black", size = 1.0) + # Serie originale
  geom_line(aes(y = trend, col = model), size = 1.0) +  # Trend stimati
  scale_color_manual(values = c("trend_m1" = "red", 
                                "trend_m2" = "green", 
                                "trend_m3" = "blue", 
                                "trend_m4" = "purple",
                                "z1_detr_a4" = "orange"),
                     labels = c("Trend Lineare", 
                                "Trend Quadratico", 
                                "Trend Cubico", 
                                "Trend Quarto Grado",
                                "Serie detrendizzata m4")) +
  labs(title = "Consumo Rinnovabile settore Industriale",
       x = "Tempo",
       y = "Rinnovabile_Industriale",
       color = "Modello") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")
## Distribuzione finale della serie detrendizzata
z1_detr_par %>%
  filter(Source == "Tot_Rinnovabile") %>%
  ggplot(data = ., aes(x = z1_detr_a4)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black",
                 bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(z1_detr_par$z1_detr_a4,na.rm=T),
                            sd = sd(z1_detr_par$z1_detr_a4,na.rm = T)),
                aes(col="Norm"),
                size=1.5) + 
  labs(title = "Istogramma",
       subtitle = "Rinnovabile_Industriale: serie detrendizzata",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))


## FONTE FOSSILE: Settore Energia Elettrica
##### Detrend polinomiale
e1 <- tslm(y2 ~ trend)
e2 <- tslm(y2 ~ trend + I(trend^2))
e3 <- tslm(y2 ~ trend + I(trend^2) + I(trend^3))
e4 <- tslm(y2 ~ trend + I(trend^2) + I(trend^3) + I(trend^4))
e5 <- tslm(y2 ~ trend + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5))
## Full sample performances 
perf_e1 <- model_performance(model = e1)
perf_e2 <- model_performance(model = e2)
perf_e3 <- model_performance(model = e3)
perf_e4 <- model_performance(model = e4)
perf_e5 <- model_performance(model = e5)
perf <- as.data.frame(rbind(perf_e1,perf_e2,perf_e3,perf_e4, perf_e5))
cbind(Model = c("M1","M2","M3","M4", "M5"),perf)
## Aggiungo residui e valori stimati
y2_detr_par <- dataF %>%
  mutate(
    Month = as.Date(time(y2)), # Conversione a Date
    y2 = as.numeric(y2),       # Conversione a numerico
    y2_detr_e1 = as.numeric(e1$residuals), trend_m1 = as.numeric(e1$fitted.values),
    y2_detr_e2 = as.numeric(e2$residuals), trend_m2 = as.numeric(e2$fitted.values),
    y2_detr_e3 = as.numeric(e3$residuals), trend_m3 = as.numeric(e3$fitted.values),
    y2_detr_e4 = as.numeric(e4$residuals), trend_m4 = as.numeric(e4$fitted.values))
## Sovrapposizione dei trend stimati alla serie originale
y2_detr_par %>%
  pivot_longer(cols = c(trend_m1, trend_m2, trend_m3, trend_m4, y2_detr_e3),
               names_to = "model", values_to = "trend") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y = y2), color = "black", size = 0.8) + # Serie originale
  geom_line(aes(y = trend, col = model), size = 1.2) + # Trend stimati
  scale_color_manual(values = c("trend_m1" = "red", 
                                "trend_m2" = "green", 
                                "trend_m3" = "blue", 
                                "trend_m4" = "purple",
                                "y2_detr_e3" = "orange"),
                     labels = c("Trend Lineare", 
                                "Trend Quadratico", 
                                "Trend Cubico", 
                                "Trend Quarto Grado",
                                "Serie detrendizzata m3")) +
  labs(title = "Consumo Fossile settore Energia Elettrica",
       x = "Tempo",
       y = "Fossile_Energia Elettrica",
       color = "Modello") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")
## Distribuzione finale della serie detrendizzata
y2_detr_par %>%
  filter(Source == "Tot_Fossile") %>%
  ggplot(data = ., aes(x = y2_detr_e3)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black",
                 bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(y2_detr_par$y2_detr_e3,na.rm=T),
                            sd = sd(y2_detr_par$y2_detr_e3,na.rm = T)),
                aes(col="Norm"),
                size=1.5) + 
  labs(title = "Istogramma",
       subtitle = "Fossile_Energia Elettrica: serie detrendizzata",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))


## FONTE RINNOVABILE: Settore Energia Elettrica
##### Detrend polinomiale
t1 <- tslm(clean2R ~ trend)
t2 <- tslm(clean2R ~ trend + I(trend^2))
t3 <- tslm(clean2R ~ trend + I(trend^2) + I(trend^3))
t4 <- tslm(clean2R ~ trend + I(trend^2) + I(trend^3) + I(trend^4))
## Full sample performances 
perf_t1 <- model_performance(model = t1)
perf_t2 <- model_performance(model = t2)
perf_t3 <- model_performance(model = t3)
perf_t4 <- model_performance(model = t4)
perf <- as.data.frame(rbind(perf_t1,perf_t2,perf_t3,perf_t4))
cbind(Model = c("M1","M2","M3","M4"),perf)
## Aggiungo residui e valori stimati
z2_detr_par <- ts_plot2 %>%
  mutate(
    Month = as.Date(time(clean2R)), # Conversione a Date
    clean2R = as.numeric(clean2R),  # Conversione a numerico
    z2_detr_t1 = as.numeric(t1$residuals), trend_m1 = as.numeric(t1$fitted.values),
    z2_detr_t2 = as.numeric(t2$residuals), trend_m2 = as.numeric(t2$fitted.values),
    z2_detr_t3 = as.numeric(t3$residuals), trend_m3 = as.numeric(t3$fitted.values),
    z2_detr_t4 = as.numeric(t4$residuals), trend_m4 = as.numeric(t4$fitted.values))
## Sovrapposizione dei trend stimati alla serie originale
z2_detr_par %>%
  pivot_longer(cols = c(trend_m1, trend_m2, trend_m3, trend_m4, z2_detr_t4),
               names_to = "model", values_to = "trend") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y = clean2R), color = "black", size = 0.8) + # Serie originale
  geom_line(aes(y = trend, col = model), size = 1.2) +       # Trend stimati
  scale_color_manual(values = c("trend_m1" = "red", 
                                "trend_m2" = "green", 
                                "trend_m3" = "blue", 
                                "trend_m4" = "purple",
                                "z2_detr_t4" = "orange"),
                     labels = c("Trend Lineare", 
                                "Trend Quadratico", 
                                "Trend Cubico", 
                                "Trend Quarto Grado",
                                "Serie detrendizzata m4")) +
  labs(title = "Consumo Rinnovabile settore Energia Elettrica",
       x = "Tempo",
       y = "Rinnovabile_Energia Elettrica",
       color = "Modello") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")
## Distribuzione finale della serie detrendizzata
z2_detr_par %>%
  ggplot(data = ., aes(x = z2_detr_t4)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black",
                 bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(z2_detr_par$z2_detr_t4,na.rm=T),
                            sd = sd(z2_detr_par$z2_detr_t4,na.rm = T)),
                aes(col="Norm"),
                size=1.5) + 
  labs(title = "Istogramma",
       subtitle = "Rinnovabile_Energia Elettrica: serie detrendizzata",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))


## FONTE FOSSILE: Settore Residenziale
## In questo caso si esegue la detrendizazzione ma non per correggere la stazionarietà in media (come visto nel test ADF di prima)
##### Detrend polinomiale
o1 <- tslm(y3 ~ trend)
o2 <- tslm(y3 ~ trend + I(trend^2))
o3 <- tslm(y3 ~ trend + I(trend^2) + I(trend^3))
o4 <- tslm(y3 ~ trend + I(trend^2) + I(trend^3) + I(trend^4))
## Full sample performances 
perf_o1 <- model_performance(model = o1)
perf_o2 <- model_performance(model = o2)
perf_o3 <- model_performance(model = o3)
perf_o4 <- model_performance(model = o4)
perf <- as.data.frame(rbind(perf_o1,perf_o2,perf_o3,perf_o4))
cbind(Model = c("M1","M2","M3","M4"),perf)
## Aggiungo residui e valori stimati
y3_detr_par <- dataF %>%
  mutate(
    Month = as.Date(time(y3)), # Conversione a Date
    y3 = as.numeric(y3), # Conversione a numerico
    y3_detr_o1 = as.numeric(o1$residuals), trend_m1 = as.numeric(o1$fitted.values),
    y3_detr_o2 = as.numeric(o2$residuals), trend_m2 = as.numeric(o2$fitted.values),
    y3_detr_o3 = as.numeric(o3$residuals), trend_m3 = as.numeric(o3$fitted.values),
    y3_detr_o4 = as.numeric(o4$residuals), trend_m4 = as.numeric(o4$fitted.values))
## Sovrapposizione dei trend stimati alla serie originale
y3_detr_par %>%
  pivot_longer(cols = c(trend_m1, trend_m2, trend_m3, trend_m4),
               names_to = "model", values_to = "trend") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y = y3), color = "black", size = 0.8) + # Serie originale
  geom_line(aes(y = trend, col = model), size = 1.2) +  # Trend stimati
  scale_color_manual(values = c("trend_m1" = "red", 
                                "trend_m2" = "green", 
                                "trend_m3" = "blue", 
                                "trend_m4" = "purple"),
                     labels = c("Trend Lineare", 
                                "Trend Quadratico", 
                                "Trend Cubico", 
                                "Trend Quarto Grado")) +
  labs(title = "Consumo Fossile settore Residenziale",
       x = "Tempo",
       y = "Fossile_Energia Residenziale",
       color = "Modello") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")



## FONTE RINNOVABILE: Settore Residenziale
##### Differenza prima: svolgo la differenziazione perchè la serie non risulta stazionaria
z3Diff <- dataR %>%
  mutate(D1_z3 = difference(Residenziale, lag = 1))    #Differenza prima
# Visualizzazione dei risultati
z3Diff %>%
  select(Date_idx, D1_z3) %>%
  pivot_longer(cols = -c(Date_idx), names_to = "Series", values_to = "Value") %>%
  ggplot(aes(x = Date_idx)) + 
  geom_line(aes(y = Value)) + 
  facet_wrap(~Series)

# ADF sulla serie originale (svolta in precedenza)
library(urca)
summary(ADF_z3)
summary(ADF_z3_const)
summary(ADF_z3_const_trend)
# ADF sulla serie differenziata
summary(urca::ur.df(y = z3Diff$D1_z3[-1], type = "none",selectlags = "AIC"))
summary(urca::ur.df(y = z3Diff$D1_z3[-1], type = "drift",selectlags = "AIC"))
summary(urca::ur.df(y = z3Diff$D1_z3[-1], type = "trend",selectlags = "AIC"))
## ora la serie risulta stazionaria

## Distribuzione finale della serie differenziata
z3Diff %>%
  ggplot(data = ., aes(x = D1_z3)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black",
                 bins=40) +
  geom_density(aes(col="Dens"),size=1.5) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(z3Diff$D1_z3,na.rm=T),
                            sd = sd(z3Diff$D1_z3,na.rm = T)),
                aes(col="Norm"),
                size=1.5) + 
  labs(title = "Istogramma",
       subtitle = "Rinnovabile_Residenziale: serie differenziata",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

  
  

#########################################################
##### 3.Destagionalizzazione (regressione armonica) #####  
#########################################################

## FONTE FOSSILE: Settore Industriale
# Regressione armonica + trend lineare
K1 <- tslm(clean1F ~ trend + fourier(x = clean1F, K = 1))
K2 <- tslm(clean1F ~ trend + fourier(x = clean1F, K = 2))
K3 <- tslm(clean1F ~ trend + fourier(x = clean1F, K = 3))
K4 <- tslm(clean1F ~ trend + fourier(x = clean1F, K = 4))
K5 <- tslm(clean1F ~ trend + fourier(x = clean1F, K = 5))
K6 <- tslm(clean1F ~ trend + fourier(x = clean1F, K = 6))
# Valori fittati e destagionalizzati
clean1F_deseas <- ts_plot1 %>%
  mutate(clean1F_deseas_m1 = K1$residuals, seas_m1 = K1$fitted.values,
         clean1F_deseas_m2 = K2$residuals, seas_m2 = K2$fitted.values,
         clean1F_deseas_m3 = K3$residuals, seas_m3 = K3$fitted.values,
         clean1F_deseas_m4 = K4$residuals, seas_m4 = K4$fitted.values,
         clean1F_deseas_m5 = K5$residuals, seas_m5 = K5$fitted.values,
         clean1F_deseas_m6 = K6$residuals, seas_m6 = K6$fitted.values)
# Selezione del modello 
perf_m1 <- model_performance(model = K1)
perf_m2 <- model_performance(model = K2)
perf_m3 <- model_performance(model = K3)
perf_m4 <- model_performance(model = K4)
perf_m5 <- model_performance(model = K5)
perf_m6 <- model_performance(model = K6)
perf <- as.data.frame(rbind(perf_m1,perf_m2,perf_m3,perf_m4,perf_m5,perf_m6))
cbind(Model = c("M1","M2","M3","M4","M5","M6"),perf)
# Trend + stagionalità stimati + serie originale
clean1F_deseas %>%
  pivot_longer(cols = c(seas_m6, Cleaned),
               names_to = "model", values_to = "seas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y=seas)) + 
  geom_line(aes(y=seas, col = model), linewidth =1.0)
# Trend + stagionalità stimati 
clean1F_deseas %>%
  pivot_longer(cols = c(seas_m6),
               names_to = "model", values_to = "seas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y=seas)) + 
  facet_wrap(~ model)
# Serie destagionalizzate (e detrendizzate)
clean1F_deseas %>%
  pivot_longer(cols = c(clean1F_deseas_m6),
               names_to = "model", values_to = "deseas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y = deseas)) + 
  facet_wrap(~ model) +
  ggtitle("Serie destagionalizzata (e detrendizzata): 
  Fossile_Industriale")
# Test ADF
summary(urca::ur.df(y = clean1F_deseas$clean1F_deseas_m6, type = "none",selectlags = "AIC"))



## FONTE RINNOVABILE: Settore Industriale
# Regressione armonica + trend lineare
K1 <- tslm(z1 ~ trend + fourier(x = z1, K = 1))
K2 <- tslm(z1 ~ trend + fourier(x = z1, K = 2))
K3 <- tslm(z1 ~ trend + fourier(x = z1, K = 3))
K4 <- tslm(z1 ~ trend + fourier(x = z1, K = 4))
K5 <- tslm(z1 ~ trend + fourier(x = z1, K = 5))
K6 <- tslm(z1 ~ trend + fourier(x = z1, K = 6))
# Valori fittati e destagionalizzati
z1_deseas <- dataR %>%
  select(Month, Industriale) %>%
  mutate(z1_deseas_m1 = K1$residuals, seas_m1 = K1$fitted.values,
         z1_deseas_m2 = K2$residuals, seas_m2 = K2$fitted.values,
         z1_deseas_m3 = K3$residuals, seas_m3 = K3$fitted.values,
         z1_deseas_m4 = K4$residuals, seas_m4 = K4$fitted.values,
         z1_deseas_m5 = K5$residuals, seas_m5 = K5$fitted.values,
         z1_deseas_m6 = K6$residuals, seas_m6 = K6$fitted.values)
# Selezione del modello 
perf_m1 <- model_performance(model = K1)
perf_m2 <- model_performance(model = K2)
perf_m3 <- model_performance(model = K3)
perf_m4 <- model_performance(model = K4)
perf_m5 <- model_performance(model = K5)
perf_m6 <- model_performance(model = K6)
perf <- as.data.frame(rbind(perf_m1, perf_m2, perf_m3, perf_m4, perf_m5, perf_m6))
cbind(Model = c("M1","M2","M3","M4","M5","M6"),perf)
# Trend + stagionalità stimati + serie originale
z1_deseas %>%
  pivot_longer(cols = c(seas_m5, Industriale),
               names_to = "model", values_to = "seas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y=seas)) + 
  geom_line(aes(y=seas, col = model), linewidth =1.0)
# Trend + stagionalità stimati
z1_deseas %>%
  pivot_longer(cols = c(seas_m5),
               names_to = "model", values_to = "seas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y=seas)) + 
  facet_wrap(~ model)
# Serie destagionalizzate (e detrendizzate)
z1_deseas %>%
  pivot_longer(cols = c(z1_deseas_m5),
               names_to = "model", values_to = "deseas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y=deseas)) + 
  facet_wrap(~ model) +
  ggtitle("Serie destagionalizzata (e detrendizzata): 
  Rinnovabile_Industriale")
# Test ADF
summary(urca::ur.df(y = z1_deseas$z1_deseas_m5, type = "none",selectlags = "AIC"))



## FONTE FOSSILE: Settore Energia Elettrica
# Regressione armonica + trend lineare
K1 <- tslm(y2 ~ trend + fourier(x = y2, K = 1))
K2 <- tslm(y2 ~ trend + fourier(x = y2, K = 2))
K3 <- tslm(y2 ~ trend + fourier(x = y2, K = 3))
K4 <- tslm(y2 ~ trend + fourier(x = y2, K = 4))
K5 <- tslm(y2 ~ trend + fourier(x = y2, K = 5))
K6 <- tslm(y2 ~ trend + fourier(x = y2, K = 6))
# Valori fittati e destagionalizzati
y2_deseas <- dataF %>%
  select(Month, Energia_Elettrica) %>%
  mutate(y2_deseas_m1 = K1$residuals, seas_m1 = K1$fitted.values,
         y2_deseas_m2 = K2$residuals, seas_m2 = K2$fitted.values,
         y2_deseas_m3 = K3$residuals, seas_m3 = K3$fitted.values,
         y2_deseas_m4 = K4$residuals, seas_m4 = K4$fitted.values,
         y2_deseas_m5 = K5$residuals, seas_m5 = K5$fitted.values,
         y2_deseas_m6 = K6$residuals, seas_m6 = K6$fitted.values)
# Selezione del modello 
perf_m1 <- model_performance(model = K1)
perf_m2 <- model_performance(model = K2)
perf_m3 <- model_performance(model = K3)
perf_m4 <- model_performance(model = K4)
perf_m5 <- model_performance(model = K5)
perf_m6 <- model_performance(model = K6)
perf <- as.data.frame(rbind(perf_m1, perf_m2, perf_m3, perf_m4, perf_m5, perf_m6))
cbind(Model = c("M1","M2","M3","M4","M5","M6"),perf)
# Trend + stagionalità stimati + serie originale
y2_deseas %>%
  pivot_longer(cols = c(seas_m4, Energia_Elettrica),
               names_to = "model", values_to = "seas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y=seas)) + 
  geom_line(aes(y=seas, col = model), linewidth =1.0)
# Trend + stagionalità stimati
y2_deseas %>%
  pivot_longer(cols = c(seas_m4),
               names_to = "model", values_to = "seas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y=seas)) + 
  facet_wrap(~ model)
# Serie destagionalizzate (e detrendizzate)
y2_deseas %>%
  pivot_longer(cols = c(y2_deseas_m4),
               names_to = "model", values_to = "deseas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y=deseas)) + 
  facet_wrap(~ model) +
  ggtitle("Serie destagionalizzata (e detrendizzata): 
  Fossile_Energia Elettrica")
# Test ADF
summary(urca::ur.df(y = y2_deseas$y2_deseas_m4, type = "none",selectlags = "AIC"))



## FONTE RINNOVABILE: Settore Energia Elettrica
K1 <- tslm(clean2R ~ trend + fourier(x = clean2R, K = 1))
K2 <- tslm(clean2R ~ trend + fourier(x = clean2R, K = 2))
K3 <- tslm(clean2R ~ trend + fourier(x = clean2R, K = 3))
K4 <- tslm(clean2R ~ trend + fourier(x = clean2R, K = 4))
K5 <- tslm(clean2R ~ trend + fourier(x = clean2R, K = 5))
K6 <- tslm(clean2R ~ trend + fourier(x = clean2R, K = 6))
# Valori fittati e destagionalizzati
z2_deseas <- ts_plot2 %>%
  select(Month, Cleaned) %>%
  mutate(z2_deseas_m1 = K1$residuals, seas_m1 = K1$fitted.values,
         z2_deseas_m2 = K2$residuals, seas_m2 = K2$fitted.values,
         z2_deseas_m3 = K3$residuals, seas_m3 = K3$fitted.values,
         z2_deseas_m4 = K4$residuals, seas_m4 = K4$fitted.values,
         z2_deseas_m5 = K5$residuals, seas_m5 = K5$fitted.values,
         z2_deseas_m6 = K6$residuals, seas_m6 = K6$fitted.values)
# Selezione del modello 
perf_m1 <- model_performance(model = K1)
perf_m2 <- model_performance(model = K2)
perf_m3 <- model_performance(model = K3)
perf_m4 <- model_performance(model = K4)
perf_m5 <- model_performance(model = K5)
perf_m6 <- model_performance(model = K6)
perf <- as.data.frame(rbind(perf_m1, perf_m2, perf_m3, perf_m4, perf_m5, perf_m6))
cbind(Model = c("M1","M2","M3","M4","M5","M6"),perf)
# Trend + stagionalità stimati + serie originale
z2_deseas %>%
  pivot_longer(cols = c(seas_m2, Cleaned),
               names_to = "model", values_to = "seas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y=seas)) + 
  geom_line(aes(y=seas, col = model), linewidth =1.0)
# Trend + stagionalità stimati
z2_deseas %>%
  pivot_longer(cols = c(seas_m2),
               names_to = "model", values_to = "seas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y=seas)) + 
  facet_wrap(~ model)
# Serie destagionalizzate (e detrendizzate)
z2_deseas %>%
  pivot_longer(cols = c(z2_deseas_m2),
               names_to = "model", values_to = "deseas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y=deseas)) + 
  facet_wrap(~ model) +
  ggtitle("Serie destagionalizzata (e detrendizzata): 
  Rinnovabile_Energia Elettrica")
# Test ADF
summary(urca::ur.df(y = z2_deseas$z2_deseas_m2, type = "none",selectlags = "AIC"))



## FONTE FOSSILE: Settore Residenziale
# Regressione armonica + trend lineare
K1 <- tslm(y3 ~ trend + fourier(x = y3, K = 1))
K2 <- tslm(y3 ~ trend + fourier(x = y3, K = 2))
K3 <- tslm(y3 ~ trend + fourier(x = y3, K = 3))
K4 <- tslm(y3 ~ trend + fourier(x = y3, K = 4))
K5 <- tslm(y3 ~ trend + fourier(x = y3, K = 5))
K6 <- tslm(y3 ~ trend + fourier(x = y3, K = 6))
# Valori fittati e destagionalizzati
y3_deseas <- dataF %>%
  select(Month, Residenziale) %>%
  mutate(y3_deseas_m1 = K1$residuals, seas_m1 = K1$fitted.values,
         y3_deseas_m2 = K2$residuals, seas_m2 = K2$fitted.values,
         y3_deseas_m3 = K3$residuals, seas_m3 = K3$fitted.values,
         y3_deseas_m4 = K4$residuals, seas_m4 = K4$fitted.values,
         y3_deseas_m5 = K5$residuals, seas_m5 = K5$fitted.values,
         y3_deseas_m6 = K6$residuals, seas_m6 = K6$fitted.values)
# Selezione del modello 
perf_m1 <- model_performance(model = K1)
perf_m2 <- model_performance(model = K2)
perf_m3 <- model_performance(model = K3)
perf_m4 <- model_performance(model = K4)
perf_m5 <- model_performance(model = K5)
perf_m6 <- model_performance(model = K6)
perf <- as.data.frame(rbind(perf_m1,perf_m2,perf_m3,perf_m4,perf_m5,perf_m6))
cbind(Model = c("M1","M2","M3","M4","M5","M6"),perf)
# Trend + stagionalità stimati + serie originale
y3_deseas %>%
  pivot_longer(cols = c(seas_m6, Residenziale),
               names_to = "model", values_to = "seas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y=seas)) + 
  geom_line(aes(y=seas, col = model), linewidth =1.0)
# Trend + stagionalità stimati
y3_deseas %>%
  pivot_longer(cols = c(seas_m6),
               names_to = "model", values_to = "seas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y=seas)) + 
  facet_wrap(~ model)
# Serie destagionalizzate (e detrendizzate)
y3_deseas %>%
  pivot_longer(cols = c(y3_deseas_m6),
               names_to = "model", values_to = "deseas") %>%
  ggplot(aes(x = Month)) + 
  geom_line(aes(y=deseas)) + 
  facet_wrap(~ model) +
  ggtitle("Serie destagionalizzata (e detrendizzata): 
  Fossile_Residenziale")
# Test ADF
summary(urca::ur.df(y = y3_deseas$y3_deseas_m6, type = "none",selectlags = "AIC"))




#############################
##### 3.Decomposizione  #####
#############################
clrs <- c("Original" = "blue", "Destag" = "red")   # colori istogrammi


## FONTE FOSSILE: Settore Industriale
### Decomposizione additiva
dec_add <- clean1F %>% 
  decompose(type="additive")
# Componente trend e stagionale 
y1_add_destag <- dec_add$trend + dec_add$random
y1_add_detrend <- dec_add$seasonal + dec_add$random

# Rappresentazione serie orignale con le sue componenti
autoplot(dec_add) + 
  xlab("Year") +
  ggtitle("Decomposizione additiva per Fossile settore Industriale")
# Serie originale e destagionalizzata
autoplot(clean1F,series = "Original") + 
  autolayer(y1_add_destag,series = "Destag", size=1.01) + 
  labs(x="Year",
       title = "Decomposizione additiva per Fossile settore Industriale",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=clrs,breaks = c("Original","Destag"))
# Serie originale e detrendizzata
autoplot(clean1F,series = "Original") + 
  autolayer(y1_add_detrend,series = "Detrend") + 
  labs(x="Year",
       title = "Decomposizione additiva per Fossile settore Industriale",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=clrs,breaks = c("Original","Detrend"))


## FONTE FOSSILE: Settore Energia Elettrica
### Decomposizione additiva
dec_add <- y2 %>% 
  decompose(type="additive")
# Componente trend e stagionale 
y2_add_destag <- dec_add$trend + dec_add$random
y2_add_detrend <- dec_add$seasonal + dec_add$random

# Rappresentazione serie originale con le sue componenti
autoplot(dec_add) + 
  xlab("Year") +
  ggtitle("Decomposizione additiva per Fossile settore Energia Elettrica")
# Serie originale e destagionalizzata
autoplot(y2,series = "Original") + 
  autolayer(y2_add_destag,series = "Destag", size=1.01) + 
  labs(x="Year",
       title = "Decomposizione additiva per Fossile settore Energia Elettrica",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=clrs,breaks = c("Original","Destag"))
# Serie originale e detrendizzata
autoplot(y2,series = "Original") + 
  autolayer(y2_add_detrend,series = "Detrend") + 
  labs(x="Year",
       title = "Decomposizione additiva per Fossile settore Energia Elettrica",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=clrs,breaks = c("Original","Detrend"))


## FONTE FOSSILE: Settore Residenziale
### Decomposizione additiva
dec_add <- y3 %>% 
  decompose(type="additive")
# Componente trend e stagionale 
y3_add_destag <- dec_add$trend + dec_add$random
y3_add_detrend <- dec_add$seasonal + dec_add$random

# Rappresentazione serie originale con le sue componenti
autoplot(dec_add) + 
  xlab("Year") +
  ggtitle("Decomposizione additiva per Fossile settore Residenziale")
# Serie originale e destagionalizzata
autoplot(y3,series = "Original") + 
  autolayer(y3_add_destag,series = "Destag", size=1.01) + 
  labs(x="Year",
       title = "Decomposizione additiva per Fossile settore Residenziale",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=clrs,breaks = c("Original","Destag"))


## FONTE RINNOVABILE: Settore Industriale
### Decomposizione additiva
dec_add <- z1 %>% 
  decompose(type="additive")
# Componente trend e stagionale 
z1_add_destag <- dec_add$trend + dec_add$random
z1_add_detrend <- dec_add$seasonal + dec_add$random

# Rappresentazione serie originale con le sue componenti
autoplot(dec_add) + 
  xlab("Year") +
  ggtitle("Decomposizione additiva per Rinnovabile settore Industriale")
# Serie originale e destagionalizzata
autoplot(z1,series = "Original") + 
  autolayer(z1_add_destag,series = "Destag", size=1.01) + 
  labs(x="Year",
       title = "Decomposizione additiva per Rinnovabile settore Industriale",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=clrs,breaks = c("Original","Destag"))
# Serie originale e detrendizzata
autoplot(z1,series = "Original") + 
  autolayer(z1_add_detrend,series = "Detrend") + 
  labs(x="Year",
       title = "Decomposizione additiva per Rinnovabile settore Industriale",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=clrs,breaks = c("Original","Detrend"))


## FONTE RINNOVABILE: Settore Energia Elettrica
### Decomposizione additiva
dec_add <- clean2R %>% 
  decompose(type="additive")
# Componente trend e stagionale 
z2_add_destag <- dec_add$trend + dec_add$random
z2_add_detrend <- dec_add$seasonal + dec_add$random

# Rappresentazione serie originale con le sue componenti
autoplot(dec_add) + 
  xlab("Year") +
  ggtitle("Decomposizione additiva per Rinnovabile settore Energia Elettrica")
# Serie originale e destagionalizzata
autoplot(clean2R,series = "Original") + 
  autolayer(z2_add_destag,series = "Destag", size=1.01) + 
  labs(x="Year",
       title = "Decomposizione additiva per Rinnovabile settore Energia Elettrica",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=clrs,breaks = c("Original","Destag"))
# Serie originale e detrendizzata
autoplot(clean2R,series = "Original") + 
  autolayer(z2_add_detrend,series = "Detrend") + 
  labs(x="Year",
       title = "Decomposizione additiva per Rinnovabile settore Energia Elettrica",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=clrs,breaks = c("Original","Detrend"))




#######################################################
########## 4.Analisi di correlazione lineare ##########
#######################################################
library(GGally)
  
### FONTE FOSSILE (Petrolio, Carbone, Gas Naturale) ###
#------------------------------------------------------
# Scatterplot per tipologia di settore con linee di tendenza aggregate 
fn_add_lm_loess <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}
dataF %>% 
  ggpairs(columns = c("Industriale","Energia_Elettrica","Residenziale"),
          mapping = aes(color = Source),
          lower = list(continuous = fn_add_lm_loess))


##############################################################
########## Stima dei modelli di regressione lineare ##########
##############################################################

### comando "lm"
# Modello 1
mod_m1F <- lm(formula = Energia_Elettrica ~ Industriale + Residenziale, data = dataF)  
summary(mod_m1F)
plot(mod_m1F)

### comando 'tslm'
dataFt <- dataF %>%   #(creo una versione serie storiche per t_sibble)
  ts_ts()

# Modello 1 (uguale al modello 1 con comando "lm")
m1F <- tslm(formula = Tot_Fossile_Energia_Elettrica ~ Tot_Fossile_Industriale + Tot_Fossile_Residenziale, data = dataFt)  
summary(m1F)
fitted(m1F)  # Valori previsti 
hist(residuals(m1F))  # Residui del modello

# Modello 2
m2F <- tslm(formula = Tot_Fossile_Energia_Elettrica ~ Tot_Fossile_Residenziale, data = dataFt)
summary(m2F)
fitted(m2F)  
hist(residuals(m2F))  

# Modello 3
m3F <- tslm(formula = Tot_Fossile_Energia_Elettrica ~ Tot_Fossile_Industriale, data = dataFt)
summary(m3F)
fitted(m3F)  
hist(residuals(m3F)) 

## Confronto dei modelli
perfo_m1F <- model_performance(m1F)
perfo_m2F <- model_performance(m2F)
perfo_m3F <- model_performance(m3F)
perfoF <- as.data.frame(rbind(perfo_m1F,perfo_m2F,perfo_m3F))  
cbind(Model = c("M1","M2","M3"),perfoF)

## Analisi grafica dei residui del modello M1
checkresiduals(m1F)




### FONTE RINNOVABILE (Energia Solare, Energia Eolica, Energia Geotermica, Energia Biomassa, Energia Idroelettrica) ###
#----------------------------------------------------------------------------------------------------------------------
# Differenzio tutte le serie storiche
modDiff <- dataR %>%
  mutate(D1_Industriale = difference(Industriale, lag = 1),
         D1_Elettrica = difference(Energia_Elettrica, lag = 1),
         D1_Residenziale = difference(Residenziale, lag = 1)) 
# ADF sulle serie differenziate
summary(urca::ur.df(y = modDiff$D1_Industriale[-1], type = "none",selectlags = "AIC"))
summary(urca::ur.df(y = modDiff$D1_Elettrica[-1], type = "none",selectlags = "AIC"))
summary(urca::ur.df(y = modDiff$D1_Residenziale[-1], type = "none",selectlags = "AIC"))

# Sostituisco il primo valore NA con zero
modDiff <- modDiff %>% 
  mutate(across(c(D1_Industriale, D1_Elettrica, D1_Residenziale), ~ replace_na(.x, 0)))


# Scatterplot con linee di tendenza aggregate 
fn_add_lm_loess <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}
modDiff %>%  #prendo la serie differenziata
  ggpairs(columns = c("D1_Industriale","D1_Elettrica","D1_Residenziale"),
          mapping = aes(color = Source),
          lower = list(continuous = fn_add_lm_loess))


##############################################################
########## Stima dei modelli di regressione lineare ##########
##############################################################

### comando "lm"
# Modello 1
mod_m1R <- lm(formula = D1_Residenziale ~ D1_Industriale + D1_Elettrica, data = modDiff)  
summary(mod_m1R)
plot(mod_m1R)

### comando 'tslm'
modDifft <- modDiff %>%   #(creo una versione serie storiche per t_sibble)
  ts_ts()

# Modello 1 (uguale al modello 1 con comando "lm")
m1R <- tslm(formula = Tot_Rinnovabile_D1_Residenziale ~ Tot_Rinnovabile_D1_Industriale + Tot_Rinnovabile_D1_Elettrica, data = modDifft)
summary(m1R)
fitted(m1R)  
hist(residuals(m1R))  

# Modello 2
m2R <- tslm(formula = Tot_Rinnovabile_D1_Residenziale ~ Tot_Rinnovabile_D1_Industriale, data = modDifft)
summary(m2R)
fitted(m2R)  
hist(residuals(m2R))  

# Modello 3
m3R <- tslm(formula = Tot_Rinnovabile_D1_Residenziale ~ Tot_Rinnovabile_D1_Elettrica, data = modDifft)
summary(m3R)
fitted(m3R)  
hist(residuals(m3R))  

## Confronto dei modelli
perfo_m1R <- model_performance(m1R)
perfo_m2R <- model_performance(m2R)
perfo_m3R <- model_performance(m3R)
perfoR <- as.data.frame(rbind(perfo_m1R,perfo_m2R,perfo_m3R))  
cbind(Model = c("M1","M2","M3"),perfoR)

## Analisi grafica dei residui del modello M1
checkresiduals(m1R)




########################################################################
##### 5.Stima e comparazione dei modelli: ARIMA, SARIMA e regARIMA #####
########################################################################
source("FN PerfMetr_SARIMA.R", encoding = 'UTF-8')


## FONTE FOSSILE: Settore Industriale
#-------------------------------------
##### Modellistica a due stadi: destagionalizzazione + ARIMA sui residui #####
### Fase 1. Destagionalizzazione (fatta precedentemente)
K6 <- tslm(clean1F ~ trend + fourier(x = clean1F, K = 6))

### Fase 2. Analisi della serie destagionalizzata con modelli ARIMA
# 2a. Destagionalizzo usando 6 armoniche
res_k6 <- K6$residuals
# 2b. Stima dei modelli ARIMA
AR1 <- Arima(y = res_k6,order = c(1,0,0),include.constant = F,include.drift = F)
AR2 <- Arima(y = res_k6,order = c(2,0,0),include.constant = F,include.drift = F)
MA1 <- Arima(y = res_k6,order = c(0,0,1),include.constant = F,include.drift = F)
MA2 <- Arima(y = res_k6,order = c(0,0,2),include.constant = F,include.drift = F)
ARMA11 <- Arima(y = res_k6,order = c(1,0,1),include.constant = F,include.drift = F)
ARMA21 <- Arima(y = res_k6,order = c(2,0,1),include.constant = F,include.drift = F)
ARMA12 <- Arima(y = res_k6,order = c(1,0,2),include.constant = F,include.drift = F)
ARMA32 <- Arima(y = res_k6,order = c(3,0,2),include.constant = F,include.drift = F)
# 2c. Confronto modelli
(ARIMA_comp1 <- data.frame(cbind(model=c("AR1","AR2","MA1","MA2","ARMA11","ARMA21","ARMA12","ARMA32"),
                                round(rbind(PerfMetr_SARIMA(AR1),
                                            PerfMetr_SARIMA(AR2),
                                            PerfMetr_SARIMA(MA1),
                                            PerfMetr_SARIMA(MA2),
                                            PerfMetr_SARIMA(ARMA11),
                                            PerfMetr_SARIMA(ARMA21),
                                            PerfMetr_SARIMA(ARMA12),
                                            PerfMetr_SARIMA(ARMA32)),2))))
# 2d. Identificazione automatica modello ottimo 
(ARIMA_opt <- auto.arima(y = res_k6, max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,stationary = T))
checkresiduals(ARIMA_opt)    #controllo dei residui del modello ottimo



##### Modellistica diretta con SARIMA #####
### Fase 1. Identificazione modello ottimo (stepwise automatica)
(SARIMA_opt <- auto.arima(y = clean1F, max.p = 3, max.q = 3, seasonal = T, parallel = F, stepwise = F,
                         stationary = F,ic = "aicc"))



##### Modellistica con regARIMA #####
### Fase 1. Identificazione modello ottimo (stepwise automatica) con variabili esogene
(regARIMA_opt <- auto.arima(y = clean1F, xreg = fourier(clean1F,K = 6),
                           max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,
                           stationary = F,ic = "aicc"))


### Metriche per selezionare il modello ottimo tra regressione, ARIMA, SARIMA, regARIMA
(model_comp <- data.frame(cbind(model=c("ARIMA","SARIMA","regARIMA"),
                                 round(rbind(PerfMetr_SARIMA(ARIMA_opt),
                                             PerfMetr_SARIMA(SARIMA_opt),
                                             PerfMetr_SARIMA(regARIMA_opt)),3))))


### Analisi delle innovazioni del modello ottimo selezionato
innov <- SARIMA_opt$residuals
p1 <- autoplot(innov) + 
  labs(title = "Serie storica")
p2 <- as_tsibble(innov) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(innov,na.rm=T),
                            sd = sd(innov,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))
p3 <- as_tsibble(innov) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "Trillion di BTU")
p4 <- gglagplot(innov,set.lags = c(1,2,3,6,12,18,24,36,48)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")
p5 <- ggAcf(innov) + 
  labs(title = "ACF")
p6 <- ggPacf(innov) + 
  labs(title = "PACF")
p <- ggarrange(p1,p2,p3,p4,p5,p6,ncol = 2,nrow = 3)
print(annotate_figure(p,
                      top = text_grob("Innovazioni del modello SARIMA ottimo",
                                      color = "red", face = "bold", size = 14)))




## FONTE FOSSILE: Settore Energia Elettrica
#-------------------------------------------
##### Modellistica a due stadi: destagionalizzazione + ARIMA sui residui #####
### Fase 1. Destagionalizzazione (fatta precedentemente)
K4 <- tslm(y2 ~ trend + fourier(x = y2, K = 4))

### Fase 2. Analisi della serie destagionalizzata con modelli ARIMA
# 2a. Destagionalizzo usando 6 armoniche
res_k4 <- K4$residuals
# 2b. Stima modelli ARIMA
WN <- Arima(y = res_k4,order = c(0,0,0),include.constant = F,include.drift = F)
AR1 <- Arima(y = res_k4,order = c(1,0,0),include.constant = F,include.drift = F)
AR2 <- Arima(y = res_k4,order = c(2,0,0),include.constant = F,include.drift = F)
MA1 <- Arima(y = res_k4,order = c(0,0,1),include.constant = F,include.drift = F)
MA2 <- Arima(y = res_k4,order = c(0,0,2),include.constant = F,include.drift = F)
ARMA11 <- Arima(y = res_k4,order = c(1,0,1),include.constant = F,include.drift = F)
ARMA21 <- Arima(y = res_k4,order = c(2,0,1),include.constant = F,include.drift = F)
ARMA12 <- Arima(y = res_k4,order = c(1,0,2),include.constant = F,include.drift = F)
# 2c. Confronto modelli
(ARIMA_comp1 <- data.frame(cbind(model=c("WN","AR1","AR2","MA1","MA2","ARMA11","ARMA21","ARMA12"),
                                round(rbind(PerfMetr_SARIMA(WN),
                                            PerfMetr_SARIMA(AR1),
                                            PerfMetr_SARIMA(AR2),
                                            PerfMetr_SARIMA(MA1),
                                            PerfMetr_SARIMA(MA2),
                                            PerfMetr_SARIMA(ARMA11),
                                            PerfMetr_SARIMA(ARMA21),
                                            PerfMetr_SARIMA(ARMA12)),2))))
# 2d. Identificazione automatica modello ottimo 
(ARIMA_opt <- auto.arima(y = res_k4, max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,stationary = T))
checkresiduals(ARIMA_opt)    #controllo dei residui del modello ottimo



##### Modellistica diretta con SARIMA #####
### Fase 1. Identificazione modello ottimo (stepwise automatica)
(SARIMA_opt <- auto.arima(y = y2, max.p = 3, max.q = 3, seasonal = T, parallel = F, stepwise = F,
                          stationary = F,ic = "aicc"))



##### Modellistica con regARIMA #####
### Fase 1. Identificazione modello ottimo (stepwise automatica) con variabili esogene
(regARIMA_opt <- auto.arima(y = y2, xreg = fourier(y2,K = 4),
                           max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,
                           stationary = F,ic = "aicc"))


### Metriche per selezionare il modello ottimo tra regressione, ARIMA, SARIMA, regARIMA
(model_comp <- data.frame(cbind(model=c("ARIMA","SARIMA","regARIMA"),
                                 round(rbind(PerfMetr_SARIMA(ARIMA_opt),
                                             PerfMetr_SARIMA(SARIMA_opt),
                                             PerfMetr_SARIMA(regARIMA_opt)),3))))


### Analisi delle innovazioni del modello ottimo selezionato
innov <- SARIMA_opt$residuals
p1 <- autoplot(innov) + 
  labs(title = "Serie storica")
p2 <- as_tsibble(innov) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(innov,na.rm=T),
                            sd = sd(innov,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))
p3 <- as_tsibble(innov) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "Trillion di BTU")
p4 <- gglagplot(innov,set.lags = c(1,2,3,6,12,18,24,36,48)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")
p5 <- ggAcf(innov) + 
  labs(title = "ACF")
p6 <- ggPacf(innov) + 
  labs(title = "PACF")
p <- ggarrange(p1,p2,p3,p4,p5,p6,ncol = 2,nrow = 3)
print(annotate_figure(p,
                      top = text_grob("Innovazioni del modello SARIMA ottimo",
                                      color = "red", face = "bold", size = 14)))




## FONTE FOSSILE: Settore Residenziale
#-------------------------------------
##### Modellistica a due stadi: destagionalizzazione + ARIMA sui residui #####
### Fase 1. Destagionalizzazione (fatta precedentemente)
K6 <- tslm(y3 ~ trend + fourier(x = y3, K = 6))

### Fase 2. Analisi della serie destagionalizzata con modelli ARIMA
# 2a. Destagionalizzo usando 6 armoniche
res_k6 <- K6$residuals
# 2b. Stima modelli ARIMA
WN <- Arima(y = res_k6,order = c(0,0,0),include.constant = F,include.drift = F)
AR1 <- Arima(y = res_k6,order = c(1,0,0),include.constant = F,include.drift = F)
AR2 <- Arima(y = res_k6,order = c(2,0,0),include.constant = F,include.drift = F)
MA1 <- Arima(y = res_k6,order = c(0,0,1),include.constant = F,include.drift = F)
MA2 <- Arima(y = res_k6,order = c(0,0,2),include.constant = F,include.drift = F)
ARMA11 <- Arima(y = res_k6,order = c(1,0,1),include.constant = F,include.drift = F)
ARMA21 <- Arima(y = res_k6,order = c(2,0,1),include.constant = F,include.drift = F)
ARMA12 <- Arima(y = res_k6,order = c(1,0,2),include.constant = F,include.drift = F)
# 2c. Confronto modelli
(ARIMA_comp1 <- data.frame(cbind(model=c("WN","AR1","AR2","MA1","MA2","ARMA11","ARMA21","ARMA12"),
                                round(rbind(PerfMetr_SARIMA(WN),
                                            PerfMetr_SARIMA(AR1),
                                            PerfMetr_SARIMA(AR2),
                                            PerfMetr_SARIMA(MA1),
                                            PerfMetr_SARIMA(MA2),
                                            PerfMetr_SARIMA(ARMA11),
                                            PerfMetr_SARIMA(ARMA21),
                                            PerfMetr_SARIMA(ARMA12)),2))))
# 2d. Identificazione automatica modello ottimo 
(ARIMA_opt <- auto.arima(y = res_k6, max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,stationary = T))
checkresiduals(ARIMA_opt)    #controllo dei residui del modello ottimo



##### Modellistica diretta con SARIMA #####
### Fase 1. Identificazione modello ottimo (stepwise automatica)
(SARIMA_opt <- auto.arima(y = y3, max.p = 3, max.q = 3, seasonal = T, parallel = F, stepwise = F,
                          stationary = F,ic = "aicc"))



##### Modellistica con regARIMA #####
### Fase 1. Identificazione modello ottimo (stepwise automatica) con variabili esogene
(regARIMA_opt <- auto.arima(y = y3, xreg = fourier(y3,K = 6),
                           max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,
                           stationary = F,ic = "aicc"))



### Metriche per selezionare il modello ottimo tra regressione, ARIMA, SARIMA, regARIMA
(model_comp <- data.frame(cbind(model=c("ARIMA","SARIMA","regARIMA"),
                                round(rbind(PerfMetr_SARIMA(ARIMA_opt),
                                            PerfMetr_SARIMA(SARIMA_opt),
                                            PerfMetr_SARIMA(regARIMA_opt)),3))))


### Analisi delle innovazioni del modello ottimo selezionato
innov <- SARIMA_opt$residuals
p1 <- autoplot(innov) + 
  labs(title = "Serie storica")
p2 <- as_tsibble(innov) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(innov,na.rm=T),
                            sd = sd(innov,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))
p3 <- as_tsibble(innov) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "Trillion di BTU")
p4 <- gglagplot(innov,set.lags = c(1,2,3,6,12,18,24,36,48)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")
p5 <- ggAcf(innov) + 
  labs(title = "ACF")
p6 <- ggPacf(innov) + 
  labs(title = "PACF")
p <- ggarrange(p1,p2,p3,p4,p5,p6,ncol = 2,nrow = 3)
print(annotate_figure(p,
                      top = text_grob("Innovazioni del modello SARIMA ottimo",
                                      color = "red", face = "bold", size = 14)))




## FONTE RINNOVABILE: Settore Industriale
#-----------------------------------------
##### Modellistica a due stadi: destagionalizzazione + ARIMA sui residui #####
### Fase 1. Destagionalizzazione (fatta precedentemente)
K5 <- tslm(z1 ~ trend + fourier(x = z1, K = 5))

### Fase 2. Analisi della serie destagionalizzata con modelli ARIMA
# 2a. Destagionalizzo usando 6 armoniche
res_k5 <- K5$residuals
# 2b. Stima dei modelli ARIMA
AR1 <- Arima(y = res_k5,order = c(1,0,0),include.constant = F,include.drift = F)
AR2 <- Arima(y = res_k5,order = c(2,0,0),include.constant = F,include.drift = F)
MA1 <- Arima(y = res_k5,order = c(0,0,1),include.constant = F,include.drift = F)
MA2 <- Arima(y = res_k5,order = c(0,0,2),include.constant = F,include.drift = F)
ARMA11 <- Arima(y = res_k5,order = c(1,0,1),include.constant = F,include.drift = F)
ARMA21 <- Arima(y = res_k5,order = c(2,0,1),include.constant = F,include.drift = F)
ARMA12 <- Arima(y = res_k5,order = c(1,0,2),include.constant = F,include.drift = F)
ARMA32 <- Arima(y = res_k5,order = c(3,0,2),include.constant = F,include.drift = F)
# 2c. Confronto modelli
(ARIMA_comp1 <- data.frame(cbind(model=c("AR1","AR2","MA1","MA2","ARMA11","ARMA21","ARMA12","ARMA32"),
                                 round(rbind(PerfMetr_SARIMA(AR1),
                                             PerfMetr_SARIMA(AR2),
                                             PerfMetr_SARIMA(MA1),
                                             PerfMetr_SARIMA(MA2),
                                             PerfMetr_SARIMA(ARMA11),
                                             PerfMetr_SARIMA(ARMA21),
                                             PerfMetr_SARIMA(ARMA12),
                                             PerfMetr_SARIMA(ARMA32)),2))))
# 2d. Identificazione automatica modello ottimo 
(ARIMA_opt <- auto.arima(y = res_k5, max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,stationary = T))
checkresiduals(ARIMA_opt)    #controllo dei residui del modello ottimo



##### Modellistica diretta con SARIMA #####
### Fase 1. Identificazione modello ottimo (stepwise automatica)
(SARIMA_opt <- auto.arima(y = z1, max.p = 3, max.q = 3, seasonal = T, parallel = F, stepwise = F,
                          stationary = F,ic = "aicc"))



##### Modellistica con regARIMA #####
### Fase 1. Identificazione modello ottimo (stepwise automatica) con variabili esogene
(regARIMA_opt <- auto.arima(y = z1, xreg = fourier(z1,K = 5),
                            max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,
                            stationary = F,ic = "aicc"))


### Metriche per selezionare il modello ottimo tra regressione, ARIMA, SARIMA, regARIMA
(model_comp <- data.frame(cbind(model=c("ARIMA","SARIMA","regARIMA"),
                                round(rbind(PerfMetr_SARIMA(ARIMA_opt),
                                            PerfMetr_SARIMA(SARIMA_opt),
                                            PerfMetr_SARIMA(regARIMA_opt)),3))))


### Analisi delle innovazioni del modello ottimo selezionato
innov <- SARIMA_opt$residuals
p1 <- autoplot(innov) + 
  labs(title = "Serie storica")
p2 <- as_tsibble(innov) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(innov,na.rm=T),
                            sd = sd(innov,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))
p3 <- as_tsibble(innov) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "Trillion di BTU")
p4 <- gglagplot(innov,set.lags = c(1,2,3,6,12,18,24,36,48)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")
p5 <- ggAcf(innov) + 
  labs(title = "ACF")
p6 <- ggPacf(innov) + 
  labs(title = "PACF")
p <- ggarrange(p1,p2,p3,p4,p5,p6,ncol = 2,nrow = 3)
print(annotate_figure(p,
                      top = text_grob("Innovazioni del modello SARIMA ottimo",
                                      color = "red", face = "bold", size = 14)))




## FONTE RINNOVABILE: Settore Energia Elettrica
#-----------------------------------------------
##### Modellistica a due stadi: destagionalizzazione + ARIMA sui residui #####
### Fase 1. Destagionalizzazione (fatta precedentemente)
K2 <- tslm(clean2R ~ trend + fourier(x = clean2R, K = 2))

### Fase 2. Analisi della serie destagionalizzata con modelli ARIMA
# 2a. Destagionalizzo usando 6 armoniche
res_k2 <- K2$residuals
# 2b. Stima dei modelli ARIMA
AR1 <- Arima(y = res_k2,order = c(1,0,0),include.constant = F,include.drift = F)
AR2 <- Arima(y = res_k2,order = c(2,0,0),include.constant = F,include.drift = F)
MA1 <- Arima(y = res_k2,order = c(0,0,1),include.constant = F,include.drift = F)
MA2 <- Arima(y = res_k2,order = c(0,0,2),include.constant = F,include.drift = F)
ARMA11 <- Arima(y = res_k2,order = c(1,0,1),include.constant = F,include.drift = F)
ARMA21 <- Arima(y = res_k2,order = c(2,0,1),include.constant = F,include.drift = F)
ARMA12 <- Arima(y = res_k2,order = c(1,0,2),include.constant = F,include.drift = F)
# 2c. Confronto modelli
(ARIMA_comp1 <- data.frame(cbind(model=c("AR1","AR2","MA1","MA2","ARMA11","ARMA21","ARMA12"),
                                 round(rbind(PerfMetr_SARIMA(AR1),
                                             PerfMetr_SARIMA(AR2),
                                             PerfMetr_SARIMA(MA1),
                                             PerfMetr_SARIMA(MA2),
                                             PerfMetr_SARIMA(ARMA11),
                                             PerfMetr_SARIMA(ARMA21),
                                             PerfMetr_SARIMA(ARMA12)),2))))
# 2d. Identificazione automatica modello ottimo 
(ARIMA_opt <- auto.arima(y = res_k2, max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,stationary = T))
checkresiduals(ARIMA_opt)    #controllo dei residui del modello ottimo



##### Modellistica diretta con SARIMA #####
### Fase 1. Identificazione modello ottimo (stepwise automatica)
(SARIMA_opt <- auto.arima(y = clean2R, max.p = 3, max.q = 3, seasonal = T, parallel = F, stepwise = F,
                          stationary = F,ic = "aicc"))



##### Modellistica con regARIMA #####
### Fase 1. Identificazione modello ottimo (stepwise automatica) con variabili esogene
(regARIMA_opt <- auto.arima(y = clean2R, xreg = fourier(clean2R,K = 2),
                            max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,
                            stationary = F,ic = "aicc"))


### Metriche per selezionare il modello ottimo tra regressione, ARIMA, SARIMA, regARIMA
(model_comp <- data.frame(cbind(model=c("ARIMA","SARIMA","regARIMA"),
                                round(rbind(PerfMetr_SARIMA(ARIMA_opt),
                                            PerfMetr_SARIMA(SARIMA_opt),
                                            PerfMetr_SARIMA(regARIMA_opt)),3))))


### Analisi delle innovazioni del modello ottimo selezionato
innov <- SARIMA_opt$residuals
p1 <- autoplot(innov) + 
  labs(title = "Serie storica")
p2 <- as_tsibble(innov) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(innov,na.rm=T),
                            sd = sd(innov,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))
p3 <- as_tsibble(innov) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "Trillion di BTU")
p4 <- gglagplot(innov,set.lags = c(1,2,3,6,12,18,24,36,48)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")
p5 <- ggAcf(innov) + 
  labs(title = "ACF")
p6 <- ggPacf(innov) + 
  labs(title = "PACF")
p <- ggarrange(p1,p2,p3,p4,p5,p6,ncol = 2,nrow = 3)
print(annotate_figure(p,
                      top = text_grob("Innovazioni del modello SARIMA ottimo",
                                      color = "red", face = "bold", size = 14)))




## FONTE RINNOVABILE: Settore Residenziale
#------------------------------------------
##### Modellistica a due stadi: destagionalizzazione + ARIMA sui residui #####
# 1a. Stimo i modelli con armoniche da 1 a 6
k1 <- tslm(z3 ~ trend + fourier(z3,K = 1))
k2 <- tslm(z3 ~ trend + fourier(z3,K = 2))
k3 <- tslm(z3 ~ trend + fourier(z3,K = 3))
k4 <- tslm(z3 ~ trend + fourier(z3,K = 4))
k5 <- tslm(z3 ~ trend + fourier(z3,K = 5))
k6 <- tslm(z3 ~ trend + fourier(z3,K = 6))
# 1b. Comparazione delle performance
(arm_comp <- data.frame(cbind(model=c("k1","k2","k3","k4","k5","k6"),
                             rbind(CV(k1),CV(k2),CV(k3),CV(k4),CV(k5),CV(k6)))))

### Fase 2. Analisi della serie destagionalizzata con modelli ARIMA
# 2a. Destagionalizzo usando 6 armoniche
res_k1 <- k1$residuals
# 2b. Stima dei modelli ARIMA
AR1 <- Arima(y = res_k1,order = c(1,0,0),include.constant = F,include.drift = F)
AR2 <- Arima(y = res_k1,order = c(2,0,0),include.constant = F,include.drift = F)
MA1 <- Arima(y = res_k1,order = c(0,0,1),include.constant = F,include.drift = F)
MA2 <- Arima(y = res_k1,order = c(0,0,2),include.constant = F,include.drift = F)
ARMA11 <- Arima(y = res_k1,order = c(1,0,1),include.constant = F,include.drift = F)
ARMA21 <- Arima(y = res_k1,order = c(2,0,1),include.constant = F,include.drift = F)
ARMA12 <- Arima(y = res_k1,order = c(1,0,2),include.constant = F,include.drift = F)
ARMA31 <- Arima(y = res_k1,order = c(3,0,1),include.constant = F,include.drift = F)
# 2c. Confronto modelli
(ARIMA_comp1 <- data.frame(cbind(model=c("AR1","AR2","MA1","MA2","ARMA11","ARMA21","ARMA12","ARMA31"),
                                 round(rbind(PerfMetr_SARIMA(AR1),
                                             PerfMetr_SARIMA(AR2),
                                             PerfMetr_SARIMA(MA1),
                                             PerfMetr_SARIMA(MA2),
                                             PerfMetr_SARIMA(ARMA11),
                                             PerfMetr_SARIMA(ARMA21),
                                             PerfMetr_SARIMA(ARMA12),
                                             PerfMetr_SARIMA(ARMA31)),2))))
# 2d. Identificazione automatica modello ottimo 
(ARIMA_opt <- auto.arima(y = res_k1, max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,stationary = F))  #"stationary = F" poichè ho applicato la serie storica originale
checkresiduals(ARIMA_opt)    #controllo dei residui del modello ottimo



##### Modellistica diretta con SARIMA #####
### Fase 1. Identificazione modello ottimo (stepwise automatica)
(SARIMA_opt <- auto.arima(y = z3, max.p = 3, max.q = 3, seasonal = T, parallel = F, stepwise = F,
                          stationary = F,ic = "aicc"))



##### Modellistica con regARIMA #####
### Fase 1. Identificazione modello ottimo (stepwise automatica) con variabili esogene
(regARIMA_opt <- auto.arima(y = z3, xreg = fourier(z3,K = 1),
                            max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,
                            stationary = F,ic = "aicc"))


### Metriche per selezionare il modello ottimo tra regressione, ARIMA, SARIMA, regARIMA
(model_comp <- data.frame(cbind(model=c("ARIMA","SARIMA","regARIMA"),
                                round(rbind(PerfMetr_SARIMA(ARIMA_opt),
                                            PerfMetr_SARIMA(SARIMA_opt),
                                            PerfMetr_SARIMA(regARIMA_opt)),3))))


### Analisi delle innovazioni del modello ottimo selezionato
innov <- SARIMA_opt$residuals
p1 <- autoplot(innov) + 
  labs(title = "Serie storica")
p2 <- as_tsibble(innov) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(innov,na.rm=T),
                            sd = sd(innov,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       x = "Trillion di BTU",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))
p3 <- as_tsibble(innov) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "Trillion di BTU")
p4 <- gglagplot(innov,set.lags = c(1,2,3,6,12,18,24,36,48)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")
p5 <- ggAcf(innov) + 
  labs(title = "ACF")
p6 <- ggPacf(innov) + 
  labs(title = "PACF")
p <- ggarrange(p1,p2,p3,p4,p5,p6,ncol = 2,nrow = 3)
print(annotate_figure(p,
                      top = text_grob("Innovazioni del modello SARIMA ottimo",
                                      color = "red", face = "bold", size = 14)))






