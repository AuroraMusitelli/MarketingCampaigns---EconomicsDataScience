#### PROGETTO ECONOMICS FOR DATA SCIENCE #### 
library(ggplot2)
#1 carico dati
dati <- read.csv("data.csv")


#Explorative analysis and preprocessing
dati$gender <- as.factor(dati$gender)
dati$region <- as.factor(dati$region)
dati$device_type <- as.factor(dati$device_type)
dati$marital_status <- as.factor(dati$marital_status)
dati$loyalty_tier <- as.factor(dati$loyalty_tier)
dati$group <- as.factor(dati$group)


dati$tr1 <- NULL
dati$tr2 <- NULL
dati$tr3 <- NULL

sum(is.na(dati)) #non abbiamo valori mancanti
dati$clicks[dati$clicks < 0] <- 0
dati$clicks[dati$income < 0] <- 0
#facciamo una analisi esplorativa dei valori all'intenro di alcujne variabili che riteniamo siano importanti per la ricerca

ggplot(dati, aes(x = group, y = clicks)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal()
#1. Le campagne sembrano avere effetto... ma non sempre positivo
#Tutti i gruppi con esposizione a campagne (Cam1, Cam2, Cam3, combinazioni) mostrano una distribuzione di click mediamente piÃ¹ alta rispetto al gruppo None (nessuna esposizione).
#Tuttavia, l'effetto non Ã¨ uniforme: alcune combinazioni sembrano meno efficaci o addirittura peggiori del singolo trattamento.

#2. Le campagne piÃ¹ promettenti (visivamente)
#Cam1_3 e Cam1_2_3 sembrano avere mediane piÃ¹ alte dei click rispetto agli altri gruppi. Questo puÃ² suggerire che combinazioni di campagne abbiano un effetto cumulativo o sinergico.
#Anche Cam1 da sola sembra leggermente piÃ¹ performante della media.

#3. Il gruppo None non Ã¨ il piÃ¹ basso
#Curiosamente, la mediana dei click nel gruppo None non Ã¨ la piÃ¹ bassa. Questo potrebbe suggerire che:
#la selezione per esposizione alle campagne non Ã¨ casuale,
#oppure alcune campagne non hanno avuto un impatto positivo sul comportamento degli utenti.

#4. Variabilità e outlier
#Alcuni gruppi (soprattutto Cam1, Cam3, e None) mostrano una grande variabilità e 
#molti outlier potrebbe esserci eterogeneità nei segmenti di utenti oppure alcuni utenti iperattivi.


#Serve un'analisi causale: le differenze visibili nei boxplot non bastano per dire che 
#una campagna Ã¨ piÃ¹ efficace. Potrebbero esserci fattori confondenti (es. età, income, internet usage...).
#quello che possiamo dire inizialmente Ã¨ 
#Cam1 e Cam1_3 sembrano efficaci,
#la sola esposizione a Cam2 non mostra particolari benefici.

#possiamo fare:
#propensity score matching per stimare l'effetto netto delle campagne sul numero di click, controllando per altre variabili utente.


numeric_vars <- dati[, sapply(dati, is.numeric)]
ggcorrplot::ggcorrplot(cor(numeric_vars), lab=TRUE, type="upper")
#questo grafico suggerisce che tra income e age vi Ã¨ una correlazione positiva alta
#piu sei vecchio piu guadagni (logico)
#tra age e clicks correlazione negetiva indicando che piu sei vecchio meno click esegui 
#idem tra income e clicks suggerendo che 
#puÃ² riflettere una maggiore propensione all'interazione digitale da parte dei giovani o una maggiore attrattività delle campagne per target economicamente piÃ¹ sensibili.
#nessuna correlazione tra le altre  Questo significa che, almeno in media, queste variabili non sembrano associate ai click in modo lineare.

#Età e reddito sono fortemente correlati tra loro (0.79) --> multicollinearità eliminare??

#IMPLICAZIONI STRATEGICHE:
# Le campagne potrebbero essere più efficaci se mirate a utenti più giovani e con reddito medio-basso.
# età sembra un predittore forte di comportamento (click), quindi potresti stratificare la tua analisi causale anche per fasce d'età.


#CLIKCS PER DEVICE_TYPE
ggplot(dati, aes(x = device_type, y = clicks)) +
  geom_boxplot() +
  facet_wrap(~ region) + theme_bw()
#le campagne sono piÃ¹ efficaci su dispositivi mobili, o che gli utenti mobile sono più reattivi in tutte e tre le regioni
# il device sembra essere piÃ¹ importante della regione geografica nel determinare engagement. perche non ci sono grosse differenze tra regioni italiane

#IMPLICAZIONI 
#Le campagne potrebbero essere ottimizzate per dispositivi mobili, dato che l'engagement Ã¨ sistematicamente piÃ¹ alto su Mobile e Tablet.
#Le differenze tra regioni sono molto contenute, quindi si potrebbe focalizzare l'analisi causale principalmente sul tipo di dispositivo.

ggplot(dati, aes(x = income, y = clicks)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red")

#IMPLICAZIONI POSSIBILI
#Gli utenti con redditi piÃ¹ bassi sono piÃ¹ reattivi alle campagne forse perchÃ©:
#sono piÃ¹ sensibili alle promozioni, cercano piÃ¹ attivamente offerte, navigano in contesti piÃ¹ mirati all'acquisto.
#Gli utenti con redditi elevati cliccano meno, forse perchÃ©:
#ricevono meno esposizione, sono meno interessati a campagne generaliste, acquistano direttamente senza cliccare annunci.

#### Implicazione per la strategia in base ai dati iniziali (IPOTESI)####
#Non tutte le campagne sono efficaci: Ã¨ necessario valutare singolarmente le campagne per capire quali funzionano.
#Le combinazioni Cam1+3 sembrano sinergiche, potrebbero essere riutilizzate o estese.
#serve una valutazione causale per confermare l'efficacia delle campagne rispetto al gruppo di controllo.
#Gli utenti giovani e con reddito piÃ¹ basso sono più reattivi vanno maggiormente targettizzati nelle future campagne.
#Ottimizzare tutte le campagne per Mobile e Tablet.
#Il tipo di dispositivo Ã¨ un driver piÃ¹ forte del comportamento rispetto alla regione.
#Il Desktop Ã¨ il segmento meno reattivo: le campagne dovrebbero essere diversamente progettate o ridimensionate per questi utenti.
#Potresti raccomandare al team marketing di differenziare le campagne in base al reddito, puntando a strategie piÃ¹ aggressive o personalizzate per il segmento a basso reddito (es. coupon, bundle, offerte-lampo), e piÃ¹ raffinate o â€œdi valoreâ€ per gli utenti con reddito elevato.

#### Analisi iniziale approfondita su CAMPAGNA 2 ####
#Verificare e approfondire proprio questo caso, sia per confermare la delusione, sia per capire perchÃ© Ã¨ andata cosÃ¬.

ggplot(subset(dati, group %in% c("Cam2", "Cam1", "Cam3")), 
       aes(x = group, y = clicks)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal() +
  ggtitle("Distribuzione dei click per Campagne 1, 2 e 3")
#Ã¨ il piu basso

aggregate(clicks ~ group, data = dati, FUN = mean)


##Verificare effetto marginale di CAM2 su CAM1_2 CAM1_2_3 e CAM2_3
#Se consideriamo le campagne singolarmente, Cam1 genera in media 45 click, mentre Cam2 ne genera 40.
#Tuttavia, la combinazione Cam1_2 produce solo 33 click medi, quindi peggio di entrambe le campagne prese da sole.

## Interpretazione strategica ##
#Questo suggerisce che le campagne Cam1 e Cam2 potrebbero non essere compatibili quando combinate:
#probabilmente hanno target diversi, tono comunicativo incoerente o contenuti ridondanti che confondono utente

#eccezioni positive#
#Al contrario, la combinazione Cam1_3 genera 60 click medi (piÃ¹ di entrambe le campagne da sole), e Cam1_2_3 arriva addirittura a 89.
#Questo suggerisce che Cam1 e Cam3 lavorano bene insieme, forse perchÃ© complementari nel tono o nel contenuto.


#conclusione Non tutte le combinazioni di campagne portano a un effetto sinergico.
#Alcune, come Cam1 + Cam2, sembrano annullarsi a vicenda o raggiungere utenti meno reattivi.
#Altre, come Cam1 + Cam3, mostrano un chiaro effetto potenziante.


#### Causalità tra clicks e campagna ####
# 0. Librerie necessarie
library(nnet)      # multinomial logistic regression
library(cobalt)    # balance diagnostics
library(dplyr)     # data manipulation
library(ggplot2)   # plotting

# 1. Carica e prepara i dati
dati$group <- as.factor(dati$group)

# 2. Modello multinomiale di Propensity Score
ps_model <- multinom(group ~ age + income + education_years + device_type + region + internet_usage_hours,
                     data = dati, trace = FALSE)

# 3. Calcolo delle probabilità predette (generalized propensity scores)
ps_pred <- predict(ps_model, type = "probs")

# 4. Rinomina le colonne con i livelli effettivi del trattamento
colnames(ps_pred) <- levels(dati$group)

# 5. Aggiungi le probabilità al dataframe
dati <- bind_cols(dati, as_tibble(ps_pred))

# 6. Calcolo dei pesi IPTW generico
library(dplyr)

dati <- dati %>%
  mutate(
    w = case_when(
      group == "None"     ~ 1 / None,
      group == "Cam1"     ~ 1 / Cam1,
      group == "Cam2"     ~ 1 / Cam2,
      group == "Cam3"     ~ 1 / Cam3,
      group == "Cam1_2"   ~ 1 / Cam1_2,
      group == "Cam1_3"   ~ 1 / Cam1_3,
      group == "Cam2_3"   ~ 1 / Cam2_3,
      group == "Cam1_2_3" ~ 1 / Cam1_2_3,
      TRUE                ~ NA_real_
    )
  )


# 7. Diagnostica del bilanciamento con love.plot
bal <- bal.tab(
  group ~ age + income + education_years + device_type + region + internet_usage_hours,
  data     = dati,
  weights  = dati$w,
  method   = "weighting",
  estimand = "ATE",
  un       = TRUE
)

# Love plot
love.plot(
  bal,
  stats     = "mean.diffs",
  threshold = 0.1,
  var.order = "unadjusted"
) + ggtitle("Covariate Balance: Unweighted vs. IPTW Weighted")

# 8. Stima effetti del trattamento (vs gruppo di riferimento: None)
dati$group <- relevel(dati$group, ref = "None")  # 'None' come gruppo di controllo

# Regressione naÃ¯ve (senza pesi)
naive_mod <- lm(clicks ~ group, data = dati)
naive_est <- coef(summary(naive_mod))
naive_results <- tibble(
  Treatment = rownames(naive_est)[-1],
  Estimate  = naive_est[-1, "Estimate"],
  SE        = naive_est[-1, "Std. Error"],
  Method    = "NaÃ¯ve"
)

# Regressione pesata IPTW
iptw_mod <- lm(clicks ~ group, data = dati, weights = w)
iptw_est <- coef(summary(iptw_mod))
iptw_results <- tibble(
  Treatment = rownames(iptw_est)[-1],
  Estimate  = iptw_est[-1, "Estimate"],
  SE        = iptw_est[-1, "Std. Error"],
  Method    = "IPTW"
)

# 9. Combina risultati
results <- bind_rows(naive_results, iptw_results)
results

# 10. Grafico degli effetti stimati
ggplot(results, aes(x = Treatment, y = Estimate, color = Method)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = Estimate - 1.96*SE,
                    ymax = Estimate + 1.96*SE),
                width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Estimated Effect of Each Campaign vs None",
       y = "Effect on Clicks (compared to None)",
       x = "Campaign")


#### APPLICHIAMO SOM ####
# Selezione variabili numeriche per clustering
library(dplyr)
library(kohonen)

som_vars <- dati %>%
  select(age, income, internet_usage_hours, education_years) %>%
  scale()  # Normalizzazione standard

set.seed(123)
som_grid <- somgrid(xdim = 4, ydim = 4, topo = "hexagonal")
som_model <- som(as.matrix(som_vars), grid = som_grid, rlen = 100)

plot(som_model, type = "codes", main = "Profili dei cluster")
dati$cluster_som <- som_model$unit.classif

# Subset clienti esposti alla Campagna 2
cam2 <- subset(dati, group == "Cam2")

# Confronta media dei clicks per cluster
aggregate(clicks ~ cluster_som, data = cam2, FUN = mean)

library(ggplot2)

# Supponiamo di avere un dataframe come questo
clicks_som <- data.frame(
  cluster_som = factor(1:16),
  clicks = c(69.96, 59.90320, 65.80525, 70.62952, 73.36456, 71.18,
             44.08, 49.00, 48.47538, 42.46646, 16.96930, 56.02368,
             20.61129, 15.82915, 26.70217, 18.00667)
)

# Crea il grafico
ggplot(clicks_som, aes(x = cluster_som, y = clicks)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(clicks, 1)), vjust = -0.5, size = 3) +
  labs(title = "Click medi per cluster SOM (Campagna 2)",
       x = "Cluster SOM",
       y = "Click medi") +
  theme_minimal()

#vediamo come sono fatti i cluster
aggregate(cbind(age, income, internet_usage_hours) ~ cluster_som, data = cam2, FUN = mean)




#PSM per ogni cluster
library(dplyr)

# 1) Definisco i tre gruppi di cluster
cluster_groups <- list(
  Top    = 1:6,
  Medio  = 7:10,
  Basso  = 11:16
)

# 2) Loop sui tre gruppi, applico il PSM function e salvo i risultati
results_list <- lapply(names(cluster_groups), function(name) {
  subset_data <- dati %>%
    filter(cluster_som %in% cluster_groups[[name]])
  
  est <- estimate_treatment_effect(
    data             = subset_data,
    treatment_var    = "treated2",
    outcome_var      = "clicks",
    covariate_formula = cov_formula
  )
  
  # Aggiungo una colonna per identificare il gruppo
  est$ClusterGroup <- name
  return(est)
})

# 3) Unisco tutto in un unico tibble
library(purrr)
final_results <- bind_rows(results_list)

# Stampo per controllo
print(final_results)

