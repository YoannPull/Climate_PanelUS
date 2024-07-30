library(data.table)
library(readxl)
library(forecast)
library(tseries)
library(ggplot2)
library(sandwich)
library(reshape2)
library(plm)
library(lmtest)

# Chemins des fichiers Excel
path_excel_1 <- "data/fichier1.xlsx"
path_excel_2 <- "data/fichier2.xlsx"

#----------------------------------------#
#          CHARGEMENT DES DONNÉES        #
#----------------------------------------#

# Lecture des données depuis les fichiers Excel
data_variables <- setDT(read_excel(path_excel_1, sheet = "Fama_CCAI_Weekly"))
data_sp500 <- setDT(read_excel(path_excel_1, sheet = "Data_SP500_Weekly"))
data_clim_index <- setDT(read_excel(path_excel_2, sheet = "Indices_Climatique"))
data_sharesinfo <- setDT(read_excel(path_excel_2, sheet = "Information_SP500"))

#----------------------------------------#
#         MODÈLE AR(p) POUR CCAI         #
#----------------------------------------#

# Série temporelle pour CCAI
ts_ccai <- ts(data_variables$ccai, frequency = 1)

# Sélection du lag optimal basé sur l'AIC
max_lag <- 20  
aic_values <- sapply(1:max_lag, function(p) AIC(arima(ts_ccai, order = c(p, 0, 0))))
optimal_lag <- which.min(aic_values)
cat("Lag optimal (AIC) pour un modèle AR :", optimal_lag, "\n")

# Tracé des valeurs AIC pour différents lags
plot(1:max_lag, aic_values, type = "b", col = "blue", xlab = "Number of Lags", ylab = "AIC Value", main = "AIC Values for Different AR Models")
points(optimal_lag, aic_values[optimal_lag], col = "red", pch = 19)
text(optimal_lag, aic_values[optimal_lag], labels = paste("Optimal Lag:", optimal_lag), pos = 3, col = "red")

# Modèle AR(7) et résidus
model <- arima(ts_ccai, order = c(7, 0, 0))
residuals <- residuals(model)
print(residuals)
plot(residuals, type = "l", col = "blue", xlab = "Time", ylab = "Residuals", main = "Residuals of AR(p) Model")

#----------------------------------------#
#        PRÉPARATION DU PANEL            #
#----------------------------------------#

# Nombre de tickets et de périodes
nbr_tickets = nrow(data_sharesinfo)
Tt = nrow(data_variables)

# Copie et modification des données pour le panel
X <- copy(data_variables[,.(date,rf_3_weekly,mkt_rf_3_weekly,smb_3_weekly,hml_3_weekly,
                            rmw_5_friday,cma_5_friday,ccai)])
X[, ccai_ar7_innovation := residuals]

# Réplication des données pour chaque ticket
X_panel <- do.call(rbind, replicate(nbr_tickets, X, simplify = FALSE))

# Ajout des informations sur les actions
X_panel <- cbind(X_panel, data_sp500[,.(ticker,r_stock,Sector,IndustryGroup,Industry,SubIndustry)])
X_panel$r_rf <- X_panel[,.(r_stock-rf_3_weekly)]
X_panel <- X_panel[,.(date,ticker,r_rf,mkt_rf_3_weekly,smb_3_weekly,hml_3_weekly,
                      rmw_5_friday,cma_5_friday,ccai,ccai_ar7_innovation,Sector,
                      IndustryGroup,Industry,SubIndustry)]

print(X_panel)

#----------------------------------------#
#     Regression sur le Panel Global     #
#----------------------------------------#

# Conversion en données de panel
pdata <- pdata.frame(X_panel, index = c("ticker", "date"))

# Modèle avec effets fixes individuels
model <- plm(r_rf ~ mkt_rf_3_weekly + smb_3_weekly + hml_3_weekly + 
               rmw_5_friday + cma_5_friday + ccai_ar7_innovation,
             data = pdata, model = "within")

# Résultats avec correction de Driscroll and Kraay
coeftest(model, vcov = function(x) vcovSCC(x, type = "HC1", maxlag = 7))
summary(model)

# Modèle GLM avec effets fixes
X_panel$ticker <- factor(X_panel$ticker)
model_glm <- glm(r_rf ~ mkt_rf_3_weekly + smb_3_weekly + hml_3_weekly + 
                   rmw_5_friday + cma_5_friday + ccai_ar7_innovation,
                 data = X_panel)
robust_se_glm <- vcovHC(model_glm, type = "HC1")
coeftest(model_glm, vcov = robust_se_glm)
summary(model_glm)

#----------------------------------------#
#            PANEL PAR SECTOR            #
#----------------------------------------#

unique_sector <- unique(data_sharesinfo$Sector)
results_by_sector <- list()
models_by_sector <- list()
predictor <- "ccai_ar7_innovation"

# Boucle pour chaque secteur
for (sector in unique_sector) {
  pdata_temp <- pdata.frame(X_panel[Sector == sector], index = c("ticker", "date"))
  
  model <- plm(r_rf ~ mkt_rf_3_weekly + smb_3_weekly + hml_3_weekly + 
                 rmw_5_friday + cma_5_friday + ccai_ar7_innovation,
               data = pdata_temp, model = "within")
  
  coef_test <- coeftest(model, vcov = function(x) vcovHC(x, type = "HC1", maxlag = 7))
  models_by_sector[[sector]] <- coef_test
  
  coefficients <- coef_test[predictor, "Estimate"]
  p_values <- coef_test[predictor, "Pr(>|t|)"]
  
  results_by_sector[[sector]] <- list(coefficients = coefficients, p_values = p_values)
}

# Résultats par secteur
results_df <- data.frame(
  Secteur = unique_sector,
  Valeur = predictor,
  Coefficient = sapply(results_by_sector, function(x) x$coefficients),
  Pvalue = sapply(results_by_sector, function(x) x$p_values)
)

# Tracé des résultats par secteur
ggplot(results_df, aes(x = Secteur, y = Coefficient, fill = Pvalue <= 0.1)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("grey70", "steelblue"), 
                    labels = c("P-value > 0.1", "P-value <= 0.1"),
                    name = "Significance") +
  labs(x = "Secteur", 
       y = "Coefficient estimé",
       title = "Coefficients des Modèles PLM par Secteur",
       subtitle = "Avec indication de la significativité (P-value < 0.1)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

### GLM ###

# Modèles GLM par secteur
results_by_sector <- list()
models_by_sector <- list()

for (sector in unique_sector) {
  data_temp <- X_panel[X_panel$Sector == sector, ]
  
  model <- glm(r_rf ~ mkt_rf_3_weekly + smb_3_weekly + hml_3_weekly + 
                 rmw_5_friday + cma_5_friday + ccai_ar7_innovation,
               data = data_temp)
  
  robust_se_glm <- vcovHC(model, type = "HC1")
  coef_test <- coeftest(model, vcov = robust_se_glm)
  
  if (predictor %in% rownames(coef_test)) {
    coefficients <- coef_test[rownames(coef_test) == predictor, "Estimate"]
    p_values <- coef_test[rownames(coef_test) == predictor, "Pr(>|z|)"]
  } else {
    coefficients <- NA
    p_values <- NA
  }
  
  results_by_sector[[sector]] <- list(coefficients = coefficients, p_values = p_values)
}

results_df <- data.frame(
  Secteur = unique_sector,
  Valeur = predictor,
  Coefficient = sapply(results_by_sector, function(x) x$coefficients),
  Pvalue = sapply(results_by_sector, function(x) x$p_values)
)

ggplot(results_df, aes(x = Secteur, y = Coefficient, fill = Pvalue <= 0.1)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("grey70", "steelblue"), 
                    labels = c("P-value > 0.1", "P-value <= 0.1"),
                    name = "Significance") +
  labs(x = "Secteur", 
       y = "Coefficient estimé",
       title = "Coefficients des Modèles GLM par Secteur",
       subtitle = "Avec indication de la significativité (P-value < 0.1)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

#----------------------------------------#
#      PANEL PAR INDUSTRYGROUP           #
#----------------------------------------#

unique_industrygroup <- unique(data_sharesinfo$IndustryGroup)
results_by_industry <- list()
models_by_industry <- list()

# Boucle pour chaque groupe d'industrie
for (industry in unique_industrygroup) {
  pdata_temp <- pdata.frame(X_panel[IndustryGroup == industry], index = c("ticker", "date"))
  
  model <- plm(r_rf ~ mkt_rf_3_weekly + smb_3_weekly + hml_3_weekly + 
                 rmw_5_friday + cma_5_friday + ccai_ar7_innovation,
               data = pdata_temp, model = "within")
  
  coef_test <- coeftest(model, vcov = function(x) vcovHC(x, type = "HC1", maxlag = 7))
  models_by_industry[[industry]] <- coef_test
  
  coefficients <- coef_test[predictor, "Estimate"]
  p_values <- coef_test[predictor, "Pr(>|t|)"]
  
  results_by_industry[[industry]] <- list(coefficients = coefficients, p_values = p_values)
}

results_df <- data.frame(
  Industrie = unique_industrygroup,
  Valeur = predictor,
  Coefficient = sapply(results_by_industry, function(x) x$coefficients),
  Pvalue = sapply(results_by_industry, function(x) x$p_values)
)

ggplot(results_df, aes(x = Industrie, y = Coefficient, fill = Pvalue <= 0.1)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("grey70", "steelblue"), 
                    labels = c("P-value > 0.1", "P-value <= 0.1"),
                    name = "Significance") +
  labs(x = "Secteur", 
       y = "Coefficient estimé",
       title = "Coefficients des Modèles PLM par Industrie",
       subtitle = "Avec indication de la significativité (P-value < 0.1)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

# Modèles GLM par groupe d'industrie
results_by_industry <- list()
models_by_industry <- list()

for (industry in unique_industrygroup) {
  data_temp <- X_panel[X_panel$IndustryGroup == industry, ]
  
  model <- glm(r_rf ~ mkt_rf_3_weekly + smb_3_weekly + hml_3_weekly + 
                 rmw_5_friday + cma_5_friday + ccai_ar7_innovation,
               data = data_temp)
  
  robust_se_glm <- vcovHC(model, type = "HC1")
  coef_test <- coeftest(model, vcov = robust_se_glm)
  
  if (predictor %in% rownames(coef_test)) {
    coefficients <- coef_test[rownames(coef_test) == predictor, "Estimate"]
    p_values <- coef_test[rownames(coef_test) == predictor, "Pr(>|z|)"]
  } else {
    coefficients <- NA
    p_values <- NA
  }
  
  results_by_industry[[industry]] <- list(coefficients = coefficients, p_values = p_values)
}

results_df <- data.frame(
  Industrie = unique_industrygroup,
  Valeur = predictor,
  Coefficient = sapply(results_by_industry, function(x) x$coefficients),
  Pvalue = sapply(results_by_industry, function(x) x$p_values)
)

ggplot(results_df, aes(x = Industrie, y = Coefficient, fill = Pvalue <= 0.1)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("grey70", "steelblue"), 
                    labels = c("P-value > 0.1", "P-value <= 0.1"),
                    name = "Significance") +
  labs(x = "Secteur", 
       y = "Coefficient estimé",
       title = "Coefficients des Modèles GLM par Industrie",
       subtitle = "Avec indication de la significativité (P-value < 0.1)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

#----------------------------------------#
#      ROBUSTESSE DES APPROCHES          #
#----------------------------------------#

### Variation des plages temporelles    ###

# Initialisation des listes pour stocker les résultats
results_by_sector_and_period <- list()
models_by_sector_and_period <- list()
predictor <- "ccai_ar7_innovation"  # Nom du prédicteur principal

# Définition des différentes plages temporelles de 5 ans
time_periods <- list(
  period1 = c("2008-03-07", "2013-03-06"),
  period2 = c("2013-03-07", "2018-03-06"),
  period3 = c("2018-03-07", "2023-03-06"),
  period4 = c("2023-03-07", "2024-03-01")
)

# Initialisation des vecteurs pour stocker les résultats
secteurs <- c()
periodes <- c()
coefficients <- c()
p_values <- c()
significativite <- c()

# Boucle pour chaque secteur et période
for (sector in unique_sector) {
  # Filtrer les données pour le secteur actuel
  pdata_temp <- pdata.frame(X_panel[X_panel$Sector == sector], index = c("ticker", "date"))
  pdata_temp$date <- as.Date(pdata_temp$date)
  
  for (period_name in names(time_periods)) {
    # Filtrer les données pour la période actuelle
    period <- time_periods[[period_name]]
    pdata_temp_period <- pdata_temp[pdata_temp$date >= as.Date(period[1]) & pdata_temp$date <= as.Date(period[2]), ]
    
    if (nrow(pdata_temp_period) > 0) {
      # Ajuster le modèle de régression pour la période actuelle
      model <- plm(r_rf ~ mkt_rf_3_weekly + smb_3_weekly + hml_3_weekly + 
                     rmw_5_friday + cma_5_friday + ccai_ar7_innovation,
                   data = pdata_temp_period, model = "within")
      
      # Tester les coefficients avec une covariance robuste
      coef_test <- coeftest(model, vcov = function(x) vcovHC(x, type = "HC1", maxlag = 7))
      
      # Sauvegarder le modèle et les résultats des tests de coefficients
      models_by_sector_and_period[[paste(sector, period_name, sep = "_")]] <- coef_test
      
      # Extraire le coefficient et la p-value pour le prédicteur d'innovation
      coefficient <- coef_test[predictor, "Estimate"]
      p_value <- coef_test[predictor, "Pr(>|t|)"]
      
      # Stocker les résultats dans les vecteurs
      secteurs <- c(secteurs, sector)
      periodes <- c(periodes, period_name)
      coefficients <- c(coefficients, coefficient)
      p_values <- c(p_values, p_value)
      significativite <- c(significativite, p_value <= 0.1)
    }
  }
}

# Créer un DataFrame pour résumer les résultats
results_df <- data.frame(
  Secteur = secteurs,
  Période = periodes,
  Coefficient = coefficients,
  Pvalue = p_values,
  Significatif = significativite
)

print(results_df)  # Afficher les résultats

# Préparation des labels pour les périodes dans le graphique
period_labels <- data.frame(
  Période = unique(results_df$Période),
  x = seq(2, length(unique(interaction(results_df$Secteur, results_df$Période))), by = length(unique(results_df$Secteur))),
  y = max(results_df$Coefficient) * 1.1
)

# Création du graphique pour visualiser les coefficients
ggplot(results_df, aes(x = interaction(Secteur, Période), y = Coefficient, fill = Significatif)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("grey70", "steelblue"), 
                    labels = c("P-value > 0.1", "P-value <= 0.1"),
                    name = "Significance") +
  labs(x = "Secteur et Période", 
       y = "Coefficient estimé",
       title = "Coefficients des Modèles PLM par Secteur et Période",
       subtitle = "Avec indication de la significativité (P-value < 0.1)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  geom_vline(xintercept = seq(1.5, length(unique(interaction(results_df$Secteur, results_df$Période))) - 0.5, by = length(unique(results_df$Secteur))), linetype = "dashed", color = "black") +
  geom_text(data = period_labels, aes(x = x, y = y, label = Période), angle = 0, hjust = 0.5, size = 5, inherit.aes = FALSE)


###      Comparaison des modèles FF à 3 et 5 facteurs       ###

# Initialisation des vecteurs pour stocker les résultats
secteurs <- c()
coefficients_3f <- c()
coefficients_5f <- c()
p_values_3f <- c()
p_values_5f <- c()
significativite_3f <- c()
significativite_5f <- c()

# Boucle pour chaque secteur
for (sector in unique_sector) {
  # Filtrer les données pour le secteur actuel
  pdata_temp <- pdata.frame(X_panel[X_panel$Sector == sector], index = c("ticker", "date"))
  
  # Ajuster les modèles à 3 et 5 facteurs
  model_3f <- plm(r_rf ~ mkt_rf_3_weekly + smb_3_weekly + hml_3_weekly + ccai_ar7_innovation,
                  data = pdata_temp, model = "within")
  
  model_5f <- plm(r_rf ~ mkt_rf_3_weekly + smb_3_weekly + hml_3_weekly + rmw_5_friday + cma_5_friday + ccai_ar7_innovation,
                  data = pdata_temp, model = "within")
  
  # Tester les coefficients avec une covariance robuste
  coef_test_3f <- coeftest(model_3f, vcov = function(x) vcovHC(x, type = "HC1", maxlag = 7))
  coef_test_5f <- coeftest(model_5f, vcov = function(x) vcovHC(x, type = "HC1", maxlag = 7))
  
  # Extraire les coefficients et les p-values pour le prédicteur d'innovation
  coefficient_3f <- coef_test_3f[predictor, "Estimate"]
  coefficient_5f <- coef_test_5f[predictor, "Estimate"]
  p_value_3f <- coef_test_3f[predictor, "Pr(>|t|)"]
  p_value_5f <- coef_test_5f[predictor, "Pr(>|t|)"]
  
  # Stocker les résultats dans les vecteurs
  secteurs <- c(secteurs, sector)
  coefficients_3f <- c(coefficients_3f, coefficient_3f)
  coefficients_5f <- c(coefficients_5f, coefficient_5f)
  p_values_3f <- c(p_values_3f, p_value_3f)
  p_values_5f <- c(p_values_5f, p_value_5f)
  significativite_3f <- c(significativite_3f, p_value_3f <= 0.1)
  significativite_5f <- c(significativite_5f, p_value_5f <= 0.1)
}

# Créer un DataFrame pour résumer les résultats de la comparaison
results_comparison_df <- data.frame(
  Secteur = secteurs,
  Coefficient_3F = coefficients_3f,
  Coefficient_5F = coefficients_5f,
  Pvalue_3F = p_values_3f,
  Pvalue_5F = p_values_5f,
  Significatif_3F = significativite_3f,
  Significatif_5F = significativite_5f
)

print(results_comparison_df)  # Afficher les résultats

# Transformation des données pour la visualisation
results_comparison_melted <- melt(results_comparison_df, id.vars = "Secteur",
                                  measure.vars = c("Coefficient_3F", "Coefficient_5F"),
                                  variable.name = "Modèle",
                                  value.name = "Coefficient")

results_comparison_pvalue_melted <- melt(results_comparison_df, id.vars = "Secteur",
                                         measure.vars = c("Pvalue_3F", "Pvalue_5F"),
                                         variable.name = "Modèle",
                                         value.name = "Pvalue")

results_comparison_melted$Pvalue <- results_comparison_pvalue_melted$Pvalue
results_comparison_melted$Significatif <- results_comparison_melted$Pvalue <= 0.1

# Création du graphique pour comparer les coefficients des deux modèles
ggplot(results_comparison_melted, aes(x = Secteur, y = Coefficient, fill = Significatif)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ Modèle, scales = "free_y") +
  scale_fill_manual(values = c("grey70", "steelblue"), 
                    labels = c("P-value > 0.1", "P-value <= 0.1"),
                    name = "Significance") +
  labs(x = "Secteur", 
       y = "Coefficient estimé",
       title = "Comparaison des Coefficients des Modèles PLM à 3 Facteurs et 5 Facteurs par Secteur",
       subtitle = "Avec indication de la significativité (P-value < 0.1)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )
