################################################################################################
## DOMCURA – Aktuarielle Analytik & Produktmanagement (nur freclaimset3multi9207)
## Autor: Serges Letsudem Wonanke
## Ziel:  Analyse aktuarieller Aufgaben mit offenen Beispieldaten (CASdatasets)
################################################################################################

# 0) Pakete laden ------------------------------------------------------------------------------
libs <- c("dplyr","tidyr","ggplot2","lubridate","broom","knitr","scales","stringr","patchwork",
          "tweedie","statmod","kableExtra","CASdatasets", "glmnet")
invisible(lapply(libs, function(x){
  if(!require(x, character.only=TRUE)) install.packages(x)
  library(x, character.only=TRUE)
}))

################################################################################################
################ 1) CASdatasets laden - künstliche Komposit Portfolio ##########################
################################################################################################
# data(freclaimset3fire9207)                                                              ######
# data(freclaimset3dam9207)                                                               ######
# data(freclaimset3multi9207)                                                             ######
# data(fretri1TPL9207)                                                                    ######
#                                                                                         ######
# Brand_Explosion_Blitzschlag            <- freclaimset3fire9207                          ######
# Sturm_Hagel_Wasser                     <- freclaimset3dam9207                           ######
# Feuer_Wasser_Sturm_Diebstahl_Glasbruch <- freclaimset3multi9207                         ######
# Kfz_Haftpflicht                        <- fretri1TPL9207                                ######
# str(Brand_Explosion_Blitzschlag)                                                        ######
# str(Sturm_Hagel_Wasser)                                                                 ######
# str(Feuer_Wasser_Sturm_Diebstahl_Glasbruch)                                             ######
# str(Kfz_Haftpflicht)                                                                    ######
################################################################################################
################################################################################################
################################################################################################

################################################################################################
## 1) CASdatasets laden - Multi-Risk-Komposit
################################################################################################
data(freclaimset3multi9207)
df_multi <- freclaimset3multi9207

# Kurzer Struktur-Check
str(df_multi[, c(grep("_Claim$", names(df_multi), value=TRUE),
                 "Occur","Damage_Revenue","Damage_Sites")])

################################################################################################
## 2) Explorative Analyse – Gefahrenarten-Übersicht & Zeitverlauf
################################################################################################

# 2a) Boxplot aller *_Claim-Spalten (log-Skala)
par(mar = c(8, 4, 1, 2))  # Platz für Labels & Caption
boxplot(
  df_multi[, grep("_Claim$", colnames(df_multi))],
  log = "y",
  las = 3,                             # wir zeichnen die Labels manuell (45°, kursiv)
  ylab = "Schadenshöhe (log-Skala)",
  cex.axis = 0.9,
  cex.lab = 1,
  mgp = c(2, 0.4, 0)                       # y-Label näher an die Ticks
)
grid()

# Caption unter dem Plot (noch etwas höher + kursiv)
mtext(
  side = 1,
  line = 6.8,   # war 5.3 → jetzt höher
  adj = 0,
  cex = 0.8,
  font = 3,     # kursiv (italic)
  text = paste(
    "Abbildung: Verteilung der Schadenssummen nach Garantiebereichen.",
    "HSS = Hagel, Sturm, Schnee,",
    "\nDamage = Sachschäden (z. B. Maschinen- oder Wasserschäden), TPL = Haftpflicht."
  )
)
# HSS = Hagel, Sturm, Schnee: Schäden aus Naturereignissen wie Hagel, Sturm und Schnee.
# TPL: Ansprüche aus Haftpflichtschäden gegenüber Dritten (sowohl Sach- als auch Personenschäden).
# In Other: Sonstige Deckungen: andere Ansprüche, z. B. Rechtsschutz, Betriebsunterbrechung.

# 2b) Zeitliche Verteilung HSS in Mio. €
par(mar = c(4, 4, 2, 1))
plot(
  df_multi$Occur,
  df_multi$HSS_Claim / 1e6,
  type = "h",
  xlab = "Schadeneintrittsdatum",
  ylab = "Schadenshöhe (in Mio. €)",
  main = "Zeitliche Verteilung der HSS-Schäden (Hagel, Sturm, Schnee)"
)
grid()

# 2c) Risk-Portfolios - Gefahrenarten-Total (Balken, absteigend)
peril_totals <- df_multi |>
  dplyr::select(ends_with("_Claim")) |>
  tidyr::pivot_longer(everything(), names_to = "Peril", values_to = "Paid") |>
  dplyr::group_by(Peril) |>
  dplyr::summarise(Total_Incurred = sum(Paid, na.rm = TRUE), .groups="drop")

ggplot(peril_totals, aes(x = reorder(Peril, -Total_Incurred), y = Total_Incurred/1e6)) +
  geom_col(alpha = 0.9) +
  coord_flip() +
  labs(title = "Aufteilung des gesamten Schadenaufwands („Total Incurred“) nach Gefahrenarten",
       y = "Schadenaufwand (Mio. €)", x = "Gefahrenarten") +
  theme_minimal(base_size = 13)

################################################################################################
## 3) Tarifentwicklung & Auskömmlichkeit (Tweedie GLM nur auf Multi-Daten)
################################################################################################

### 3a) Feature-Engineering (nur Multi)
set.seed(1234)

#### 3a-1) Schadenhöhe berechnen (aggregiert über alle *_Claim-Spalten)
df_multi <- df_multi %>%
  mutate(
    incurred = rowSums(select(., ends_with("_Claim")), na.rm = TRUE),
    baujahr_klasse = Damage_Revenue,   # Proxy für Größen-/Umsatzklasse
    region         = Damage_Sites,     # Proxy für Anzahl/Standorte
    earned_exposure = 1
  )

#### 3a-2) Explorative Prüfung der Schadenverteilung (log1p)

df_multi <- df_multi |>
  mutate(log_incurred = log1p(incurred))

p1 <- ggplot(df_multi, aes(x = incurred)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Verteilung der Schadenhöhe (incurred)",
       x = "incurred (€)", y = "Anzahl") +
  theme_minimal(base_size = 13)

p2 <- ggplot(df_multi, aes(x = log_incurred)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "darkorange", alpha = 0.7) +
  geom_density(color = "red", size = 1.2) +
  labs(title = "Nach Transformation: log1p(incurred)",
       x = "log(1 + incurred)", y = "Dichte") +
  theme_minimal(base_size = 13)

qq <- ggplot(df_multi, aes(sample = log_incurred)) +
  stat_qq(color = "gray20") +
  stat_qq_line(color = "red", linewidth = 1.2) +
  labs(title = "Q-Q-Plot für log1p(incurred)",
       x = "Theoretische Quantile", y = "Beobachtete Quantile") +
  theme_minimal(base_size = 13)

(p1 | p2) / qq

### 3b) Train/Test-Split (80/20 stratifiziert nach baujahr_klasse)

# baujahr_klasse: das ist ein Proxy für Größen- oder Umsatzklasse des Risikos (z. B. klein, mittel, groß).
train_idx <- df_multi |>
  group_by(baujahr_klasse) |>
  group_map(~ sample(seq_len(nrow(.x)), size = ceiling(0.8 * nrow(.x))), .keep = TRUE)

row_ids <- split(seq_len(nrow(df_multi)), df_multi$baujahr_klasse)
train_rows <- unlist(Map(function(idx_list, ids) ids[idx_list], train_idx, row_ids))
test_rows  <- setdiff(seq_len(nrow(df_multi)), train_rows)

df_tr <- df_multi[train_rows, ]
df_te <- df_multi[test_rows, ]

### 3c) Regularisierter Risikoscore (Elastic Net auf TRAIN, Prognose auf TEST)

####### Designmatrix für Trainingsdaten erstellen
X_tr <- model.matrix(
  ~ scale(as.numeric(Damage_Revenue)) + scale(as.numeric(Damage_Sites)),
  data = df_tr
)[, -1]
y_tr <- log1p(df_tr$incurred)

###### Designmatrix für Testdaten erstellen (gleiche Struktur)
X_te <- model.matrix(
  ~ scale(as.numeric(Damage_Revenue)) + scale(as.numeric(Damage_Sites)),
  data = df_te
)[, -1]
###### Cross-Validation für Regularisierung (Elastic Net)
cv_fit <- cv.glmnet(
  x = X_tr, y = y_tr,
  family = "gaussian",   # kontinuierlicher Score
  alpha  = 0.5,          # Elastic Net (0=Ridge, 1=Lasso)
  nfolds = 10,
  standardize = TRUE
)

lambda_star <- cv_fit$lambda.1se # Modell ist extrem konservativ, glmnet setzt alle Koeff ≈ 0.
lambda_star <- cv_fit$lambda.min
fit <- glmnet(X_tr, y_tr, family = "gaussian", alpha = 0.5, lambda = lambda_star)

###### Lineare Prädiktionen (Train & Test)
lin_tr <- as.numeric(predict(fit, newx = X_tr, s = lambda_star))
lin_te <- as.numeric(predict(fit, newx = X_te, s = lambda_star))

###### Standardisierung der Scores (Train-Skala)
mu_tr  <- mean(lin_tr); sd_tr <- sd(lin_tr)
lin_tr_std <- (lin_tr - mu_tr) / sd_tr
lin_te_std <- (lin_te - mu_tr) / sd_tr

###### Transformation zu multiplikativen Risikoscores
df_tr$risk_score <- exp(lin_tr_std)
df_te$risk_score <- exp(lin_te_std)

###### Ableitung der (heuristischen) Netto-Prämien
df_tr$earned_premium_net <- pmax(800 * df_tr$risk_score, 200)
df_te$earned_premium_net <- pmax(800 * df_te$risk_score, 200)


### 3d) Burning-Cost-Analyse (auf TEST zur Beurteilung)

burning_te <- df_te |>
  group_by(baujahr_klasse) |>
  summarise(
    exp  = sum(earned_exposure),
    ep   = sum(earned_premium_net),
    inc  = sum(incurred),
    pure = inc/exp,
    lr   = inc/ep,
    .groups = "drop"
  )
print(burning_te)


### 3e) Burning-Cost-Analyse (nur zur Orientierung)
burning <- df_multi |>
  group_by(baujahr_klasse) |>
  summarise(
    exp  = sum(earned_exposure),
    ep   = sum(earned_premium_net),
    inc  = sum(incurred),
    pure = inc/exp,
    lr   = inc/ep,
    .groups="drop"
  )
print(burning)

### 3e) Tweedie-GLM auf TRAIN, Prognose auf TEST

fml <- as.formula(
  "incurred ~ factor(baujahr_klasse) + factor(region) + earned_premium_net"
)

mdl_tr <- glm(
  fml, data = df_tr,
  family = tweedie(var.power = 1.5, link.power = 0),
  weights = earned_exposure
)
summary(mdl_tr)

###### Prognose auf Testdaten
pred_te <- predict(mdl_tr, newdata = df_te, type = "response")

### 3f) Visualisierung der modellierten Schadenhöhen (TEST)
pred_plot <- data.frame(fitted = pred_te)
ggplot(pred_plot, aes(x = fitted/1000)) +
  geom_histogram(bins = 40, alpha = 0.8, fill = "steelblue") +
  labs(title = "Verteilung der modellierten reinen Schadenhöhe (Testdaten)",
       x = "Modellierte reine Schadenhöhe (Tsd. €)", y = "Anzahl") +
  theme_minimal(base_size = 13)

### 3g) Indikations-Tarif & Handlungsempfehlung (auf TEST, out-of-sample)

safety   <- 1.05   # Sicherheitszuschlag
expense  <- 0.25   # Verwaltungskosten (Anteil)
margin   <- 0.05   # Zielmarge (Anteil)

# Prognose der reinen Schadenhöhe auf TEST-Daten
pred_te <- predict(mdl_tr, newdata = df_te, type = "response")

# Indikations-Tarif auf TEST berechnen
df_te_ind <- df_te %>%
  mutate(
    pred_pure = pmax(pred_te, 0),                     # reine erwartete Schadenhöhe (≥0)
    ind_pure  = pred_pure,                            # hier identisch; Platzhalter für evtl. Adjustments
    ind_net   = ind_pure * safety * (1 + expense + margin),  # indizierte Nettoprämie
    ratio_indicated = ind_net / earned_premium_net,   # Verhältnis indiziert vs. aktuell
    action = dplyr::case_when(
      ratio_indicated > 1.10 ~ "Tarif anheben (nicht auskömmlich)",
      ratio_indicated < 0.90 ~ "Tarif senken (überauskömmlich)",
      TRUE                   ~ "Tarif beibehalten (nahe Ziel)"
    )
  )

# Portfolio-Zusammenfassung (TEST)
tarif_summary_te <- df_te_ind %>%
  summarise(
    aktueller_Schnitt   = mean(earned_premium_net, na.rm = TRUE),
    indizierter_Schnitt = mean(ind_net,            na.rm = TRUE),
    delta               = indizierter_Schnitt - aktueller_Schnitt
  ) %>%
  mutate(
    Handlung = dplyr::case_when(
      delta >  50 ~ "Prämienerhöhung empfohlen",
      delta < -50 ~ "Prämienreduzierung möglich",
      TRUE        ~ "Tarif stabil halten"
    )
  )

print(tarif_summary_te)

# Tabelle: robust mit knitr::kable; kableExtra wenn verfügbar
if (requireNamespace("kableExtra", quietly = TRUE)) {
  knitr::kable(tarif_summary_te, digits = 2,
               caption = "Tarifindikation und Handlungsempfehlung (TEST)") %>%
    kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
} else {
  knitr::kable(tarif_summary_te, digits = 2,
               caption = "Tarifindikation und Handlungsempfehlung (TEST)")
}

# Scatter: Indiziert vs. aktuell (TEST)
# reproduzierbares Sampling optional:
set.seed(1)
df_te_ind_k <- df_te_ind %>%
  dplyr::mutate(
    earned_premium_net_eur = earned_premium_net,    # bleibt in €
    ind_net_tsd            = ind_net / 2000         # visuelle Anpassung
  )

ggplot(
  dplyr::slice_sample(df_te_ind_k, n = min(3000, nrow(df_te_ind_k))),
  aes(x = earned_premium_net_eur, y = ind_net_tsd)
) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_abline(
    intercept = 0, slope = 1/2000,                 # 1:1-Bezugslinie angepasst
    linetype = "dashed", color = "grey40"
  ) +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  labs(
    title = "Indizierte vs. aktuelle Nettoprämie (TEST)",
    x = "Aktuelle Nettoprämie (€)",
    y = "Indizierte Nettoprämie (in €/2000, ca. Tsd. €)"
  ) +
  theme_minimal(base_size = 13)

################################################################################################
## 4) Aktuarielles Controlling / Zeitreihen-KPIs
################################################################################################

# Monatliche Schaden- und Prämienentwicklung
df_multi_kpi <- df_te_ind %>%
  mutate(JahrMonat = floor_date(Occur, "month")) %>%
  group_by(JahrMonat) %>%
  summarise(
    Exposure = sum(earned_exposure),
    Premium  = sum(earned_premium_net, na.rm = TRUE),
    Incurred = sum(incurred, na.rm = TRUE),
    nClaims  = sum(incurred > 0),
    BurningCost = Incurred / Exposure,
    Frequency  = nClaims / Exposure,
    Severity   = Incurred / pmax(nClaims, 1),
    LossRatio  = Incurred / Premium,
    .groups = "drop"
  )

# Plot: Verlustquote (Loss Ratio) im Zeitverlauf
ggplot(df_multi_kpi, aes(x = JahrMonat, y = LossRatio)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", alpha = 0.7) +
  labs(title = "Zeitreihe der Loss Ratio (aktuarielles Controlling)",
       x = "Monat", y = "Loss Ratio (Schaden / Prämie)") +
  theme_minimal(base_size = 13)

# Plot: Schadenfrequenz und -höhe
ggplot(df_multi_kpi, aes(x = JahrMonat)) +
  geom_line(aes(y = Frequency * 100, color = "Schadenfrequenz (%)"), linewidth = 1) +
  geom_line(aes(y = Severity / 1000, color = "Durchschnittsschaden (Tsd. €)"), linewidth = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Sekundärachse")) +
  labs(title = "Schadenfrequenz und Durchschnittsschaden über Zeit",
       x = "Monat", y = "Häufigkeit / Betrag") +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = c("Schadenfrequenz (%)" = "steelblue",
                                "Durchschnittsschaden (Tsd. €)" = "orange"))
################################################################################################
## 5) Partner- und Portfolio-Analyse
################################################################################################

# Gruppierung nach Region (oder Partner, falls Variable vorhanden)
portfolio_region <- df_te_ind %>%
  group_by(region) %>%
  summarise(
    n = n(),
    Exposure = sum(earned_exposure),
    Premium  = sum(earned_premium_net, na.rm = TRUE),
    Incurred = sum(incurred, na.rm = TRUE),
    LossRatio = Incurred / Premium,
    BurningCost = Incurred / Exposure,
    AvgPremium = Premium / Exposure,
    AvgClaim = Incurred / pmax(n, 1),
    .groups = "drop"
  )

# Balkendiagramm nach Regionen
ggplot(portfolio_region, aes(x = reorder(as.factor(region), -LossRatio), y = LossRatio)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = scales::percent(LossRatio, accuracy = 0.1)),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Loss Ratio nach Region (Portfolio-Analyse)",
    x = "Region", y = "Loss Ratio"
  ) +
  theme_minimal(base_size = 13)

# KPI-Tabelle
knitr::kable(portfolio_region, digits = 2, caption = "Portfolio-Performance nach Region") %>%
  kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

################################################################################################
## 6) Ergebnis-Interpretation (nur Multi)
################################################################################################
# 1) delta > 0  -> nicht auskömmlich → Prämien erhöhen
# 2) delta < 0  -> überauskömmlich → Prämien senken
# 3) action-Spalte auf Einzelvertragsebene → operative Steuerung / Produktanpassung

################################################################################################
# Ende ------------------------------------------------------------------------------------------
################################################################################################
