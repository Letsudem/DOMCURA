################################################################################################
## DOMCURA – Aktuarielle Analytik & Produktmanagement (nur freclaimset3multi9207)
## Autor: Serges Letsudem Wonanke
## Ziel:  Analyse aktuarieller Aufgaben mit offenen Beispieldaten (CASdatasets)
################################################################################################

# 0) Pakete laden ------------------------------------------------------------------------------
libs <- c("dplyr","tidyr","ggplot2","lubridate","broom","knitr","scales","stringr",
          "tweedie","statmod","kableExtra","CASdatasets")
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
################ 1) CASdatasets laden - Multi-Risk-Komposit ####################################
################################################################################################
data(freclaimset3multi9207)
df_multi <- freclaimset3multi9207

# Kurzer Struktur-Check
str(df_multi[, c(grep("_Claim$", names(df_multi), value=TRUE),
                 "Occur","Damage_Revenue","Damage_Sites")])

################################################################################################
################ 2) Explorative Analyse – Gefahrenarten-Übersicht & Zeitverlauf ################
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

# 3a) Feature-Engineering (nur Multi)
# Reine Schadenhöhe (incurred) = Summe aller *_Claim-Spalten
df_multi <- df_multi |>
  mutate(
    incurred = rowSums(dplyr::select(. , ends_with("_Claim")), na.rm = TRUE),
    # sinnvolle Proxy-Features aus Multi:
    baujahr_klasse = Damage_Revenue,   # Proxy für Größen-/Umsatzklasse
    region         = Damage_Sites,     # Proxy für Anzahl/Standorte
    earned_exposure = 1
  ) |>
  mutate(
    # risikoindizierter Score rein aus Multi-Features
    risk_score =
      1 +
      0.30 * (as.numeric(Damage_Revenue) - median(as.numeric(Damage_Revenue), na.rm=TRUE)) / 2 +
      0.20 * (as.numeric(Damage_Sites)   - median(as.numeric(Damage_Sites),   na.rm=TRUE)) / 2,
    earned_premium_net = pmax(800 * risk_score, 200)
  )

# 3b) Burning-Cost-Analyse (nur zur Orientierung)
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

# 3c) Tweedie-GLM: reine Schadenhöhe modellieren
# Hinweis: Link = log (link.power=0), p ≈ 1.5 als Start; optional Profilierung
fml <- as.formula(
  "incurred ~ factor(baujahr_klasse) + factor(region) + earned_premium_net"
)

mdl <- glm(
  fml, data = df_multi,
  family = tweedie(var.power = 1.5, link.power = 0),
  weights = earned_exposure
)

summary(mdl)

# 3d) Visualisierung modellierter Schaden
pred <- data.frame(fitted = fitted(mdl))
ggplot(pred, aes(x = fitted/1000)) +
  geom_histogram(bins = 40, alpha = 0.8) +
  labs(title = "Verteilung der modellierten reinen Schadenhöhe",
       x = "Modellierte reine Schadenhöhe (Tsd. €)", y = "Anzahl") +
  theme_minimal(base_size = 13)

################################################################################################
## 3e) Indikations-Tarif & Handlungsempfehlung
################################################################################################

safety   <- 1.05   # Sicherheitszuschlag
expense  <- 0.25   # Verwaltungskosten
margin   <- 0.05   # Zielmarge

df_multi <- df_multi |>
  mutate(
    pred_pure = pmax(fitted(mdl), 0),
    ind_pure  = pred_pure,
    ind_net   = ind_pure * safety * (1 + expense + margin),
    ratio_indicated = ind_net / earned_premium_net,
    action = case_when(
      ratio_indicated > 1.10 ~ "Tarif anheben (nicht auskömmlich)",
      ratio_indicated < 0.90 ~ "Tarif senken (überauskömmlich)",
      TRUE                   ~ "Tarif beibehalten (nahe Ziel)"
    )
  )

tarif_summary <- df_multi |>
  summarise(
    aktueller_Schnitt  = mean(earned_premium_net, na.rm=TRUE),
    indizierter_Schnitt= mean(ind_net, na.rm=TRUE),
    delta              = indizierter_Schnitt - aktueller_Schnitt
  ) |>
  mutate(
    Handlung = case_when(
      delta >  50 ~ "Prämienerhöhung empfohlen",
      delta < -50 ~ "Prämienreduzierung möglich",
      TRUE        ~ "Tarif stabil halten"
    )
  )

print(tarif_summary)

kbl(tarif_summary, digits = 2, caption = "Tarifindikation und Handlungsempfehlung (nur Multi)") |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

# Scatter: Indiziert vs. aktuell
set.seed(1)
ggplot(df_multi %>% dplyr::slice_sample(n = min(3000, n())) ,
       aes(x = earned_premium_net, y = ind_net)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey40") +
  labs(
    title = "Indizierte vs. aktuelle Nettoprämie (nur Multi)",
    x = "Aktuelle Nettoprämie (€)",
    y = "Indizierte Nettoprämie (€)"
  ) +
  theme_minimal()

################################################################################################
## 4) Aktuarielles Controlling / Zeitreihen-KPIs (nur Multi, via Occur → Jahr)
################################################################################################

ts_multi <- df_multi |>
  mutate(Jahr = lubridate::year(Occur)) |>
  group_by(Jahr) |>
  summarise(
    Total_Incurred = sum(incurred, na.rm=TRUE),
    N_claims       = n(),
    .groups="drop"
  )

ggplot(ts_multi, aes(Jahr, Total_Incurred/1e6)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  labs(title = "Schadenaufwand Multi (jährlich, aus Occur aggregiert)",
       y = "Incurred (Mio. €)", x = "Schadenjahr (Occur)") +
  theme_minimal(base_size = 13)

ggplot(ts_multi, aes(Jahr, N_claims)) +
  geom_col(alpha = 0.85) +
  labs(title = "Schadenanzahl Multi (jährlich, aus Occur aggregiert)",
       y = "Anzahl", x = "Schadenjahr (Occur)") +
  theme_minimal(base_size = 13)

################################################################################################
## 5) Ergebnis-Interpretation (nur Multi)
################################################################################################
# 1) delta > 0  -> nicht auskömmlich → Prämien erhöhen
# 2) delta < 0  -> überauskömmlich → Prämien senken
# 3) action-Spalte auf Einzelvertragsebene → operative Steuerung / Produktanpassung

################################################################################################
# Ende ------------------------------------------------------------------------------------------
################################################################################################
