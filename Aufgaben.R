################################################################################################
## DOMCURA – Aktuarielle Analytik & Produktmanagement
## Autor: Serges Letsudem Wonanke
## Ziel:  Analyse aktuarieller Aufgaben mit offenen Beispieldaten (CASdatasets)
################################################################################################

# 0) Pakete laden ------------------------------------------------------------------------------
libs <- c("dplyr","tidyr","ggplot2","lubridate","broom","knitr","scales","stringr",
          "tweedie","statmod","ChainLadder","officer","kableExtra","CASdatasets")
invisible(lapply(libs, function(x){
  if(!require(x, character.only=TRUE)) install.packages(x)
  library(x, character.only=TRUE)
}))
#"flextable"
################################################################################################
################ 1) CASdatasets laden - künstliche Komposit Portfolio ##########################
################################################################################################

data(freclaimset3fire9207)
data(freclaimset3dam9207)
data(freclaimset3multi9207)
data(fretri1TPL9207)

Brand_Explosion_Blitzschlag            <- freclaimset3fire9207
Sturm_Hagel_Wasser                     <- freclaimset3dam9207
Feuer_Wasser_Sturm_Diebstahl_Glasbruch <- freclaimset3multi9207
Kfz_Haftpflicht                        <- fretri1TPL9207
str(Brand_Explosion_Blitzschlag)
str(Sturm_Hagel_Wasser)
str(Feuer_Wasser_Sturm_Diebstahl_Glasbruch)
str(Kfz_Haftpflicht)

################################################################################################
######################## 2) Explorative Analyse – Spartenvergleich #############################
################################################################################################

# Summenbildung für jede Sparte

#  Brand / Explosion / Blitzschlag
EDA_fire <- Brand_Explosion_Blitzschlag %>%
  mutate(total_paid = rowSums(select(., starts_with("paid_Y")), na.rm = TRUE)) %>%
  summarise(
    Sparte = "Feuer / Explosion / Blitzschlag",
    n_claims = n(),
    total_paid = sum(total_paid, na.rm = TRUE),
    avg_paid = mean(total_paid, na.rm = TRUE)
  )

#  Sturm / Hagel / Wasser
EDA_sturm <- Sturm_Hagel_Wasser %>%
  mutate(total_paid = rowSums(select(., starts_with("paid_Y")), na.rm = TRUE)) %>%
  summarise(
    Sparte = "Sturm / Hagel / Wasser",
    n_claims = n(),
    total_paid = sum(total_paid, na.rm = TRUE),
    avg_paid = mean(total_paid, na.rm = TRUE)
  )

#  Verbundene Wohngebäudeversicherung (Multi-Risk)
EDA_multi <- Feuer_Wasser_Sturm_Diebstahl_Glasbruch %>%
  mutate(total_paid = rowSums(select(., ends_with("_Claim")), na.rm = TRUE)) %>%
  summarise(
    Sparte = "Feuer + Wasser + Sturm + Diebstahl + Glasbruch",
    n_claims = n(),
    total_paid = sum(total_paid, na.rm = TRUE),
    avg_paid = mean(total_paid, na.rm = TRUE)
  )

# Zusammenführen
EDA_tbl <- bind_rows(EDA_fire, EDA_sturm, EDA_multi)

print(EDA_tbl)

# Visualisierung (optional)
ggplot(EDA_tbl, aes(x = Sparte, y = avg_paid/1000, fill = Sparte)) +
  geom_col(alpha = 0.7) +
  labs(title = "Durchschnittlicher Schaden je Sparte",
       y = "Durchschnittlicher Schaden (Tsd. €)", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

################################################################################################
## 3) Tarifentwicklung & Auskömmlichkeit (deterministisch abgeleitete Prämien + Tweedie GLM)
################################################################################################

# Hilfsfunktion zur Prämienableitung aus Risikofaktoren -------------------------
derive_premium <- function(df, base_prem = 800) {
  df <- df %>%
    mutate(
      risk_score =
        1 +
        0.2 * (as.numeric(RiskCateg) - median(as.numeric(RiskCateg), na.rm = TRUE)) +
        0.1 * (as.numeric(NbEmployee) - 3) / 3 +
        0.05 * (as.numeric(Surface) - 3) / 3,
      earned_premium_net = pmax(base_prem * risk_score, 200)
    )
  df
}

# Feuer: Summe über paid_Y*-Spalten
Brand_Explosion_Blitzschlag <- Brand_Explosion_Blitzschlag %>%
  mutate(incurred = rowSums(select(., starts_with("paid_Y")), na.rm = TRUE),
         baujahr_klasse = RiskCateg,
         region = NbSite,
         earned_exposure = 1) %>%
  derive_premium()

# Sturm: Summe über paid_Y*-Spalten
Sturm_Hagel_Wasser <- Sturm_Hagel_Wasser %>%
  mutate(incurred = rowSums(select(., starts_with("paid_Y")), na.rm = TRUE),
         baujahr_klasse = RiskCateg,
         region = NbSite,
         earned_exposure = 1) %>%
  derive_premium()

# Multi: Summe über alle *_Claim-Spalten
Feuer_Wasser_Sturm_Diebstahl_Glasbruch <- Feuer_Wasser_Sturm_Diebstahl_Glasbruch %>%
  mutate(incurred = rowSums(select(., ends_with("_Claim")), na.rm = TRUE),
         baujahr_klasse = Damage_Revenue,
         region = Damage_Sites,
         earned_exposure = 1) %>%
  mutate(
    risk_score =
      1 +
      0.3 * (as.numeric(Damage_Revenue) - 2) / 2 +
      0.2 * (as.numeric(Damage_Sites) - 2) / 2
  ) %>%
  mutate(earned_premium_net = pmax(800 * risk_score, 200))

# Zusammenführen aller Sparten
base <- bind_rows(
  mutate(Brand_Explosion_Blitzschlag, Sparte = "Feuer"),
  mutate(Sturm_Hagel_Wasser, Sparte = "Sturm"),
  mutate(Feuer_Wasser_Sturm_Diebstahl_Glasbruch, Sparte = "Multi")
)

# Berechnung der Schaden- und Prämienkennzahlen
base <- base %>%
  mutate(
    pure_premium = incurred / earned_exposure,
    loss_ratio   = incurred / earned_premium_net
  )

###################################### Burning-Cost-Analyse ###########################
burning <- base %>%
  group_by(Sparte, baujahr_klasse) %>%
  summarise(exp = sum(earned_exposure),
            ep  = sum(earned_premium_net),
            inc = sum(incurred),
            pure = inc/exp,
            lr   = inc/ep, .groups = "drop")
print(burning)

###################### Tweedie-GLM: modellierte reine Schadenhöhe #####################

fml <- as.formula(
  "pure_premium ~ factor(Sparte) + factor(baujahr_klasse) +
                   factor(region) + earned_premium_net"
)

mdl <- glm(fml, data = base,
           family = tweedie(var.power = 1.5, link.power = 0),
           weights = earned_exposure)

summary(mdl)

###################### Visualisierung der modellierten Schadenhöhe ####################
pred <- data.frame(
  Sparte = base$Sparte,
  fitted = fitted(mdl)
)
ggplot(pred, aes(Sparte, fitted/1000, fill = Sparte)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Modellierte Schadenhöhe (Tsd. €)",
       y = "Ø modellierte Schadenhöhe (Tsd. €)", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

## Erweiterter Block 3 — Tarifindikation & Handlungsempfehlung
################################################################################################
## 3b) Indikations-Tarif & Handlungsempfehlung
################################################################################################

# Sicherheits- und Kostenparameter
safety   <- 1.05   # Sicherheitszuschlag
expense  <- 0.25   # Verwaltungskosten
margin   <- 0.05   # Zielmarge

# Vorherige Modellvorhersage (Pure Premium)
base$pred_pure <- pmax(fitted(mdl), 0)

# Indizierte Nettoprämie
base <- base %>%
  mutate(
    ind_pure = pred_pure,                                     # modellierte reine Prämie
    ind_net  = ind_pure * safety * (1 + expense + margin),     # Nettoprämie mit Zuschlägen
    ratio_indicated = ind_net / earned_premium_net,            # Verhältnis Modell / aktuelle Prämie
    action = case_when(
      ratio_indicated > 1.1 ~ "Tarif anheben (nicht auskömmlich)",
      ratio_indicated < 0.9 ~ "Tarif senken (überauskömmlich)",
      TRUE ~ "Tarif beibehalten (nahe Ziel)"
    )
  )

# Übersicht nach Sparten
tarif_summary <- base %>%
  group_by(Sparte) %>%
  summarise(
    aktueller_Schnitt = mean(earned_premium_net, na.rm=TRUE),
    indizierter_Schnitt = mean(ind_net, na.rm=TRUE),
    delta = indizierter_Schnitt - aktueller_Schnitt,
    Handlung = case_when(
      delta > 50 ~ "Prämienerhöhung empfohlen",
      delta < -50 ~ "Prämienreduzierung möglich",
      TRUE ~ "Tarif stabil halten"
    ),
    .groups="drop"
  )

print(tarif_summary)

# Visualisierung: Indikation vs. aktuelle Prämie
ggplot(base %>% sample_n(3000),
       aes(x = earned_premium_net, y = ind_net, color = Sparte)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey40") +
  labs(
    title = "Indizierte vs. aktuelle Nettoprämie nach Sparte",
    x = "Aktuelle Nettoprämie (€)",
    y = "Indizierte Nettoprämie (€)",
    color = "Sparte"
  ) +
  theme_minimal()

# Export Handlungsempfehlungen

kbl(tarif_summary, digits = 2, caption = "Tarifindikation und Handlungsempfehlungen") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

################################################################################################
## Ergebnis-Interpretation
################################################################################################
# 1. Sparten mit positiver Delta (>0): nicht auskömmlich → Prämien erhöhen
# 2. Sparten mit negativer Delta (<0): überauskömmlich → Prämien senken
# 3. Aktionstabellen pro Vertrag (base$action) → operative Steuerung oder Produktanpassung

################################################################################################
# 4) Aktuarielles Controlling / Zeitreihen-KPIs (aus Originaldaten 2002–2007)
################################################################################################

# ------------------------------------------------------------------------------
# 1. Ausgangsdaten: Schadenjahre extrahieren
# Die Datensätze enthalten paid_Y92 ... paid_Y07 -> also Schadenjahre 1992–2007
# Wir wählen 2002–2007, um ein realistisches jüngeres Zeitfenster zu analysieren.
# ------------------------------------------------------------------------------
################################################################################################
# ZEITREIHEN 2002–2007: Feuer & Sturm  (Kfz separat als Reserving)
################################################################################################

# --- 1) paid_Yk -> Kalenderjahr (nur Feuer, Sturm) -------------------------------------------
paid_style_by_year <- function(df, spartenname, end_year = 2007, year_range = 2002:2007) {
  paid_cols <- grep("^paid_Y\\d+$", names(df), value = TRUE)
  if (length(paid_cols) == 0) {
    stop(paste0("Keine 'paid_Y<k>'-Spalten gefunden in ", spartenname, "."))
  }
  k_idx <- as.integer(stringr::str_match(paid_cols, "^paid_Y(\\d+)$")[,2])
  k_max <- max(k_idx, na.rm = TRUE)
  start_year <- end_year - (k_max - 1)
  
  df %>%
    mutate(.id = dplyr::row_number()) %>%
    dplyr::select(.id, dplyr::all_of(paid_cols)) %>%
    tidyr::pivot_longer(dplyr::all_of(paid_cols), names_to = "paid_col", values_to = "Paid") %>%
    dplyr::mutate(
      k    = as.integer(stringr::str_match(paid_col, "^paid_Y(\\d+)$")[,2]),
      Jahr = start_year + (k - 1)
    ) %>%
    dplyr::filter(Jahr %in% year_range) %>%
    dplyr::group_by(Jahr) %>%
    dplyr::summarise(
      Incurred = sum(Paid, na.rm = TRUE),
      N_claims = dplyr::n(),                # simple frequency proxy
      .groups  = "drop"
    ) %>%
    dplyr::mutate(Sparte = spartenname) %>%
    dplyr::select(Jahr, Sparte, Incurred, N_claims)
}

# --- 2) Multi-Risk: Peril-Gesamt (ohne Zeit) + optional Imputation ----------------------------
multi_peril_totals <- function(df, spartenname = "Multi-Risk") {
  claim_cols <- grep("_Claim$", names(df), value = TRUE)
  if (length(claim_cols) == 0) stop("Keine *_Claim-Spalten im Multi-Risk-Datensatz gefunden.")
  df %>%
    dplyr::select(dplyr::all_of(claim_cols)) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "Peril", values_to = "Paid") %>%
    dplyr::group_by(Peril) %>%
    dplyr::summarise(Total_Incurred = sum(Paid, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(Sparte = spartenname)
}

multi_impute_by_year <- function(df_multi, by_year_other, year_range = 2002:2007,
                                 spartenname = "Multi-Risk (imputiert)") {
  claim_cols <- grep("_Claim$", names(df_multi), value = TRUE)
  stopifnot(length(claim_cols) > 0)
  total_multi <- df_multi %>%
    dplyr::summarise(total = sum(dplyr::across(dplyr::all_of(claim_cols)), na.rm = TRUE)) %>%
    dplyr::pull(total)
  
  w_tbl <- by_year_other %>%
    dplyr::group_by(Jahr) %>%
    dplyr::summarise(inc = sum(Incurred, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(Jahr %in% year_range)
  
  if (sum(w_tbl$inc, na.rm = TRUE) <= 0) stop("Imputation nicht möglich: Summe Incurred der anderen Sparten ist 0.")
  
  w_tbl %>%
    dplyr::mutate(
      weight   = inc / sum(inc, na.rm = TRUE),
      Incurred = total_multi * weight,
      N_claims = NA_integer_,
      Sparte   = spartenname
    ) %>%
    dplyr::select(Jahr, Sparte, Incurred, N_claims)
}

# === Anwendung: Feuer & Sturm auf 2002–2007 ===============================================
ts_fire <- paid_style_by_year(Brand_Explosion_Blitzschlag, "Feuer / Explosion / Blitzschlag")
ts_dam  <- paid_style_by_year(Sturm_Hagel_Wasser,          "Sturm / Hagel / Wasser")

ts_other_no_kfz <- dplyr::bind_rows(ts_fire, ts_dam)

# Multi-Risk: Peril-Übersicht (ohne Zeit) + optional Imputation auf 2002–2007
multi_peril <- multi_peril_totals(Feuer_Wasser_Sturm_Diebstahl_Glasbruch)
ts_multi_imp <- multi_impute_by_year(Feuer_Wasser_Sturm_Diebstahl_Glasbruch, ts_other_no_kfz)

# Gemeinsame Zeitreihe (ohne Kfz, Multi imputiert)
ts_all_no_kfz <- dplyr::bind_rows(ts_other_no_kfz, ts_multi_imp) %>%
  dplyr::group_by(Jahr, Sparte) %>%
  dplyr::summarise(Total_Incurred = sum(Incurred, na.rm = TRUE),
                   N_claims = suppressWarnings(sum(N_claims, na.rm = TRUE)),
                   .groups = "drop")

# PLOT: Zeitreihe 2002–2007 für Feuer, Sturm (+ Multi imputiert)
ggplot(ts_all_no_kfz, aes(Jahr, Total_Incurred/1e6, color = Sparte)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  scale_x_continuous(breaks = 2002:2007, limits = c(2002, 2007)) +
  labs(title = "Schadenaufwand je Sparte (2002–2007)",
       subtitle = "Multi-Risk zeitlich imputiert proportional zu Feuer & Sturm (Kfz separat als Reserving)",
       y = "Incurred (Mio. €)", x = "Schadenjahr") +
  theme_minimal(base_size = 13)

ggplot(multi_peril, aes(x = reorder(Peril, -Total_Incurred), y = Total_Incurred/1e6)) +
  geom_col(alpha = 0.9) +
  coord_flip() +
  labs(title = "Multi-Risk: Peril-Aufschlüsselung (Gesamt)",
       y = "Incurred (Mio. €)", x = "Peril") +
  theme_minimal(base_size = 13)

################################################################################################
# KfZ-HAFTPFLICHT: RESERVING-CONTROLLING (Mack-Chain-Ladder)
################################################################################################

# Ziel: IBNR, Ultimate, Mack S.E., CV je Unfalljahr; Fokus 2002–2007

# Hilfsfunktionen ---------------------------------------------------------------
is_cumulative_triangle <- function(tri) {
  # TRUE, wenn jede Zeile entlang der Entwicklungsachsen nicht fallend ist
  all(apply(as.matrix(tri), 1, function(r) {
    x <- stats::na.omit(as.numeric(r))
    if (length(x) <= 1) TRUE else all(diff(x) >= 0)
  }))
}

latest_cumulative_by_row <- function(tri) {
  apply(as.matrix(tri), 1, function(r) {
    x <- stats::na.omit(as.numeric(r))
    if (length(x) == 0) NA_real_ else tail(x, 1)
  })
}

# 1) Triangle bereitstellen -----------------------------------------------------
kfz_tri <- tryCatch({
  if (inherits(Kfz_Haftpflicht, "triangle")) {
    Kfz_Haftpflicht
  } else {
    ChainLadder::as.triangle(Kfz_Haftpflicht)
  }
}, error = function(e) NULL)

if (is.null(kfz_tri)) {
  stop(paste0(
    "Kfz_Haftpflicht konnte nicht in ein Triangle umgewandelt werden.\n",
    "Vorhandene Spalten (Auszug): ", paste(head(names(Kfz_Haftpflicht), 30), collapse = ", "), " ..."
  ))
}

# 2) Falls inkremental, auf kumulativ umstellen --------------------------------
if (!is_cumulative_triangle(kfz_tri)) {
  kfz_tri <- ChainLadder::incr2cum(kfz_tri)
}

# 3) Mack-Chain-Ladder laufen lassen ------------------------------------------
mcl <- ChainLadder::MackChainLadder(kfz_tri, est.sigma = "Mack")

completed <- mcl$FullTriangle                 # vervollständigt kumulativ
latest    <- latest_cumulative_by_row(kfz_tri)

AY        <- suppressWarnings(as.numeric(rownames(completed)))
Ultimate  <- as.numeric(completed[, ncol(completed)])
Latest    <- as.numeric(latest)
IBNR      <- Ultimate - Latest

# Mack-Standardfehler & CV je AY (falls verfügbar)
SE_by_AY <- tryCatch({
  se_mat <- mcl$Mack.S.E
  if (is.null(se_mat)) rep(NA_real_, length(AY)) else as.numeric(se_mat[, ncol(se_mat)])
}, error = function(e) rep(NA_real_, length(AY)))

CV_by_AY <- ifelse(!is.na(SE_by_AY) & Ultimate != 0, SE_by_AY / Ultimate, NA_real_)

kfz_res_control <- data.frame(
  AY = AY,
  Latest = Latest,
  Ultimate = Ultimate,
  IBNR = IBNR,
  Mack_SE = SE_by_AY,
  CV = CV_by_AY
)

# 4) Auf 2002–2007 filtern -----------------------------------------------------
kfz_res_2002_2007 <- kfz_res_control %>% dplyr::filter(AY >= 2002 & AY <= 2007)

# 5) Ausgabe & Plots -----------------------------------------------------------
print(kfz_res_2002_2007)

ggplot(kfz_res_2002_2007, aes(x = AY, y = IBNR/1e6)) +
  geom_col() +
  scale_x_continuous(breaks = 2002:2007) +
  labs(title = "Kfz-Haftpflicht: IBNR je Unfalljahr (Mack)",
       subtitle = "AY 2002–2007",
       x = "Unfalljahr (AY)", y = "IBNR (Mio. €)") +
  theme_minimal(base_size = 13)

ggplot(kfz_res_2002_2007, aes(x = AY)) +
  geom_line(aes(y = Ultimate/1e6), linewidth = 1.1) +
  geom_line(aes(y = Latest/1e6), linetype = "dashed") +
  scale_x_continuous(breaks = 2002:2007) +
  labs(title = "Kfz-Haftpflicht: Ultimate vs. Latest (Mack)",
       subtitle = "Vollentwicklung (durchgezogen) vs. aktuell beobachtet (gestrichelt)",
       x = "Unfalljahr (AY)", y = "Schaden kumulativ (Mio. €)") +
  theme_minimal(base_size = 13)

ggplot(kfz_res_2002_2007, aes(x = AY, y = CV)) +
  geom_point(size = 2) + geom_line() +
  scale_x_continuous(breaks = 2002:2007) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Kfz-Haftpflicht: Mack-Unsicherheit (CV) je Unfalljahr",
       x = "Unfalljahr (AY)", y = "CV (Mack S.E. / Ultimate)") +
  theme_minimal(base_size = 13)

################################################################################################
# 5) Partner- und Portfolio-Analysen -----------------------------------------------------------
################################################################################################

# realen Vertriebspartner (z. B. Vermittler-ID, Maklergruppe, Kooperation) vorhandeln.

################################################################################################
# 6) Reporting & Präsentationen (PowerPoint) ---------------------------------------------------
################################################################################################
# sehe datei Präsentation

################################################################################################
# 7) Ad-hoc-Analysen & Sensitivität ------------------------------------------------------------
################################################################################################

################################################################################################
# Ende ------------------------------------------------------------------------------------------
################################################################################################

