################################################################################################
## DOMCURA – Aktuarielle Analytik & Produktmanagement
## Autor: Serges Letsudem Wonanke
## Ziel:  Analyse aktuarieller Aufgaben mit offenen Beispieldaten (CASdatasets)
################################################################################################

# 0) Pakete laden ------------------------------------------------------------------------------
#libs <- c("dplyr","tidyr","ggplot2","lubridate","broom",
#          "tweedie","statmod","ChainLadder","officer","flextable")
invisible(lapply(libs, function(x){
  if(!require(x, character.only=TRUE)) install.packages(x)
  library(x, character.only=TRUE)
}))

################################################################################################
###################### 1) CASdatasets laden (keine DOMCURA-Dateien) ############################
################################################################################################
if(!require(CASdatasets)) devtools::install_github("casact/CASdatasets")
library(CASdatasets)

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

################################################################################################
# 4) Aktuarielles Controlling / Zeitreihen-KPIs ------------------------------------------------
################################################################################################
base <- base %>%
  mutate(mon = sample(seq(as.Date("2020-01-01"), as.Date("2024-12-01"), by="month"), nrow(base), TRUE))
month_kpi <- base %>%
  group_by(mon) %>%
  summarise(ep=sum(earned_premium_net), inc=sum(incurred),
            lr=inc/ep, .groups="drop")

ggplot(month_kpi, aes(mon, lr)) +
  geom_line(color="#00a075") + 
  labs(title="Monatliche Schadenquote – Wohngebäude (simuliert)", x=NULL, y="Loss Ratio")

################################################################################################
# 5) Partner- und Portfolio-Analysen -----------------------------------------------------------
################################################################################################
partner_tbl <- base %>%
  mutate(partner=sample(paste("Partner",1:8), nrow(base), TRUE)) %>%
  group_by(partner) %>%
  summarise(policies=n(), ep=sum(earned_premium_net),
            inc=sum(incurred), lr=inc/ep,
            avg_sum=mean(sum_insured), .groups="drop") %>%
  arrange(desc(ep))
partner_tbl

################################################################################################
# 6) Reporting & Präsentationen (PowerPoint) ---------------------------------------------------
################################################################################################
doc <- read_pptx()
doc <- add_slide(doc, layout="Title and Content", master="Office Theme")
doc <- ph_with(doc, "Aktuarielle Analyse – Wohngebäude (Beispieldaten)", 
               location=ph_location_type("title"))
ft <- flextable(partner_tbl)
doc <- ph_with(doc, value=ft, location=ph_location_type("body"))
print(doc, target="DOMCURA_Report_OpenData.pptx")

################################################################################################
# 7) Ad-hoc-Analysen & Sensitivität ------------------------------------------------------------
################################################################################################
uplift <- 0.04; elasticity <- -0.9
df_quotes <- data.frame(
  quoted_premium = runif(3000, 400, 1200),
  bind_prob = runif(3000, 0.1, 0.5),
  expected_cost = runif(3000, 200, 800)
) %>%
  mutate(new_prem = quoted_premium*(1+uplift),
         new_bind = pmin(pmax(bind_prob*(1+elasticity*uplift), 0), 1))

EP_now  <- sum(df_quotes$quoted_premium * df_quotes$bind_prob)
EP_new  <- sum(df_quotes$new_prem * df_quotes$new_bind)
Cost_now<- sum(df_quotes$expected_cost * df_quotes$bind_prob)
Cost_new<- sum(df_quotes$expected_cost * df_quotes$new_bind)

delta <- list(
  dEP = EP_new-EP_now,
  dCost = Cost_new-Cost_now,
  dTechER = (EP_new-Cost_new) - (EP_now-Cost_now)
)
print(delta)

################################################################################################
# 8) Schadenreservierung (Claim Triangle – Kfz-Haftpflicht) ------------------------------------
################################################################################################
tri_tpl <- as.triangle(Kfz_Haftpflicht)
mack <- MackChainLadder(tri_tpl)
summary(mack)
plot(mack, main="Kfz-Haftpflicht – Reserveschätzung (Mack-Modell)")

################################################################################################
# Ende ------------------------------------------------------------------------------------------
################################################################################################

