## 0) Setup & Datenzugriff

install.packages(c("data.table","dplyr","tidyr","ggplot2","lubridate",
                   "DBI","RSQLite","broom","tweedie","statmod"))

library(data.table); library(dplyr); library(tidyr); library(ggplot2)
library(lubridate); library(DBI); library(broom); library(tweedie); library(statmod)

# Beispiel: CSV laden (falls du CSV nutzt)
policies         <- fread("C:/Users/serge/Desktop/Arbeit/2025/Projekten_DS_DA/DOMCURA/Daten/policies.csv")
exposures        <- fread("C:/Users/serge/Desktop/Arbeit/2025/Projekten_DS_DA/DOMCURA/Daten/exposures.csv")
claims           <- fread("C:/Users/serge/Desktop/Arbeit/2025/Projekten_DS_DA/DOMCURA/Daten/claims.csv")
tariff_factors   <- fread("C:/Users/serge/Desktop/Arbeit/2025/Projekten_DS_DA/DOMCURA/Daten/tariff_factors.csv")
market_benchmark <- fread("C:/Users/serge/Desktop/Arbeit/2025/Projekten_DS_DA/DOMCURA/Daten/market_benchmark.csv")

# Datumsfelder
exposures[, period := as.Date(period)]
claims[, `:=`(loss_date = as.Date(loss_date),
              report_date = as.Date(report_date),
              settlement_date = as.Date(settlement_date))]
policies[, `:=`(start_date = as.Date(start_date), end_date = as.Date(end_date))]

## 1) Neugeschäftstarife & Auskömmlichkeit (Burning Cost + GLM/Tweedie)

### 1.1 Burning Cost je Merkmal (Beispiel: Wohngebäude × Baujahr)
# Merge exposures + claims + tariff_factors
base <- exposures %>%
  left_join(policies, by = "policy_id") %>%
  left_join(claims %>% group_by(policy_id) %>% summarise(incurred = sum(paid + reserve, na.rm=TRUE)),
            by = "policy_id") %>%
  left_join(tariff_factors %>% filter(factor_name == "baujahr_klasse") %>%
              rename(baujahr_klasse = factor_level), by = "policy_id") %>%
  mutate(incurred = coalesce(incurred, 0))

agg <- base %>%
  filter(product == "Wohngebaeude") %>%
  group_by(baujahr_klasse) %>%
  summarise(exp    = sum(earned_exposure, na.rm=TRUE),
            ep_net = sum(earned_premium_net, na.rm=TRUE),
            incurred = sum(incurred, na.rm=TRUE),
            pure_premium = incurred/exp,
            loss_ratio   = incurred/ep_net) %>%
  arrange(desc(loss_ratio))
agg

### 1.2 GLM/Tweedie (Frequenz×Schwere kombiniert) + Indikation

# Policy-level Aggregation
pol_agg <- exposures %>%
  group_by(policy_id) %>%
  summarise(exp = sum(earned_exposure),
            ep  = sum(earned_premium_net))

clm_agg <- claims %>%
  group_by(policy_id) %>%
  summarise(claim_cost = sum(paid + reserve, na.rm=TRUE))

tf_wide <- tariff_factors %>%
  pivot_wider(names_from = factor_name, values_from = factor_level)

df <- policies %>%
  left_join(pol_agg,  by="policy_id") %>%
  left_join(clm_agg,  by="policy_id") %>%
  left_join(tf_wide,  by="policy_id") %>%
  mutate(exp = coalesce(exp, 0),
         claim_cost = coalesce(claim_cost, 0),
         y_pure = claim_cost / pmax(exp, 1e-6)) %>%
  filter(product == "Wohngebaeude")

# Formel: kategoriale Faktoren via factor()
fml <- as.formula(
  "y_pure ~ factor(plz_zone) + factor(baujahr_klasse) + factor(dachart) + factor(nutzung) +
             factor(gebaeudeart) + factor(sicherungen) + factor(hochwasserzone) +
             factor(roof_material) + factor(heizung) + sum_insured + deductible"
)

# Tweedie GLM mit log-Link und Offset=log(exp)
mdl <- glm(fml, data=df,
           family = tweedie(var.power=1.5, link.power=0),  # log-Link
           weights = exp)                                  # Alternative: offset(log(exp))

summary(mdl)

# Indizierte Prämie
pred_pure <- pmax(exp(predict(mdl, type="link")), 0)  # schon je Exposure (durch weights/offset)
safety <- 1.05; expense <- 0.25; margin <- 0.05
df$ind_pure <- pred_pure
df$ind_net  <- df$ind_pure * safety * (1 + expense + margin)

df %>% select(policy_id, y_pure, ind_pure, ind_net) %>% head()

## 2) Aktuarielles Controlling – Monatswürfel & Zeitreihen-KPIs

# Monatswürfel: mon, product, partner
month_cube <- exposures %>%
  left_join(policies, by="policy_id") %>%
  mutate(mon = floor_date(period, "month")) %>%
  group_by(mon, product, partner) %>%
  summarise(exp = sum(earned_exposure, na.rm=TRUE),
            ep_net = sum(earned_premium_net, na.rm=TRUE), .groups="drop") %>%
  left_join(claims %>% mutate(mon = floor_date(loss_date,"month")) %>%
              group_by(mon, policy_id) %>% summarise(inc = sum(paid + reserve, na.rm=TRUE), .groups="drop") %>%
              left_join(policies %>% select(policy_id, product, partner), by="policy_id") %>%
              group_by(mon, product, partner) %>% summarise(incurred = sum(inc), .groups="drop"),
            by=c("mon","product","partner")) %>%
  mutate(incurred = coalesce(incurred, 0),
         loss_ratio = incurred/ep_net)

# Plot Loss Ratio Zeitreihe für ein Produkt
ggplot(month_cube %>% filter(product=="Wohngebaeude"),
       aes(mon, loss_ratio)) +
  geom_line() + labs(title="Loss Ratio – Wohngebäude (monatlich)", x=NULL, y="LR")

# 3) Regelmäßige Reportings & Portfolioanalysen
### 3.1 Kohorten nach Underwriting-Jahr & Storno

cohort <- policies %>%
  mutate(uw_year = year(start_date)) %>%
  left_join(exposures %>% group_by(policy_id) %>%
              summarise(ep_net = sum(earned_premium_net), .groups="drop"),
            by="policy_id") %>%
  left_join(claims %>% group_by(policy_id) %>%
              summarise(incurred = sum(paid + reserve, na.rm=TRUE), .groups="drop"),
            by="policy_id") %>%
  group_by(uw_year, product) %>%
  summarise(cancel_rate = mean(state=="cancelled"),
            ep_net = sum(ep_net, na.rm=TRUE),
            incurred = sum(incurred, na.rm=TRUE),
            loss_ratio = incurred/ep_net, .groups="drop") %>%
  arrange(desc(uw_year))
cohort
### 3.2 Segment-Treiber (Heatmap-Denke)

seg <- policies %>%
  left_join(exposures %>% group_by(policy_id) %>% summarise(exp=sum(earned_exposure), ep=sum(earned_premium_net)), by="policy_id") %>%
  left_join(claims   %>% group_by(policy_id) %>% summarise(inc=sum(paid+reserve, na.rm=TRUE)), by="policy_id") %>%
  left_join(tf_wide, by="policy_id") %>%
  group_by(product, plz_zone, baujahr_klasse) %>%
  summarise(exp=sum(exp, na.rm=TRUE), ep=sum(ep, na.rm=TRUE), inc=sum(inc, na.rm=TRUE), .groups="drop") %>%
  mutate(lr = inc/ep, pure = inc/exp)
seg %>% arrange(desc(lr)) %>% head(15)

## 4) Präsentationen (Vorstände/Partner)
### 4.1 Partner-Factsheet (Tabellenoutput)

partner_facts <- policies %>%
  left_join(exposures %>% group_by(policy_id) %>%
              summarise(ep_net=sum(earned_premium_net), .groups="drop"),
            by="policy_id") %>%
  left_join(claims %>% group_by(policy_id) %>%
              summarise(closed = sum(status=="Closed"),
                        n_claims = n(),
                        inc = sum(paid+reserve, na.rm=TRUE), .groups="drop"),
            by="policy_id") %>%
  group_by(partner, product) %>%
  summarise(policen = n(),
            ep_net = sum(ep_net, na.rm=TRUE),
            incurred = sum(inc, na.rm=TRUE),
            close_rate = sum(closed, na.rm=TRUE)/pmax(sum(n_claims, na.rm=TRUE),1),
            avg_deductible = mean(deductible, na.rm=TRUE), .groups="drop") %>%
  arrange(desc(ep_net))
partner_facts

### 4.2 (Optional) PowerPoint-Gerüst mit officer

install.packages("officer")
library(officer)
doc <- read_pptx()
doc <- add_slide(doc, layout="Title and Content", master="Office Theme")
doc <- ph_with(doc, "DOMCURA – Portfolioüberblick", location=ph_location_type("title"))
doc <- ph_with(doc, "Kern-KPIs: Loss Ratio, Treiber, Tarifmaßnahmen, Partner-Highlights",
               location = ph_location_type("body"))
print(doc, target="DOMCURA_Report_Template.pptx")


## 5) Ad-hoc-Analysen

### 5.1 Pricing-Uplift & Elastizität (Volumen/Ertrag)

# df_quotes: quoted_premium, bind_prob, expected_cost (simuliert/aus Historie)
uplift <- 0.04; elasticity <- -0.9
df_quotes <- data.frame(
  quoted_premium = runif(5000, 400, 1400),
  bind_prob = runif(5000, 0.1, 0.5),
  expected_cost = runif(5000, 200, 1000)
)
df_quotes <- df_quotes %>%
  mutate(new_prem = quoted_premium*(1+uplift),
         new_bind = pmin(pmax(bind_prob*(1+elasticity*uplift), 0), 1))

EP_now  <- sum(df_quotes$quoted_premium * df_quotes$bind_prob)
EP_new  <- sum(df_quotes$new_prem      * df_quotes$new_bind)
Cost_now<- sum(df_quotes$expected_cost * df_quotes$bind_prob)
Cost_new<- sum(df_quotes$expected_cost * df_quotes$new_bind)

list(dEP = EP_new-EP_now, dCost = Cost_new-Cost_now, dTechER = (EP_new-Cost_new) - (EP_now-Cost_now))

### 5.2 Trend/Inflation (Severity-Drift)

claims %>%
  mutate(q = floor_date(loss_date, "quarter")) %>%
  filter(!is.na(paid) & paid > 0) %>%
  group_by(q) %>%
  summarise(avg_paid = mean(paid, na.rm=TRUE)) %>%
  arrange(q)

### 5.3 Outlier-Screening

q_hi <- quantile(claims$paid, 0.995, na.rm=TRUE)
sus  <- claims %>%
  filter(paid > q_hi) %>%
  filter(difftime(report_date, loss_date, units="days") > 60)
head(sus %>% select(claim_id, policy_id, loss_date, report_date, paid))

## 7) Governance / Modelle erklären

### 7.1 GLM-Koeffizienten & Effekte

coef_tbl <- broom::tidy(mdl) %>% arrange(desc(estimate))
head(coef_tbl, 15)

### 7.2 Segment-Effekte (Relativitäten) – einfache Tabelle

rel_plz <- df %>%
  group_by(plz_zone) %>%
  summarise(pure = mean(y_pure, na.rm=TRUE),
            exp  = sum(exp, na.rm=TRUE), .groups="drop")
rel_plz$rel <- rel_plz$pure / weighted.mean(df$y_pure, w=df$exp, na.rm=TRUE)
rel_plz %>% arrange(desc(rel))
