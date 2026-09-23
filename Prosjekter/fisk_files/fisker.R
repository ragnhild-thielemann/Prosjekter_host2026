
library(workflows)
library(tidymodels)
library(tidyverse)
library(tidytuesdayR)
library(xgboost)

set.seed(670)
data_fish <- tidytuesdayR::tt_load(2026, week = 11)

losses <- ( data_fish$monthly_losses_data)
a <- data_fish$monthly_mortality_data
View(a)

mortality <- data_fish$monthly_mortality_data |>
  mutate(date =  as.factor(month(as.Date(date)))) |> #gjør måned observasjonen har skjedd i til en forklaringsvaribel
  select(species,date,region,median) #sorter bare med de relevante kolonnene vidre

#Deler datasettet i to, der vi har en del for trening, og en del for å teste modellene våre

mortality_delt <- initial_split(mortality, 0.8)
trening_fisk <- training(mortality_delt)
kontroll_fisk <- testing(mortality_delt)

#setter opp en recipe, for å lage responsvariablene vi trener settet på

rec_fisk <- trening_fisk |> #vi oppretter recipen på treningsettet
  recipe(median ~ .) |>
  step_dummy(all_nominal_predictors())

wf <- workflow() |> #Det er samme grunn-workflow for alle modellene
  add_recipe(rec_fisk) 

#Setter nå opp ulike regresjonsmodeller å trene maskinen på datasettet vårt

lm_model <- linear_reg(mode = "regression") |>
  set_engine("lm")

rf_model <- rand_forest(mode = "regression")|>
  set_engine("ranger")

xg_model <- boost_tree() |> #xg-boost bruker det samme som random forest, men det opprettes nye trær som reduserer reusidalene fra de forrige trærne
  set_engine("xgboost")|>
  set_mode("regression")



wf_lm <- wf |>
  add_model(lm_model)|> #legger til den linjære modellen
  fit(trening_fisk) #trener modellen på treningsdataen vår

wf_rf <- wf |>
  add_model(rf_model)|>
  fit(trening_fisk)

wf_xg <- wf |>
  add_model(xg_model) |>
  fit(trening_fisk) 

lasso_model <- linear_reg(mode = "regression",
                          engine = "glmnet", 
                          penalty = 1,
                          mixture = 0.5)

wf_lasso <- wf |>
  add_model(lasso_model)|>
  fit(trening_fisk)

unique(trening_fisk$region)
unique(kontroll_fisk$region)
oppsumerende <- kontroll_fisk |>
  mutate(
    p_lm = predict(wf_lm, kontroll_fisk)$.pred,
    p_rf = predict(wf_rf, kontroll_fisk)$.pred,
    p_xg = predict(wf_xg, kontroll_fisk)$.pred,
    p_lasso = predict(wf_lasso, kontroll_fisk)$.pred
  ) |>
  select(median, p_lm, p_rf, p_xg, p_lasso) |>
  pivot_longer(
    cols = -median,
    names_prefix = "p_",
    names_to = "model",
    values_to = "predict"
  ) |>
  mutate(error = (predict - median)^2) |>
  summarise(
    MSE = mean(error),
    .by = model
  )


oppsumerende

