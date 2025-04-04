### Codigo de procesamiento de datos

# Objetivo: Construir una base a nivel hogar con la mayor cantidad 
# de variables predictivas

library(pacman)

pacman::p_load(
  tidyverse
)

load("~/Taller-2/Taller2.RData")

train_hogares <- train_hogares |>
  select(colnames(test_hogares))

train_personas <- train_personas |> 
  select(colnames(test_personas))

dataset <- train_hogares |> 
  left_join(train_personas, by = c("id", "Clase", "Dominio", "Fex_c", "Fex_dpto", "Depto"))

dataset |> 
  select(starts_with("P")) |> 
  mutate_all(as.factor) |> 
  summary()

sum <- dataset |> 
  select(P6050,
         P6545,
         P6580,
         P6585s1,
         P6585s2,
         P6585s3,
         P6585s4,
         P6590,
         P6600,
         P6610,
         P6620,
         P7040,
         P7045,
         P7050,
         P7090,
         P7110,
         P7120,
         P7150,
         P7160,
         P7310,
         P7350,
         P7510s1,
         P7510s2,
         P7510s3,
  ) |> 
  mutate_all(as.factor) |>
  summary()


dataset <- dataset |> 
  group_by(id) |> 
  mutate(recibe_primas = case_when(
    any(P6545 == 1 | P6580 == 1, na.rm = TRUE) ~ 1,
        TRUE ~ 0),
    recibe_subsidios = case_when(
      any(P6585s1 == 1 | P6585s2 == 1 | P6585s3 == 1 | P6585s4 == 1, na.rm = TRUE) ~ 1,
          TRUE ~ 0),
    ing_nomonet = case_when(
      any(P6590 == 1 | P6600 == 1 | P6620 == 1, na.rm = TRUE) ~ 1,
      TRUE ~ 0),
    recibe_remesas = case_when(
      any(P7510s1 == 1 | P7510s2 == 1 | P7510s3 == 1, na.rm = TRUE) ~ 1,
      TRUE ~ 0),
    des_duradero = case_when(
      any(P7310 == 2 & (P7350 %in% c(6, 7, 8) | is.na(P7350)), na.rm = TRUE) ~ 1,
      TRUE ~ 0),
    empleo_precario_jefe = case_when(
      P6050 == 1 & ((P6590 == 1 | P6600 == 1) | (P7040 == 1 & P7050 %in% c(6, 7, 8))) ~ 1,
      TRUE ~ 0),
    segunda_informal = case_when(
      any(P7040 == 1 & P7050 %in% c(6, 7, 8)) ~ 1,
      TRUE ~ 0),
    int_subempleo = case_when(
      sum(Oc, na.rm = TRUE) > 0 ~ sum(P7090 == 1 & P7110 == 1 & P7120 == 1, na.rm = TRUE) / sum(Oc, na.rm = TRUE),
      TRUE ~ 1)
    )









