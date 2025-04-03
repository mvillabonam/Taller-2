### Codigo de procesamiento de datos

# Objetivo: Construir una base a nivel hogar con la mayor cantidad 
# de variables predictivas

library(pacman)

pacman::p_load(
  tidyverse, dyplr
)

#-----------ESTABLECER DIRECTORIO--------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("Taller2.RData")

#-1. EXPLORING DATASET ------------------------------------
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

# ----> 1. EXPLORING DATA SET  
# MAIN STATISTICS
summary <- dataset |> 
  select(where(is.numeric)) |> 
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") |> 
  group_by(Variable) |> 
  summarise(
    Min = round(min(Value, na.rm = TRUE),2),
    Mean = round(mean(Value, na.rm = TRUE),2),
    Max = round(max(Value, na.rm = TRUE),2),
    NA_count = sum(is.na(Value)),
    NA_percent = round(100 * mean(is.na(Value)), 2)
  )
write_csv(summary, "1.Exploracion.csv")

# ----> CREATE GROUP AGES 
dataset <- dataset |> 
  mutate(age_group = case_when(
    P6040 < 12 ~ "0-12",
    P6040 >= 12 & P6040 < 18 ~ "12-18",
    P6040 >= 18 & P6040 < 24 ~ "18-24",
    P6040 >= 24 & P6040 < 65 ~ "24-65",
    P6040 >= 65 ~ "65+"
  )) |> 
  mutate(age_group = factor(age_group, 
                            levels = c("0-12", "12-18", "18-24", "24-65", "65+"), 
                            ordered = TRUE))
# ----> P6090 y P6100: 
SEGURIDAD_SOCIAL <- dataset|> 
  group_by(P6090, P6100,age_group) |> 
  summarise(count = n(), .groups = "drop_last") |> 
  mutate(total = sum(count)) |> 
  mutate(porcentaje = count / total) 

# ---> CREATING NEW VARIABLES.
# ---> PERTENECE AL RÉGIMEN SUBSIDIADO?
dataset <- dataset |> 
  mutate(regimen_subsidiado = as.numeric(case_when(P6100 == 3 ~ 1, TRUE ~ 0))) ;rm(SEGURIDAD_SOCIAL)

# ---> INFORMAL WORKER
JOB_TYPE <- dataset |> 
  group_by(age_group, P6430) |> 
  summarise(count = sum(is.na(P6430)), .groups = "drop_last") |> 
  ungroup() |> 
  mutate(total = sum(count)) |> 
  mutate(porcentaje = round(count / total, 2))
write_csv(JOB_TYPE, "JOB_TYPE.csv"); rm(JOB_TYPE)

# -- 37% of the MISSING VALUES are between 18-65 AGE groups.
dataset <- dataset %>%
  mutate(
    informal = case_when(
      is.na(P6430) & is.na(regimen_subsidiado) ~ NA_real_,
      P6430 %in% c(4, 5, 6, 8) & regimen_subsidiado == 1 ~ 1,
      TRUE ~ 0
    )
  )

# ----> INCOME 
LABOR_INCOME_DIC <- c("P6510","P6545","P6630s1","P6630s2","P6630s3",
                      "P6630s4","P6630s6","P7510s7","P7510s5","P7510s6")

LABOR_INCOME_IN_DIC <- c("P7422", "P7472","P6510")

SUBSIDIOS <- c("P6585s1","P6585s2","P6585s3","P6585s4","P6580","P6590", 
               "P6600")

OTHER_INCOME_DIC <- c( "P6610","P7040","P7495", "P7500s2",
                      "P7500s3","P7505","P7510s1","P7510s2","P7510s3",
                      "P7510s7") 

# ----> INCOME 

