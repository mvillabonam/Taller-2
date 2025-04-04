### Codigo de procesamiento de datos

# Objetivo: Construir una base a nivel hogar con la mayor cantidad 
# de variables predictivas

library(pacman)

pacman::p_load(
  tidyverse, dyplr, ggplot2
)

#-----------ESTABLECER DIRECTORIO--------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("Taller2.RData")

#-1. EXPLORING train_dataset ------------------------------------
train_hogares <- train_hogares |>
  select(colnames(test_hogares))

train_personas <- train_personas |> 
  select(colnames(test_personas))

train_dataset <- train_hogares |> 
  left_join(train_personas, by = c("id", "Clase", "Dominio", "Fex_c", "Fex_dpto", "Depto"))

train_dataset |> 
  select(starts_with("P")) |> 
  mutate_all(as.factor) |> 
  summary()

# ----> 1. EXPLORING DATA SET  
# MAIN STATISTICS
summary <- train_dataset |> 
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
train_dataset <- train_dataset |> 
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
SEGURIDAD_SOCIAL <- train_dataset|> 
  group_by(P6090, P6100,age_group) |> 
  summarise(count = n(), .groups = "drop_last") |> 
  mutate(total = sum(count)) |> 
  mutate(porcentaje = count / total) 

# ---> CREATING NEW VARIABLES.
# ---> PERTENECE AL RÉGIMEN SUBSIDIADO?
train_dataset <- train_dataset |> 
  mutate(regimen_subsidiado = as.numeric(case_when(P6100 == 3 ~ 1, TRUE ~ 0))) ;rm(SEGURIDAD_SOCIAL)

# ---> INFORMAL WORKER
JOB_TYPE <- train_dataset |> 
  group_by(age_group, P6430) |> 
  summarise(count = sum(is.na(P6430)), .groups = "drop_last") |> 
  ungroup() |> 
  mutate(total = sum(count)) |> 
  mutate(porcentaje = round(count / total, 2))
write_csv(JOB_TYPE, "JOB_TYPE.csv")
#rm(JOB_TYPE)

# -- 37% of the MISSING VALUES are between 18-65 AGE groups.
train_dataset <- train_dataset %>%
  mutate(
    informal = case_when(
      is.na(P6430) & is.na(regimen_subsidiado) ~ NA_real_,
      P6430 %in% c(4, 5, 6, 8) & regimen_subsidiado == 1 ~ 1,
      TRUE ~ 0
    )
  )

# ----> Overcrowding (Hacinamiento)
train_dataset <- train_dataset |>
  mutate(hacinamiento = Nper / P5010)

# ----> Housing Cost 

# household costs by tenure type (P5090)
train_dataset |>
  group_by(P5090) |>
  summarise(
    pays_amortization = sum(!is.na(P5100)),
    pays_rent = sum(!is.na(P5140)),
    pays_imputed_rent = sum(!is.na(P5130)),
    total_people = n()
  )
# We compare amortization vs imputed rent for some households (P5090 == 2)
train_dataset |>
  filter(P5090 == 2) |>
  select(id, P5100, P5130) |>
  mutate(mayor_gasto = ifelse(P5100 > P5130, "Amortización", "Imputed Rent")) |> 
  count(mayor_gasto)

# We assign rent (P5140) or imputed rent (P5130) based on P5090
train_dataset <- train_dataset |>
  mutate(costo_vivienda = ifelse(P5090 == 3, P5140, P5130))

summary(train_dataset$costo_vivienda)

# Winsorize extreme values
lim_inf <- quantile(train_dataset$costo_vivienda, 0.01, na.rm = TRUE)
lim_sup <- quantile(train_dataset$costo_vivienda, 0.99, na.rm = TRUE)

ggplot(train_dataset, aes(y = costo_vivienda)) +
  geom_boxplot(fill = "lightblue", alpha = 0.5) +
  geom_hline(yintercept = lim_inf, color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = lim_sup, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Boxplot with Winsorized Limits", y = "Housing cost") +
  theme_minimal()

train_dataset <- train_dataset |>
  mutate(costo_vivienda = pmin(pmax(costo_vivienda, lim_inf), lim_sup))

# Replace missing values (98, 99) with the median. Or reg?? 
train_dataset |> count(costo_vivienda < 100)
train_dataset <- train_dataset |>
  mutate(costo_vivienda = ifelse(costo_vivienda %in% c(98, 99), median(costo_vivienda, na.rm = TRUE), costo_vivienda))


# ----> Dependency ratio
train_dataset <- train_dataset |>
  group_by(id) |>
  mutate(
    num_ocupados = sum(Oc, na.rm = TRUE),
    num_dependientes = sum(Ina, na.rm = TRUE) + 
                   sum(Des, na.rm = TRUE) + 
                   sum(ifelse(Pet == 0, 1, 0), na.rm = TRUE),
    tasa_dependencia = ifelse(num_ocupados > 0, num_dependientes / num_ocupados, NA)
  ) |>
  ungroup()
head(train_dataset)

#Revisando si es viable imputar los NA
max_tasa <- max(train_dataset$tasa_dependencia, na.rm = TRUE)
print(max_tasa) 
mediana_tasa <- median(train_dataset$tasa_dependencia, na.rm = TRUE)
print(mediana_tasa)
cantidad_na <- sum(is.na(train_dataset$tasa_dependencia))
print(cantidad_na)

#Imputation ??
sum(is.na(train_dataset$tasa_dependencia))
max(train_dataset$tasa_dependencia, na.rm = TRUE)
median(train_dataset$tasa_dependencia, na.rm = TRUE)
quantile(train_dataset$tasa_dependencia, probs = c(0.25, 0.5, 0.75, 0.95), na.rm = TRUE)

#Being conservative, we assume a high dependency in the missing data.
train_dataset <- train_dataset |> 
  mutate(tasa_dependencia = ifelse(is.na(tasa_dependencia), quantile(tasa_dependencia, probs = 0.95, na.rm = TRUE), tasa_dependencia))

# ----> Household education level
train_dataset <- train_dataset |>
  group_by(id) |>
  mutate(
    median_education = median(P6210, na.rm = TRUE),
    max_educ_hogar = max(P6210, na.rm = TRUE) 
  ) |>
  ungroup()

head(train_dataset)

sum(is.na(train_dataset$median_education))
sum(is.na(train_dataset$max_educ_hogar))
hist(train_dataset$median_education)
hist(train_dataset$max_educ_hogar)

# ----> Health vulnerability
#1 Vulnerable - 0 Not vulnerable
train_dataset <- train_dataset |> 
  group_by(id) |> 
  mutate(vulnerabilidad= as.integer(all(P6090 == 2 | P6100 %in% c(3, 9), na.rm = TRUE))) |> 
  ungroup()

sum(is.na(train_dataset$vulnerabilidad))
hist(train_dataset$vulnerabilidad)

# ----> Informal work per household
train_dataset <- train_dataset |> 
  group_by(id) |> 
  mutate(
    num_informal = sum(P6430 %in% c(4, 8), na.rm = TRUE),
    prop_informal = num_informal / num_ocupados
  ) |> 
  ungroup()

#Imputation
sum(is.na(train_dataset$prop_informal))#Na household without ocuppied people
hist(train_dataset$prop_informal)

train_dataset <- train_dataset |> 
  mutate(prop_informal = ifelse(is.na(prop_informal), 1, prop_informal))

# ----> INCOME 
LABOR_INCOME_DIC <- c("P6510","P6545","P6630s1","P6630s2","P6630s3",
                      "P6630s4","P6630s6","P7510s7","P7510s5","P7510s6")

LABOR_INCOME_IN_DIC <- c("P7422", "P7472","P6510")

SUBSIDIOS <- c("P6585s1","P6585s2","P6585s3","P6585s4","P6580","P6590", 
               "P6600")

OTHER_INCOME_DIC <- c( "P6610","P7040","P7495", "P7500s2",
                       "P7500s3","P7505","P7510s1","P7510s2","P7510s3",
                       "P7510s7") 

# ---> CREATING NEW VARIABLES
train_dataset <- train_dataset |> 
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

train_dataset <- train_dataset |> 
  filter(P6050 == 1)

# ------------------------------------------------------------------------------
# -----------> TEST DATA SET 
#-1. EXPLORING test_dataset ------------------------------------
test_dataset <- test_hogares |> 
  left_join(test_personas, by = c("id", "Clase", "Dominio", "Fex_c", "Fex_dpto", "Depto"))

test_dataset |> 
  select(starts_with("P")) |> 
  mutate_all(as.factor) |> 
  summary()

# ----> 1. EXPLORING DATA SET  
# MAIN STATISTICS
summary <- test_dataset |> 
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
test_dataset <- test_dataset |> 
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
SEGURIDAD_SOCIAL <- test_dataset|> 
  group_by(P6090, P6100,age_group) |> 
  summarise(count = n(), .groups = "drop_last") |> 
  mutate(total = sum(count)) |> 
  mutate(porcentaje = count / total) 

# ---> CREATING NEW VARIABLES.
# ---> PERTENECE AL RÉGIMEN SUBSIDIADO?
test_dataset <- test_dataset |> 
  mutate(regimen_subsidiado = as.numeric(case_when(P6100 == 3 ~ 1, TRUE ~ 0))) ;rm(SEGURIDAD_SOCIAL)

# ---> INFORMAL WORKER
JOB_TYPE <- test_dataset |> 
  group_by(age_group, P6430) |> 
  summarise(count = sum(is.na(P6430)), .groups = "drop_last") |> 
  ungroup() |> 
  mutate(total = sum(count)) |> 
  mutate(porcentaje = round(count / total, 2))
write_csv(JOB_TYPE, "JOB_TYPE.csv")
#rm(JOB_TYPE)

# -- 37% of the MISSING VALUES are between 18-65 AGE groups.
test_dataset <- test_dataset %>%
  mutate(
    informal = case_when(
      is.na(P6430) & is.na(regimen_subsidiado) ~ NA_real_,
      P6430 %in% c(4, 5, 6, 8) & regimen_subsidiado == 1 ~ 1,
      TRUE ~ 0
    )
  )

# ----> Overcrowding (Hacinamiento)
test_dataset <- test_dataset |>
  mutate(hacinamiento = Nper / P5010)

# ----> Housing Cost 

# household costs by tenure type (P5090)
test_dataset |>
  group_by(P5090) |>
  summarise(
    pays_amortization = sum(!is.na(P5100)),
    pays_rent = sum(!is.na(P5140)),
    pays_imputed_rent = sum(!is.na(P5130)),
    total_people = n()
  )
# We compare amortization vs imputed rent for some households (P5090 == 2)
test_dataset |>
  filter(P5090 == 2) |>
  select(id, P5100, P5130) |>
  mutate(mayor_gasto = ifelse(P5100 > P5130, "Amortización", "Imputed Rent")) |> 
  count(mayor_gasto)

# We assign rent (P5140) or imputed rent (P5130) based on P5090
test_dataset <- test_dataset |>
  mutate(costo_vivienda = ifelse(P5090 == 3, P5140, P5130))

summary(test_dataset$costo_vivienda)

# Winsorize extreme values
lim_inf <- quantile(test_dataset$costo_vivienda, 0.01, na.rm = TRUE)
lim_sup <- quantile(test_dataset$costo_vivienda, 0.99, na.rm = TRUE)

ggplot(test_dataset, aes(y = costo_vivienda)) +
  geom_boxplot(fill = "lightblue", alpha = 0.5) +
  geom_hline(yintercept = lim_inf, color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = lim_sup, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Boxplot with Winsorized Limits", y = "Housing cost") +
  theme_minimal()

test_dataset <- test_dataset |>
  mutate(costo_vivienda = pmin(pmax(costo_vivienda, lim_inf), lim_sup))

# Replace missing values (98, 99) with the median. Or reg?? 
test_dataset |> count(costo_vivienda < 100)
test_dataset <- test_dataset |>
  mutate(costo_vivienda = ifelse(costo_vivienda %in% c(98, 99), median(costo_vivienda, na.rm = TRUE), costo_vivienda))


# ----> Dependency ratio
test_dataset <- test_dataset |>
  group_by(id) |>
  mutate(
    num_ocupados = sum(Oc, na.rm = TRUE),
    num_dependientes = sum(Ina, na.rm = TRUE) + 
      sum(Des, na.rm = TRUE) + 
      sum(ifelse(Pet == 0, 1, 0), na.rm = TRUE),
    tasa_dependencia = ifelse(num_ocupados > 0, num_dependientes / num_ocupados, NA)
  ) |>
  ungroup()
head(test_dataset)

#Revisando si es viable imputar los NA
max_tasa <- max(test_dataset$tasa_dependencia, na.rm = TRUE)
print(max_tasa) 
mediana_tasa <- median(test_dataset$tasa_dependencia, na.rm = TRUE)
print(mediana_tasa)
cantidad_na <- sum(is.na(test_dataset$tasa_dependencia))
print(cantidad_na)

#Imputation ??
sum(is.na(test_dataset$tasa_dependencia))
max(test_dataset$tasa_dependencia, na.rm = TRUE)
median(test_dataset$tasa_dependencia, na.rm = TRUE)
quantile(test_dataset$tasa_dependencia, probs = c(0.25, 0.5, 0.75, 0.95), na.rm = TRUE)

#Being conservative, we assume a high dependency in the missing data.
test_dataset <- test_dataset |> 
  mutate(tasa_dependencia = ifelse(is.na(tasa_dependencia), quantile(tasa_dependencia, probs = 0.95, na.rm = TRUE), tasa_dependencia))

# ----> Household education level
test_dataset <- test_dataset |>
  group_by(id) |>
  mutate(
    median_education = median(P6210, na.rm = TRUE),
    max_educ_hogar = max(P6210, na.rm = TRUE) 
  ) |>
  ungroup()

head(test_dataset)

sum(is.na(test_dataset$median_education))
sum(is.na(test_dataset$max_educ_hogar))
hist(test_dataset$median_education)
hist(test_dataset$max_educ_hogar)

# ----> Health vulnerability
#1 Vulnerable - 0 Not vulnerable
test_dataset <- test_dataset |> 
  group_by(id) |> 
  mutate(vulnerabilidad= as.integer(all(P6090 == 2 | P6100 %in% c(3, 9), na.rm = TRUE))) |> 
  ungroup()

sum(is.na(test_dataset$vulnerabilidad))
hist(test_dataset$vulnerabilidad)

# ----> Informal work per household

test_dataset <- test_dataset |> 
  group_by(id) |> 
  mutate(
    num_informal = sum(P6430 %in% c(4, 8), na.rm = TRUE),
    prop_informal = num_informal / num_ocupados
  ) |> 
  ungroup()

#Imputation
sum(is.na(test_dataset$prop_informal))#Na household without ocuppied people
hist(test_dataset$prop_informal)

test_dataset <- test_dataset |> 
  mutate(prop_informal = ifelse(is.na(prop_informal), 1, prop_informal))

# ----> INCOME 
LABOR_INCOME_DIC <- c("P6510","P6545","P6630s1","P6630s2","P6630s3",
                      "P6630s4","P6630s6","P7510s7","P7510s5","P7510s6")

LABOR_INCOME_IN_DIC <- c("P7422", "P7472","P6510")

SUBSIDIOS <- c("P6585s1","P6585s2","P6585s3","P6585s4","P6580","P6590", 
               "P6600")

OTHER_INCOME_DIC <- c( "P6610","P7040","P7495", "P7500s2",
                       "P7500s3","P7505","P7510s1","P7510s2","P7510s3",
                       "P7510s7") 

# ---> CREATING NEW VARIABLES
test_dataset <- test_dataset |> 
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

test_dataset <- test_dataset |> 
  filter(P6050 == 1)


test_dataset <- test_dataset |> 
  filter(P6050 == 1)

