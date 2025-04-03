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
  mutate(age_group = factor(age_group,
                            levels = c("0-12", "12-18", "18-24", "24-55", "55+"),
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


# ----> P6630s1, P6630s2, P6630s3, P6630s4,P6630s6, P6800, P6870, P6920

# ----> DESCRIPCIÓN INGRESOS:
LABOR_INCOME_F <- c("P6630s1", "P6630s2", "P6630s3", "P6630s4", 
                   "P6630s6","P6510",
                   "P7510s5","P7510s6","P7510s7")


LABOR_INCOME_IN <- c("P7422", "P7472", "P6630s3", "P6630s4", 
                    "P6630s6", "P6800", "P6870", "P6920","P6510",
                    "P7510s5","P7510s6","P7510s7")

OTHER_INCOME <- c("P7495", "P7500s2", "P7500s3", "P7505", 
                     "P7510s5", "P7510s6", "P7510s7")

OCUPIED_INCOME <- dataset |>
  filter(age_group > "12-18",Oc==1) |>
  group_by(P6920) |>
  summarise(across(all_of(LABOR_INCOME_F),
                   list(
                     mean = ~mean(.x, na.rm = TRUE),
                     naTotal = ~sum(is.na(.x)),
                     naPercent = ~mean(is.na(.x)) * 100
                   ),
                   .names = "{.col}_{.fn}"),
            .groups = "drop") |>
  pivot_longer(-P6920,
               names_to = c("variable", "stat"),
               names_sep = "_",
               values_to = "value") |>
  pivot_wider(names_from = stat, values_from = value)

# ----> Overcrowding (Hacinamiento)
dataset <- dataset |>
  mutate(hacinamiento = Nper / P5010)

# ----> Housing Cost 

# household costs by tenure type (P5090)
dataset |>
  group_by(P5090) |>
  summarise(
    pays_amortization = sum(!is.na(P5100)),
    pays_rent = sum(!is.na(P5140)),
    pays_imputed_rent = sum(!is.na(P5130)),
    total_people = n()
  )
# We compare amortization vs imputed rent for some households (P5090 == 2)
dataset |>
  filter(P5090 == 2) |>
  select(id, P5100, P5130) |>
  mutate(mayor_gasto = ifelse(P5100 > P5130, "Amortización", "Imputed Rent")) |> 
  count(mayor_gasto)
dataset <- dataset |>
  select(-mayor_gasto)

# We assign rent (P5140) or imputed rent (P5130) based on P5090
dataset <- dataset |>
  mutate(costo_vivienda = ifelse(P5090 == 3, P5140, P5130))

summary(dataset$costo_vivienda)

# Winsorize extreme values
lim_inf <- quantile(dataset$costo_vivienda, 0.01, na.rm = TRUE)
lim_sup <- quantile(dataset$costo_vivienda, 0.99, na.rm = TRUE)

ggplot(dataset, aes(y = costo_vivienda)) +
  geom_boxplot(fill = "lightblue", alpha = 0.5) +
  geom_hline(yintercept = lim_inf, color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = lim_sup, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Boxplot with Winsorized Limits", y = "Housing cost") +
  theme_minimal()

dataset <- dataset |>
  mutate(costo_vivienda = pmin(pmax(costo_vivienda, lim_inf), lim_sup))

# Replace missing values (98, 99) with the median. Or reg?? 
dataset |> count(costo_vivienda < 100)
dataset <- dataset |>
  mutate(costo_vivienda = ifelse(costo_vivienda %in% c(98, 99), median(costo_vivienda, na.rm = TRUE), costo_vivienda))


# ----> Dependency ratio
dataset <- dataset |>
  group_by(id) |>
  mutate(
    ocupados = sum(Oc, na.rm = TRUE),
    dependientes = sum(Ina, na.rm = TRUE) + 
                   sum(Des, na.rm = TRUE) + 
                   sum(ifelse(Pet == 0, 1, 0), na.rm = TRUE),
    tasa_dependencia = ifelse(ocupados > 0, dependientes / ocupados, NA)
  ) |>
  ungroup()
head(dataset)

#Revisando si es viable imputar los NA
max_tasa <- max(dataset$tasa_dependencia, na.rm = TRUE)
print(max_tasa) 
mediana_tasa <- median(dataset$tasa_dependencia, na.rm = TRUE)
print(mediana_tasa)
cantidad_na <- sum(is.na(dataset$tasa_dependencia))
print(cantidad_na)

# ----> Household education level
dataset <- dataset |>
  group_by(id) |>
  mutate(
    educacion_promedio_ = median(P6210, na.rm = TRUE),
    max_educ_hogar = max(P6210, na.rm = TRUE) 
  ) |>
  ungroup()

sum(is.na(dataset$median_education))
hist(dataset$median_education)

# ----> Household health security
dataset <- dataset |>
  group_by(id) |>
  mutate(health_insurance_coverage = mean(P6090 == 1, na.rm = TRUE)) |>
  ungroup()

sum(is.na(dataset$average_education))
hist(dataset$health_insurance_coverage, col = "lightgreen")
