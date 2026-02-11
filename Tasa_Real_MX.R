# -----------------------------------------------------------------------------
# Script: Tasa_Real_MX.R
# Proyecto: El Escudo de Tasas Reales (Sisu Lab)
# Objetivo: Calcular la Tasa Real (CETES - Inflación) histórica
# -----------------------------------------------------------------------------
install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
library(readr)

# --- PASO 1: Cargar Datos de CETES (Tasa Nominal) ---
# Nota: Los archivos de Banxico suelen traer ~17 lineas de titulos. 
# Usamos 'skip' para saltarlas. Si te da error, abre el CSV y cuenta las lineas.

#Se cargan los archivos de CETES Y INPC

raw_cetes <- read_csv("C:/Users/Erick Perez/Documents/Proyect_Lab/data/cetes_historico.csv",
                      skip = 17, col_names = FALSE, locale = locale(encoding = "latin1"))

raw_inpc <- read_csv("C:/Users/Erick Perez/Documents/Proyect_Lab/data/inpc_historico.csv",
                     skip = 17, col_names = FALSE, locale = locale(encoding = "latin1"))

# Limpieza CETES y INPC

cetes_clean <- raw_cetes %>%
  select(X1, X2) %>%
  rename(fecha = X1, tasa = X2) %>%
  # PASO CRÍTICO: Convertir texto a número
  # Esto convertirá los "N/E" en NA y te dará un aviso (warning) que es NORMAL.
  mutate(tasa = as.numeric(tasa)) %>% 
  mutate(fecha = dmy(fecha)) %>%
  # Ahora filtramos tanto las fechas vacías como las tasas vacías
  filter(!is.na(fecha), !is.na(tasa)) %>% 
  mutate(mes = floor_date(fecha, "month")) %>%
  group_by(mes) %>%
  summarise(tasa = mean(tasa, na.rm = TRUE))

print("Vista previa CETES Mensual:");print(head(cetes_clean))

inpc_clean <- raw_inpc %>%
  select(X1, X2) %>%
  rename(fecha = X1, inpc = X2) %>%
  mutate(inpc = as.numeric(inpc)) %>% 
  mutate(fecha = dmy(fecha)) %>%
  filter(!is.na(fecha), !is.na(inpc)) %>%
  mutate(mes = floor_date(fecha, "month")) %>%
  arrange(mes) %>%
  mutate(inflacion_anual = (inpc / lag(inpc, 12) - 1) * 100) %>%
  filter(!is.na(inflacion_anual))%>%
  select(mes, inflacion_anual)

print("--- INPC LIMPIO Y CALCULADO ---");print(head(inpc_clean))


#PASO 3
# Unimos ambas tablas por la columna común 'mes'
datos_finales <- inner_join(cetes_clean, inpc_clean, by = "mes") %>%
  filter(!is.na(inflacion_anual)) # Quitamos los primeros meses donde no hay cálculo anual

# --- PASO 4: Cálculo de Tasa Real (Aproximación Fisher) ---
# Tasa Real Aprox = Tasa Nominal - Inflación
# Tasa Real Exacta = ((1 + i)/(1 + pi)) - 1
datos_finales <- datos_finales %>%
  mutate(tasa_real = ((1 + tasa/100) / (1 + inflacion_anual/100) - 1) * 100,
  escudo_positivo = ifelse(tasa_real > 0, "Ganancia", "Pérdida"))

print("TABLA FINAL:");print(head(datos_finales))

ggplot(datos_finales, aes(x = mes, y = tasa_real)) +
  
  # CAMBIO CLAVE: Ponemos el fill DENTRO de geom_col, pero dentro de aes()
  geom_col(aes(fill = escudo_positivo), width = 25) + 
  
  geom_hline(yintercept = 0, color = "black") +
  
  # Definimos los colores explícitamente
  scale_fill_manual(values = c(
    "Ganancia" = "#2ecc71",       # Verde
    "Pérdida" = "#e74c3c"  # Rojo
  )) +
  
  labs(
    title = "Histórico: Tasa Real de CETES",
    subtitle = "Verde = Ganancia neta | Rojo = Pérdida de poder adquisitivo",
    x = "Año",
    y = "Tasa Real (%)",
    fill = "Resultado"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
