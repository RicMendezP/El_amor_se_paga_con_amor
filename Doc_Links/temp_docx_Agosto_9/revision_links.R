library(officer)
library(stringr)
library(httr)
library(dplyr)
library(purrr)
library(xml2)
library(stringr)
library(tidyr)
library(readr)

# Ruta del archivo Word
archivo_word <- "/home/ricardo/Personal_RM/Libro/Doc_Links/El_amor_se_paga_con_amor_agosto_9_2025.docx"

#Verificar ruta ok y cargue archivo
getwd()
file.exists(archivo_word)

# Crea una carpeta temporal
carpeta_temp <- "temp_docx"
unzip(archivo_word, exdir = carpeta_temp)
## Leer todos los footnotes y contarlos
footnotes <- read_xml(file.path(carpeta_temp, "word/footnotes.xml"))
# Extraer texto de cada footnote
# map_chr() aplica una función a cada elemento de una lista y devuelve un vector de caracteres (de ahí _chr).
# En este caso, la función aplicada es xml_text, que extrae el texto contenido en un nodo XML.
footnotes_texto_individual <- xml_find_all(footnotes, "//w:footnote") %>% 
  map_chr(xml_text)

# Crear un data frame con número de footnote y texto
df_footnotes <- tibble(
  id = seq_along(footnotes_texto_individual),
  texto = footnotes_texto_individual
)
# Razón por la que salen dos espacios en blanco al inicio del df_footnotes 
# the first two <w:footnote> nodes inside footnotes.xml are special footnotes used by Word, not actual ones from your document.
# They usually have w:id="-1" and w:id="0" and are reserved for things like separator lines or continuation separators.
# Guardar como CSV
archivo_footnotes <- "/home/ricardo/Personal_RM/Libro/Doc_Links/footnotes_completos.csv"
write.csv(df_footnotes, archivo_footnotes, row.names = FALSE)

#Extraer URLs de cada footnote (si hay)
df_footnotes_urls <- df_footnotes %>%
  mutate(urls = str_extract_all(texto, "https?://[^\\s]+"))

# 2️⃣ Crear dataframe con URLs extraídas
# ese asterisco rojo que ves en RStudio no significa un error sintáctico,
# sino que el editor a veces marca advertencias visuales por:
# Uso de funciones anidadas (map_chr() dentro de mutate()),
# o símbolos como ~ (tilde) en funciones lambda (que RStudio resalta por precaución).
df_footnotes_urls <- df_footnotes %>%
  mutate(urls = str_extract_all(texto, "https?://[^\\s]+")) %>%
  mutate(urls = map_chr(urls, ~ if (length(.x) == 0) "" else paste(.x, collapse = ", ")))

# Guardar como CSV
archivo_footnotes_urls <- "/home/ricardo/Personal_RM/Libro/Doc_Links/footnotes_con_id_urls.csv"
write.csv(df_footnotes_urls, archivo_footnotes_urls, row.names = FALSE)

# Ahora vamos rear tabla expandida: una fila por cada URL (pueden haber varias urls en una fila),
## y luego verificar urls con errores
df_urls_expandido <- df_footnotes_urls %>%
  # Filtrar solo los footnotes que tienen URLs
  filter(urls != "") %>%
  # Separar las URLs en filas individuales
  separate_rows(urls, sep = ",\\s*") %>% ## función de tidyr
  # Limpiar espacios extra
  mutate(urls = str_trim(urls))
#  Guardar el archivo expandido (opcional)
archivo_urls_expandido <- "/home/ricardo/Personal_RM/Libro/Doc_Links/footnotes_urls_expandido.csv"
write.csv(df_urls_expandido, archivo_urls_expandido, row.names = FALSE)

# 🧠 Función para verificar el estado HTTP de una URL
# tryCatch() Si la URL funciona → devuelve el status code (por ejemplo 200).
# Si hay error (timeout, URL mal escrita, servidor no responde) 
# → entra en el bloque error = function(e) y devuelve NA, en lugar de detener el script.

check_url_status <- function(url) {
  tryCatch({
    resp <- httr::HEAD(url, timeout(20)) # usamos HEAD porque es más rápido que GET
    status <- httr::status_code(resp)
    return(status)
  }, error = function(e) {
    return(NA) # si hay error (timeout, DNS, etc.)
  })
}

# Verificar todas las URLs OJO TOMA UNOS DIEZ MINUTOS
df_urls_verificadas <- df_urls_expandido %>%
  mutate(status_code = map_int(urls, check_url_status))

# 🚨 Filtrar solo las que tienen error (no 200)
df_errores_urls <- df_urls_verificadas %>%
  filter(is.na(status_code) | status_code != 200)

# 💾 Guardar en nuevo archivo CSV
archivo_errores <- "/home/ricardo/Personal_RM/Libro/Doc_Links/errores_urls.csv"
write.csv(df_errores_urls, archivo_errores, row.names = FALSE)

cat("✅ Archivo creado con las URLs que presentan errores en:\n", archivo_errores)

### resumen
# 🧾 Resumen del chequeo de URLs
total_urls <- nrow(df_urls_expandido)
total_ok <- sum(df_urls_verificadas$status_code == 200, na.rm = TRUE)
total_error <- nrow(df_errores_urls)

cat("📊 RESUMEN DE VERIFICACIÓN:\n")
cat("🔹 Total de URLs verificadas:", total_urls, "\n")
cat("✅ URLs correctas (200):", total_ok, "\n")
cat("🚨 URLs con error:", total_error, "\n")

# Mensaje adicional
if (total_error == 0) {
  cat("🎉 Todas las URLs parecen estar funcionando correctamente.\n")
} else {
  cat("⚠️ Revisa el archivo de errores para más detalles:\n", archivo_errores, "\n")
}










