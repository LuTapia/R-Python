library(googlesheets4)

table <- "responses"

saveData <- function(data) {
  # Los datos deben ser un marco de datos en lugar de un vector con nombre
  data <- data %>% as.list() %>% data.frame()
  # Agrega los datos como una nueva fila
  sheet_append(SHEET_ID, data)
}

loadData <- function() {
  # Leer los datos
  read_sheet(SHEET_ID)
}
