library(shiny)
library(shinyWidgets)
library(shinyjs)
library(bslib)
library(dplyr)
library(rvest)
library(ggplot2)
library(tictoc)
library(stringr)
library(purrr)
library(tibble)

tic("Lectura")
# Carga el archivo HTML
game_html <- read_html("./escenarios/game12.html")

lotka_cells <- game_html %>% 
  html_nodes("div.lotka-cell")

fct_def <- function(nodo){
  
  evaluacion <- nodo |> 
    html_nodes("div") |> 
    html_attrs()
  
  es_fijo <- str_detect(evaluacion[[1]], "locked")
  
  if(length(evaluacion) == 1){
    
    sentido_1 <- NA
    signo_1 <- NA
    
    sentido_2 <- NA
    signo_2 <- NA
    
  } else{
    
    if(length(evaluacion) == 2){
      
      sentido_1 <- sub(".*--", "", evaluacion[[2]])
      
      signo_1 <- nodo |> 
        html_nodes("svg") |> 
        html_attrs()
      
      signo_1 <- signo_1[[2]] |> 
        as.matrix() |> 
        as.data.frame() |> 
        rownames_to_column() |> 
        filter(rowname == "aria-label")
      
      signo_1 <- signo_1$V1[1]
      
      sentido_2 <- NA
      signo_2 <- NA
      
    } else{
      
      if(length(evaluacion) == 3){
        
        sentido_1 <- sub(".*--", "", evaluacion[[2]])
        
        signo_1 <- nodo |> 
          html_nodes("svg") |> 
          html_attrs()
        
        signo_1 <- signo_1[[2]] |> 
          as.matrix() |> 
          as.data.frame() |> 
          rownames_to_column() |> 
          filter(rowname == "aria-label")
        
        signo_1 <- signo_1$V1[1]
        
        sentido_2 <- sub(".*--", "", evaluacion[[3]])
        
        signo_2 <- nodo |> 
          html_nodes("svg") |> 
          html_attrs()
        
        signo_2 <- signo_2[[3]] |> 
          as.matrix() |> 
          as.data.frame() |> 
          rownames_to_column() |> 
          filter(rowname == "aria-label")
        
        signo_2 <- signo_2$V1[1]
        
      }
    }
  }
  
  if(length(nodo |> 
            html_elements("svg") |> 
            html_attrs()) > 1){
    
    img <- nodo |> 
      html_elements("svg") |> 
      html_attrs() 
    
    img <- img[[1]] |>
      unlist() |> 
      as.matrix() |> 
      as.data.frame() |> 
      rownames_to_column() |> 
      filter(rowname == "aria-label")
    
    img <- img$V1[1]
    
  }else{
    
    img <- nodo |> 
      html_elements("svg") |> 
      html_attrs() |>
      unlist() |> 
      as.matrix() |> 
      as.data.frame() |> 
      rownames_to_column() |> 
      filter(rowname == "aria-label")
    
    img <- img$V1[1]
    
  }
  
  return(list(img = img,
              fijo = es_fijo,
              sentido_1 = sentido_1,
              signo_1 = signo_1,
              sentido_2 = sentido_2,
              signo_2 = signo_2))
  
}

final <- lapply(lotka_cells, fct_def) |> 
  bind_rows()

final <- data.frame(final)

final <- final |> 
  rowid_to_column() |> 
  mutate(eje_x = rep(1:6, each = 6),
         eje_y = rep(1:6, times = 6))

escenario_base <- final |> 
  ggplot(aes(y = eje_x, x = eje_y, fill = fijo)) +
  geom_tile(linewidth = 1, col = "#889fbf") +
  scale_y_reverse() +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_line(color = "#889fbf",
                                        size = 0.75,
                                        linetype = 1),
        panel.grid.minor = element_line(color = "#889fbf",
                                        size = 0.75,
                                        linetype = 1),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
  scale_fill_manual(values = c("#eff6fe", "#9db4d3")) +
  coord_fixed(ratio = 1)

elementos <- final |> 
  filter(fijo == TRUE) |> 
  group_split(img)

simbolos <- final  |> 
  filter(!is.na(signo_1)) |> 
  group_split(signo_1)

simbolos <- rbind(bind_rows(simbolos), 
                  bind_rows(simbolos) |> 
                    filter(!is.na(sentido_2)) |> 
                    mutate(sentido_1 = sentido_2,
                           signo_1 = signo_2)
)

simbolos <- simbolos  |> 
  filter(!is.na(signo_1)) |> 
  group_split(signo_1)


# if(length(simbolos) == 1){
#   
#   escenario_base <- escenario_base + 
#     geom_point(data = elementos[[1]],
#                size = 9, shape = 17, color = "#1e2c46") +
#     geom_point(data = elementos[[2]],
#                size = 12, shape = 19, color = "#de6f41")+
#     geom_point(data = simbolos[[1]] |> 
#                  filter(sentido_1 == "down") |> 
#                  mutate(eje_x = eje_x + 0.5), 
#                shape = "X", size = 2, color = "black", stroke = 5)+
#     geom_point(data = simbolos[[1]] |> 
#                  filter(sentido_1 == "right") |> 
#                  mutate(eje_y = eje_y + 0.5), 
#                shape = "X", size = 2, color = "black", stroke = 5)
#   
# } else{
#   
#   
#   escenario_base <- escenario_base + 
#     geom_point(data = elementos[[1]],
#                size = 9, shape = 17, color = "#1e2c46") +
#     geom_point(data = elementos[[2]],
#                size = 12, shape = 19, color = "#de6f41")+
#     geom_point(data = simbolos[[1]] |> 
#                  filter(sentido_1 == "down") |> 
#                  mutate(eje_x = eje_x + 0.5), 
#                shape = "X", size = 2, color = "black", stroke = 5)+
#     geom_point(data = simbolos[[1]] |> 
#                  filter(sentido_1 == "right") |> 
#                  mutate(eje_y = eje_y + 0.5), 
#                shape = "X", size = 2, color = "black", stroke = 5)+
#     geom_point(data = simbolos[[2]] |> 
#                  filter(sentido_1 == "down") |> 
#                  mutate(eje_x = eje_x + 0.5), 
#                shape = "=", size = 6, color = "black", stroke = 5)+
#     geom_point(data = simbolos[[2]] |> 
#                  filter(sentido_1 == "right") |> 
#                  mutate(eje_y = eje_y + 0.5), 
#                shape = "=", size = 6, color = "black", stroke = 5)
#   
# }
# 
# 
# escenario_base


# convertir_simbolos <- function(simbolos) {
#   simbolos |> 
#     mutate(
#       nx = eje_x + as.integer(sentido_1 == "down"),
#       ny = eje_y + as.integer(sentido_1 == "right")
#     )
# }
# 
# cruces <- simbolos[[1]] |> 
#   convertir_simbolos()
# 
# iguales <- simbolos[[2]] |> 
#   convertir_simbolos()
# 
# restricciones <- bind_rows(cruces, iguales)


library(dplyr)
library(purrr)

# Función para añadir columnas calculadas
convertir_simbolos <- function(simbolo) {
  simbolo |> 
    mutate(
      nx = eje_x + as.integer(sentido_1 == "down"),
      ny = eje_y + as.integer(sentido_1 == "right")
    )
}

# Aplicar la transformación solo si hay datos
simbolos_convertidos <- simbolos |> 
  map(convertir_simbolos)

# Combinar todo en un solo dataframe
todos <- bind_rows(simbolos_convertidos)

# Separar según el tipo de signo
cruces <- todos |> filter(signo_1 == "Cross")
iguales <- todos |> filter(signo_1 %in% c("Equal", "Igual"))

# Resultado combinado (si lo necesitas)
restricciones <- bind_rows(cruces, iguales)



# Función para generar listas de pares desde filas
generate_pairs <- function(df, tipo) {
  df %>%
    filter(str_detect(signo_1, tipo)) %>%
    rowwise() %>%
    mutate(pair = list(list(c(eje_x, eje_y), c(nx, ny)))) %>%
    ungroup() %>%
    pull(pair)
}

# Aplicación
not_equal_pairs <- generate_pairs(restricciones, "Cross")
equal_pairs     <- generate_pairs(restricciones, "Equal|Igual")



# Valores fijos: "Moon" = 1, "Sun" = 2
valores_fijos <- final %>%
  filter(fijo == TRUE) %>%
  mutate(valor = ifelse(img == "Moon" | img == "Luna", 1L, 2L))



library(ompr)
library(dplyr)

n <- 6
model <- MIPModel() %>%
  
  # The number k stored in position i,j
  add_variable(x[i, j, k], i = 1:n, j = 1:n, k = 1:2, type = "binary") %>%
  
  # no objective
  set_objective(0) %>%
  
  # only one number can be assigned per cell
  add_constraint(sum_over(x[i, j, k], k = 1:2) == 1, i = 1:n, j = 1:n) %>%
  
  # each number is exactly once in a row
  add_constraint(sum_over(x[i, j, k], j = 1:n) == 3, i = 1:n, k = 1:2) %>%
  
  # each number is exactly once in a column
  add_constraint(sum_over(x[i, j, k], i = 1:n) == 3, j = 1:n, k = 1:2) %>% 
  
  # Prohibir tres números iguales consecutivos en filas
  add_constraint(
    x[i, j, k] + x[i, j + 1, k] + x[i, j + 2, k] <= 2, i = 1:n, j = 1:(n - 2), k = 1:2) |> 
  
  # Prohibir tres números iguales consecutivos en columnas
  add_constraint(
    x[i, j, k] + x[i + 1, j, k] + x[i + 2, j, k] <= 2, i = 1:(n - 2), j = 1:n, k = 1:2)



# Aplicar restricciones al modelo
for (row in seq_len(nrow(valores_fijos))) {
  i <- valores_fijos$eje_x[row]
  j <- valores_fijos$eje_y[row]
  k_fijo <- valores_fijos$valor[row]
  
  # Agrega la restricción: x[i, j, k_fijo] == 1
  model <- model %>%
    add_constraint(x[i, j, k_fijo] == 1)
}



library(purrr)

# Función general para aplicar restricciones entre pares
add_pair_constraints <- function(model, pairs, operator, n) {
  # Validar y aplicar por cada par
  walk(pairs, function(pair) {
    coords <- unlist(pair)
    if (any(coords < 1 | coords > n)) {
      stop("Coordenadas fuera del rango 1..n")
    }
    
    i1 <- pair[[1]][1]; j1 <- pair[[1]][2]
    i2 <- pair[[2]][1]; j2 <- pair[[2]][2]
    
    # Aplica la restricción para cada valor k
    for (k in 1:2) {
      if (operator == "!=") {
        model <<- model %>%
          add_constraint(x[i1, j1, k] + x[i2, j2, k] <= 1)
      } else if (operator == "==") {
        model <<- model %>%
          add_constraint(x[i1, j1, k] == x[i2, j2, k])
      } else {
        stop("Operador no soportado: usa '==' o '!='.")
      }
    }
  })
  
  return(model)
}

# Aplicar al modelo con validación
model <- model %>%
  add_pair_constraints(not_equal_pairs, "!=", n) %>%
  add_pair_constraints(equal_pairs, "==", n)


model

library(ompr.roi)
library(ROI.plugin.glpk)
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

optimo <- result %>% 
  get_solution(x[i,j,k]) %>%
  filter(value > 0) %>%  
  select(i, j, k) %>% 
  tidyr::spread(j, k) %>% 
  select(-i) |> 
  as.data.frame() |> 
  as.matrix()

optimo_1 <- optimo
optimo_2 <- optimo

optimo_1[optimo_1 == 1] <- "Moon"
optimo_1[optimo_1 == 2] <- "Sun"
optimo_1 <- matrix(optimo_1, nrow = 6, byrow = FALSE)

optimo_2[optimo_2 == 2] <- "Moon"
optimo_2[optimo_2 == 1] <- "Sun"
optimo_2 <- matrix(optimo_2, nrow = 6, byrow = FALSE)



n_rows <- 6
n_cols <- 6

special_cells <- final %>%
  filter(fijo == TRUE) %>%
  mutate(valor = ifelse(img %in% c("Moon", "Luna", "Sun", "Sol"), "#889fbf", NA_character_)) %>%
  filter(!is.na(valor)) %>%
  transmute(clave = paste0(eje_x, "_", eje_y), valor) %>%
  tibble::deframe()

generar_conectores <- function(df) {
  # Conectores horizontales
  horizontal_connectors <- df %>%
    filter(sentido_1 == "right", !is.na(signo_1)) %>%
    transmute(
      row = eje_x,
      col1 = eje_y,
      col2 = ny,
      symbol = ifelse(signo_1 %in% c("Equal","Igual"), "=", "X")
    ) %>%
    purrr::transpose()
  
  # Conectores verticales
  vertical_connectors <- df %>%
    filter(sentido_1 == "down", !is.na(signo_1)) %>%
    transmute(
      col = eje_y,
      row1 = eje_x,
      row2 = nx,
      symbol = ifelse(signo_1 %in% c("Equal","Igual"), "=", "X")
    ) %>%
    purrr::transpose()
  
  list(
    horizontal_connectors = horizontal_connectors,
    vertical_connectors = vertical_connectors
  )
}

# Suponiendo que tu tibble se llama `tabla`
conectores <- generar_conectores(restricciones)

horizontal_connectors <- conectores$horizontal_connectors
vertical_connectors <- conectores$vertical_connectors


# Función para detectar celdas a resaltar (3 iguales consecutivas)
find_matches <- function(mat) {
  to_highlight <- matrix(FALSE, nrow = n_rows, ncol = n_cols)
  
  # Horizontales
  for (i in 1:n_rows) {
    for (j in 1:(n_cols - 2)) {
      if (mat[i, j] != "" && mat[i, j] == mat[i, j+1] && mat[i, j] == mat[i, j+2]) {
        to_highlight[i, j:(j+2)] <- TRUE
      }
    }
  }
  
  # Verticales
  for (j in 1:n_cols) {
    for (i in 1:(n_rows - 2)) {
      if (mat[i, j] != "" && mat[i, j] == mat[i+1, j] && mat[i, j] == mat[i+2, j]) {
        to_highlight[i:(i+2), j] <- TRUE
      }
    }
  }
  
  to_highlight
}

# Detectar filas o columnas completas con 4 o más íconos iguales consecutivos
find_full_matches <- function(mat) {
  full_highlight <- matrix(FALSE, nrow = n_rows, ncol = n_cols)
  
  # Revisar filas
  for (i in 1:n_rows) {
    counts <- table(mat[i, mat[i, ] != ""])
    if (any(counts >= 4)) {
      full_highlight[i, ] <- TRUE
    }
  }
  
  # Revisar columnas
  for (j in 1:n_cols) {
    counts <- table(mat[mat[, j] != "", j])
    if (any(counts >= 4)) {
      full_highlight[, j] <- TRUE
    }
  }
  
  return(full_highlight)
}

find_connector_violations <- function(mat) {
  invalid <- matrix(FALSE, nrow = n_rows, ncol = n_cols)
  
  check_pair <- function(val1, val2, symbol) {
    if (val1 == "" || val2 == "") return(FALSE)
    if (symbol == "=") return(val1 != val2)
    if (symbol == "X" || symbol == "x") return(val1 == val2)
    return(FALSE)
  }
  
  # Revisa conectores horizontales
  for (conn in horizontal_connectors) {
    i <- conn$row
    j1 <- conn$col1
    j2 <- conn$col2
    if (check_pair(mat[i, j1], mat[i, j2], conn$symbol)) {
      invalid[i, j1] <- TRUE
      invalid[i, j2] <- TRUE
    }
  }
  
  # Revisa conectores verticales
  for (conn in vertical_connectors) {
    j <- conn$col
    i1 <- conn$row1
    i2 <- conn$row2
    if (check_pair(mat[i1, j], mat[i2, j], conn$symbol)) {
      invalid[i1, j] <- TRUE
      invalid[i2, j] <- TRUE
    }
  }
  
  return(invalid)
}


# Función para decidir el color de fondo
cell_color <- function(i, j, value) {
  key <- paste0(i, "_", j)
  if (key %in% names(special_cells)) {
    return(special_cells[[key]])
  }
  "#eff6fe"
}

# UI
ui <- fluidPage(
  
  useShinyjs(),
  tags$head(
    tags$script(HTML("
    var startTime = Date.now();
    var timer = setInterval(function(){
      var now = Date.now();
      var elapsed = Math.floor((now - startTime)/1000);
      var minutes = Math.floor(elapsed / 60);
      var seconds = elapsed % 60;
      document.getElementById('timer').innerText =
        (minutes < 10 ? '0' : '') + minutes + ':' +
        (seconds < 10 ? '0' : '') + seconds;
    }, 1000);

    Shiny.addCustomMessageHandler('stopTimer', function(message) {
      clearInterval(timer);
    });
  "))
  ),
  
  
  tags$head(
    tags$style(HTML("
    .game-title {
      font-family: 'Press Start 2P', monospace;
      font-size: 28px;
      text-align: center;
      padding: 20px 10px;
      color: #de6f41;
      background-color: #1e2c46;
      border-radius: 12px;
      margin-bottom: 20px;
      box-shadow: 0 4px 10px rgba(0,0,0,0.4);
      letter-spacing: 1px;
    }

    @import url('https://fonts.googleapis.com/css2?family=Press+Start+2P&display=swap');
  "))
  ),
  
  div(class = "game-title", "🕹️  Juego de Optimización"),
  
  
  setBackgroundColor(
    color = "ghostwhite",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE
  ),
  
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #9db4d3;
            color: #1e2c46;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
  )),
  
  
  
  tags$style(HTML("
  #sidebar {
    line-height: 1.3;
    font-size: 14px;
  }

  #sidebar ul {
    margin-top: 0;
    margin-bottom: 0.5em;
  }

  #sidebar li {
    margin-bottom: 0.2em;
  }

  #sidebar p {
    margin-bottom: 0.6em;
  }
")),
  
  # mainPanel(
  #   width = 6,
  #   tagList(
  #     uiOutput("board_ui"),
  #     br(),  # Espacio vertical
  #     div(style = "text-align: center; font-size: 18px; font-weight: bold; color: #1e2c46;",
  #         "⏱ Tiempo: ", span(id = "timer", "00:00")
  #     ),
  #     br(),
  #     actionButton("reset_board", "🔄 Reiniciar Tablero", class = "btn btn-primary")
  #     
  #   )
  # ),
  
  mainPanel(
    width = 6,
    tagList(
      div(style = "display: flex; justify-content: center;",
          uiOutput("board_ui")
      ),
      br(),  # Espacio vertical
      div(style = "text-align: center; font-size: 18px; font-weight: bold; color: #1e2c46;",
          "⏱ Tiempo: ", span(id = "timer", "00:00")
      ),
      br(),
      div(style = "text-align: center;",
          actionButton("reset_board", "🔄 Reiniciar Tablero", class = "btn btn-primary")
      )
    )
  ),
  
  
  sidebarPanel(
    width = 4,
    id = "sidebar",
    h3("¿Cómo se juega?"),
    HTML("
    <p>Tu misión es simple... o eso parece:</p>

    <ul>
      <li>Llena la cuadrícula usando solo <span style='font-size:20px;'><svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg>️</span> y <span style='font-size:20px;'><svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg>️</span>.</li>
      <li><b>¡Pero ojo!</b> No puede haber más de <b>2 triángulos o 2 circulos</b> consecutivos, ni en fila ni en columna. Nada de tríos.  <br><svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg><svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg><svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg> ❌     <br> <svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg><svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg> ✅ </li>
      <li>Cada fila y cada columna debe tener <b>la misma cantidad</b> de <span style='font-size:20px;'><svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg>️</span> y <span style='font-size:20px;'><svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg>️</span>. Equilibrio, ¿sabes?</li>
    </ul>

    <hr>

    <p>¿Ves símbolos como <b>=</b> o <b>x</b> entre algunas celdas?</p>
    <ul>
      <li><b>=</b> significa que esas dos celdas deben ser <b>iguales</b>.  <br>(<svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg>️ = <svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg>️ o <svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg>=<svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg>)</li>
      <li><b>x</b> significa que deben ser <b>diferentes</b>. (<svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg>️ ≠ <svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg>)</li>
    </ul>

    <hr>

    <p><b>Haz clic</b> en una celda para alternar entre <svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg>️,  <svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg> y vacía. ¡Experimenta!</p>
    <p>Cuando todo esté en su lugar... lo sabrás 😉</p>
    <p><i>¿Listo para descifrar el equilibrio del universo?</i></p>
  "),
    br(),
    
    tags$head(
      tags$style(HTML("
    #reset_board {
      color: #1e2c46;
      background-color: #de6f41;
      border: 2px solid #1e2c46;
      font-size: 16px;
      font-weight: bold;
      padding: 10px 20px;
      border-radius: 20px;
      transition: all 0.3s ease;
      display: block;
      margin: 20px auto 0 auto;
    }

    #reset_board:hover {
      color: #de6f41;
      background-color: #1e2c46;
      border-color: #de6f41;
      cursor: pointer;
    }
  "))
    )
  )
  
)


# Server
server <- function(input, output, session) {
  cell_states <- c("", "Moon", "Sun")
  
  
  session$onSessionEnded(function() {
    game_won(FALSE)  # reinicia al cerrar sesión
  })
  
  
  # Supongamos que la lista se llama 'lista'
  n_rows <- 6
  n_cols <- 6
  initial_matrix <- matrix("", nrow = n_rows, ncol = n_cols)
  
  # Nombres en inglés
  traducir_icono <- function(icono) {
    if (icono %in% c("Luna", "Moon")) return("Moon")
    if (icono %in% c("Sol", "Sun")) return("Sun")
    return("")
  }
  
  # Rellenar la matriz
  for (tbl in elementos) {
    for (i in seq_len(nrow(tbl))) {
      fila <- tbl$eje_x[i]
      columna <- tbl$eje_y[i]
      icono <- traducir_icono(tbl$img[i])
      initial_matrix[fila, columna] <- icono
    }
  }
  
  board <- reactiveVal(initial_matrix)
  
  observeEvent(input$reset_board, {
    board(initial_matrix)
  })
  
  output$board_ui <- renderUI({
    mat <- board()
    highlights <- find_matches(mat)
    full_highlights <- find_full_matches(mat)
    connector_violations <- find_connector_violations(mat)
    
    table_rows <- list()
    
    for (i in 1:n_rows) {
      cell_row <- list()
      
      for (j in 1:n_cols) {
        btn_id <- paste0("cell_", i, "_", j)
        value <- mat[i, j]
        
        value_icon <- switch(
          value,
          "Moon" = HTML('<svg width="32" height="32" viewBox="0 0 100 100"><polygon points="50,10 90,90 10,90" style="fill:#de6f41;" /></svg>'),
          "Sun"  = HTML('<svg width="32" height="32" viewBox="0 0 100 100"><circle cx="50" cy="50" r="42" style="fill:#1e2c46;" /></svg>'),
          value
        )
        
        # Nuevo color de fondo si hay coincidencia de 4 o más
        bg_color <- if (game_won()) {
          "#C8E6C9"  # Verde claro para victoria
        } else if (connector_violations[i, j]) {
          "#FFCDD2"  # Rojo claro
        } else if (full_highlights[i, j]) {
          "#FFE0B2"  # Naranja claro
        } else {
          cell_color(i, j, value)
        }
        
        
        
        border_color <- if (highlights[i, j]) "red" else "#889fbf"
        
        style <- paste0(
          "width: 40px; height: 40px; padding: 0; text-align: center;",
          "border: 3px solid ", border_color, "; background-color: ", bg_color, ";"
        )
        
        cell <- tags$td(actionButton(btn_id, label = value_icon, style = style), style = "padding:2px;")
        cell_row <- append(cell_row, list(cell))
        
        # Conector horizontal entre j y j+1
        if (j < n_cols) {
          conn <- Filter(function(x) x$row == i && 
                           ((x$col1 == j && x$col2 == j + 1) || 
                              (x$col2 == j && x$col1 == j + 1)), 
                         horizontal_connectors)
          if (length(conn) > 0) {
            symb <- conn[[1]]$symbol
            connector_cell <- tags$td(symb, style = "width: 15px; text-align: center; font-weight: bold; font-size: 15px; color: #de6f41;")
          } else {
            connector_cell <- tags$td(" ", style = "width: 15px;")
          }
          cell_row <- append(cell_row, list(connector_cell))
        }
      }
      
      table_rows <- append(table_rows, list(tags$tr(cell_row)))
      
      # Conector vertical entre filas
      if (i < n_rows) {
        conn_row <- list()
        for (j in 1:n_cols) {
          conn <- Filter(function(x) x$col == j && 
                           ((x$row1 == i && x$row2 == i + 1) || 
                              (x$row2 == i && x$row1 == i + 1)), 
                         vertical_connectors)
          if (length(conn) > 0) {
            symb <- conn[[1]]$symbol
            conn_cell <- tags$td(symb, style = "height: 15px; text-align: center; font-weight: bold; font-size: 15px; color: #de6f41;")
          } else {
            conn_cell <- tags$td(" ", style = "height: 15px;")
          }
          conn_row <- append(conn_row, list(conn_cell))
          
          if (j < n_cols) {
            conn_row <- append(conn_row, list(tags$td(" ")))
          }
        }
        table_rows <- append(table_rows, list(tags$tr(conn_row)))
      }
    }
    
    tags$table(
      style = "border-collapse: collapse;",
      table_rows
    )
  })
  
  game_won <- reactiveVal(FALSE)
  
  # Agrega esto antes de tu observe
  tags$style(HTML("
  .modal-dialog {
    width: 500px; /* Ancho personalizado */
  }
  .modal-content {
    background-color: #f9f9f9; /* Fondo gris claro */
  }
  .modal-title {
    text-align: center;
    color: #337ab7; /* Color azul para el título */
  }
  .modal-body {
    padding: 20px;
  }
  .modal-footer {
    text-align: center;
  }
"))
  
  
  
  observe({
    lapply(1:n_rows, function(i) {
      lapply(1:n_cols, function(j) {
        btn_id <- paste0("cell_", i, "_", j)
        observeEvent(input[[btn_id]], {
          
          # 🚫 Si ya ganaste, no permitir cambios
          if (game_won()) return()
          
          key <- paste0(i, "_", j)
          if (key %in% names(special_cells)) return()
          
          mat <- board()
          current_val <- mat[i, j]
          next_val <- cell_states[(match(current_val, cell_states, nomatch = 1) %% length(cell_states)) + 1]
          mat[i, j] <- next_val
          board(mat)
          
          # # ✅ Verifica si el nuevo tablero es una solución
          if (identical(mat, optimo_1) || identical(mat, optimo_2)) {
            game_won(TRUE)
            session$sendCustomMessage("stopTimer", list())
            
            
            showModal(modalDialog(
              title = HTML("<h2 style='text-align: center; background-color: #1e2c46; color: #de6f41;'>¡Te felicito! Lo lograste 🎉</h2>"),
              fluidPage(
                fluidRow(
                  column(12, 
                         p(style = "color: #1e2c46;", "¿Te atreves a mostrar tu resultado y descubrir dónde realmente estás en esta partida?"),
                         p(style = "color: #1e2c46;", "Ingresa tus datos y compite con los demás. Solo los mejores recibirán un regalo sorpresa…"),
                         p(style = "color: #de6f41;", em("algo que nadie espera, pero que todos querrán.")),
                         p(style = "color: #1e2c46;", "¿Prefieres mantener el bajo perfil? Sin problema, pero recuerda: lo misterioso siempre tiene su recompensa.")
                  )
                ),
                fluidRow(
                  column(12,
                         HTML("<input id='winner_email' type='email' placeholder='ejemplo@correo.com' style='width:100%; padding:10px; font-size:20px;'><br><span style='color: #1e2c46;'>Tu correo electrónico</span>")
                  )
                ),br(),
                # fluidRow(
                #   column(6, 
                #          HTML("<button id='submit_email' style='background-color: #de6f41; color: #1e2c46; padding:10px20px; border: none; border-radius:15px; cursor: pointer;'>Enviar</button>")),
                #   column(6,
                #          HTML("<button id='cancel' style='background-color: #889fbf; color: #1e2c46; padding:10px20px; border: none; border-radius:15px; cursor: pointer;'>Cancelar</button>")
                #   )
                # ),
              ),
              #              fluidRow(
              #                column(12,
              #                       HTML("<div style='text-align: center; margin-top: 20px;'>
              # <button id='submit_email' style='background-color: #de6f41; color: #1e2c46; padding: 10px 20px; border: none; border-radius:15px; cursor: pointer;'>
              # Enviar
              # </button>
              # <button id='cancel' style='background-color: #889fbf; color: #1e2c46; padding: 10px 20px; border: none; border-radius:15px; cursor: pointer;'>
              # Cancelar
              # </button>
              # </div>")
              #                )
              #              ),
              
              fluidRow(
                column(12,
                       div(style='text-align: center; margin-top: 20px;',
                           actionButton("submit_email", "Enviar", style = "background-color: #de6f41; color: #1e2c46; padding: 10px 20px; border: none; border-radius:15px; cursor: pointer;"),
                           actionButton("cancel", "Cancelar", style = "background-color: #889fbf; color: #1e2c46; padding: 10px 20px; border: none; border-radius:15px; cursor: pointer;")
                       )
                )
              ),
              
              easyClose = FALSE,
              #footer = "Sígueme en Instagram: @cchiquitovalencia"
              footer = HTML("Sígueme en <a href='https://www.instagram.com/cchiquitovalencia' target='_blank' style='color: #de6f41; text-decoration: none;'>@cchiquitovalencia</a>")
              
            ))
            
          }
          
          
        }, ignoreInit = TRUE)
      })
    })
  })
  
  observeEvent(input$submit_email, {
    email <- input$winner_email
    
    # Validación básica
    if (!is.null(email) && grepl(".+@.+\\..+", email)) {
      # Aquí podrías guardar el correo en una base de datos, archivo, etc.
      cat("Correo ingresado:", email, "\n")
      
      # Cerrar el modal y dar feedback
      removeModal()
      showModal(modalDialog(
        title = "¡Gracias!",
        p("Tu correo fue recibido con decisión. 🎯"),
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      showModal(modalDialog(
        title = "Correo no válido",
        p("Por favor ingresa un correo válido."),
        textInput("winner_email", "Tu correo electrónico:", value = email),
        actionButton("submit_email", "Enviar"),
        easyClose = FALSE,
        footer = NULL
      ))
    }
  })
  
  
}

shinyApp(ui, server)


