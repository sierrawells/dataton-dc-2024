#
# Autora: SW
# Mantenedoras: SW, AF, PB, IS
# Licencia:  Data Cívica 2024 ©
# ---------------------------------------------

# TODO

pacman::p_load(magick, add2ggplot, sysfonts, showtext)

tipo <- "Barlow Condensed Bold"
devices <- c(".png", ".svg")

# FONT PREP ====
showtext_auto(enable = TRUE, record = TRUE)
showtext_opts(dpi = 300)

font_dir <- file.path(Sys.getenv("HOME"), "/Downloads/")
font_add("Barlow", regular = paste0(font_dir, "Barlow/Barlow-Regular.ttf"))
font_add("Barlow Bold", regular = paste0(font_dir, "Barlow/Barlow-Bold.ttf"))
font_add("Barlow Condensed", regular = paste0(font_dir, "Barlow_Condensed/BarlowCondensed-Regular.ttf"))
font_add("Barlow Condensed Bold", regular = paste0(font_dir, "Barlow_Condensed/BarlowCondensed-Bold.ttf"))
font_add("Fraunces", regular = paste0(font_dir, "Fraunces/static/Fraunces_9pt-Bold.ttf"))

stopifnot(all(c("Barlow", "Barlow Condensed", "Fraunces", "Barlow Condensed Bold",
                "Barlow Bold") %in% font_families()))

# FUNCIONES ====
add_dclogo <- function(graf, escala){
  graf_con_logo <- add_logo(
    plot_path = graf,
    logo_path = here("descriptives/dc_logo.png"),
    logo_position = "bottom right",
    logo_scale = escala)
  
  magick::image_write(graf_con_logo, graf)
}

# PALETAS DE COLORES ====
pal <- c("#292bda", "#ff406e", "#fca311", "#7f71c4", "#90401d", "#92c0f7",
         "#040d5f", "#cecece")

grad <- c("#292bda", "#E5E6FB")

# COLORES CONTSTANTES ====
ejes <- "#c1c1c1"
texto <- "#29302C"
fondo <- "#faf5e9"

# TEMA PARA GRÁFICAS ====
tema <- theme(
  plot.title = element_text(family = "Fraunces",
                            color = texto,
                            face = "bold"),
  plot.subtitle = element_text(family = "Barlow",
                               color = texto),
  axis.text.x = element_text(family = "Barlow Condensed Bold",
                             color = texto,
                             face = "bold"),
  axis.text.y = element_text(family = "Barlow Condensed Bold",
                             color = texto,
                             face = "bold"),
  plot.caption = element_text(family = "Barlow Condensed",
                              color = texto,
                              hjust = 0),
  axis.title = element_text(family = "Barlow Bold",
                            color = texto),
  legend.title = element_text(family = "Barlow Bold",
                              color = texto, 
                              face = "bold"),
  legend.text = element_text(family = "Barlow Bold",
                             color = texto,
                             face = "bold"),
  strip.text = element_text(
    family = "Barlow Bold",
    color = texto,
    face = "bold"),
  axis.line = element_line(color = ejes),
  axis.ticks = element_line(color = ejes),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = ejes,
                                  linewidth = 0.1,
                                  linetype = "dashed"),
  panel.background = element_rect(fill = fondo),
  plot.background = element_rect(fill = fondo),
  legend.background = element_rect(fill = fondo))

# done.