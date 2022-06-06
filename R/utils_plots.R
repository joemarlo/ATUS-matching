require(viridis)

# build custom theme
theme_custom <- function() {
  ggplot2::theme_gray() +
    ggplot2::theme(
      panel.grid.minor.y = ggplot2::element_line(color = NA),
      panel.grid.major.y = ggplot2::element_line(color = "gray95"),
      panel.grid.minor.x = ggplot2::element_line(color = NA),
      panel.grid.major.x = ggplot2::element_line(color = "gray95"),
      panel.background = ggplot2::element_rect(fill = NA),
      plot.background = ggplot2::element_rect(
        fill = 'white',
        color = "gray95",
        size = 10
      ),
      plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm"),
      axis.title = ggplot2::element_text(color = "gray30"),
      axis.ticks = ggplot2::element_line(color = NA),
      strip.background = ggplot2::element_rect(fill = "gray95"),
      strip.text = ggplot2::element_text(
        color = "gray30",
        size = 10,
        face = "bold"
      ),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      text = ggplot2::element_text(family = "Helvetica",
                          color = "gray30"),
      plot.caption = ggplot2::element_text(face = "italic",
                                  size = 6,
                                  color = 'grey50'),
      legend.key = ggplot2::element_rect(fill = NA)
    )
}

# set custom theme
# ggplot2::theme_set(theme_custom())
ggplot2::theme_set(theme_bw())

# set default continuous colors
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

# set default discrete colors
scale_colour_discrete <- function(...) {
  scale_color_viridis(..., discrete = TRUE)
}
scale_color_discrete <- function(...) {
  scale_color_viridis(..., discrete = TRUE)
}
scale_fill_discrete <- function(...) {
  scale_fill_viridis(..., discrete = TRUE)
}
