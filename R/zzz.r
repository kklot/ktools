.onLoad <- function(libname, pkgname) {
  okcol <<- c(
    base03 = "#002d38",
    base02 = "#093946",
    base01 = "#5b7279",
    base00 = "#657377",
    base0 = "#98a8a8",
    base1 = "#8faaab",
    base2 = "#f1e9d2",
    base3 = "#fbf7ef",
    yellow = "#ac8300",
    orange = "#d56500",
    red = "#f23749",
    magenta = "#dd459d",
    violet = "#7d80d1",
    blue = "#2b90d8",
    cyan = "#259d94",
    green = "#819500"
  )
  options(
    ggplot2.discrete.colour=ktools:::okabe,
    ggplot2.discrete.fill=ktools:::okabe
  )
  require('showtext')
  require('ggplot2')
  font_add_google('Roboto Slab', 'Roboto Slab')
  showtext_auto() 
  k.theme <<- theme_light() +
      theme(
          text = element_text(family = 'Roboto Slab', colour = okcol['base00']),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = okcol['base3']),
          panel.border = element_rect(linewidth = 0.1, fill = NA, colour = 'gray1'),
          strip.background = element_rect("white"),
          strip.text = element_text(color = "grey20", hjust = 0),
          legend.key = element_rect(fill = 'transparent'),
          legend.background = element_rect(fill = 'transparent'),
          legend.title = element_text(color = okcol['yellow']),
          plot.caption = element_text(color = okcol['yellow'])
      )
  require('here')
  require('dplyr')
  require('tidyr')
  require('stringr')
  require('sf')
  sf::sf_use_s2(FALSE)
  invisible()
}
