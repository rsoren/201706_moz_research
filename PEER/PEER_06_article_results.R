#
# PEER_06_article_results.R
#
# Reed Sorensen
# October 2017
# 


library(dplyr)
library(forestplot)

df <- read.csv("PEER/PEER_stepped_wedge_oddsratios.csv")

df2 <- df %>%
  mutate(description2 = gsub(" - ", ", ", description))

# TODO: submit a bug report to 'forestplot' package authors
# -- the symbol size changes arbitrarily for each row
# -- have to change the source code like this to fix it:

draw_a_simple_mfing_circle <- function(lower_limit, estimate, upper_limit, size, y.offset = 0.5, 
  clr.line, clr.marker, lwd, lty = 1, vertices, vertices.height = 0.1,  ...) {
  if (is.na(lower_limit) || is.na(estimate) || is.na(upper_limit)) 
    return()
  forestplot:::prFpDrawLine(lower_limit = lower_limit, upper_limit = upper_limit, 
    clr.line = clr.line, lwd = lwd, lty = lty, y.offset = y.offset, 
    vertices = vertices, vertices.height = vertices.height)
  box <- convertX(unit(estimate, "native"), "npc", valueOnly = TRUE)
  if (box >= 0 && box <= 1) {
    if (is.unit(size)) {
      size <- convertUnit(size, unitTo = "mm", valueOnly = TRUE)
      size <- unit(size/2, "mm")
    }
    else {
      size <- unit(size/2, "snpc")
    }
    # grid.circle(x = unit(estimate, "native"), y = unit(y.offset, 
    #   "npc"), r = size, gp = gpar(fill = clr.marker, col = clr.marker))
    grid.circle(x = unit(estimate, "native"), y = unit(y.offset,
      "npc"),  r = 0.06,  gp = gpar(fill = clr.marker, col = clr.marker))
  }
}


jpeg(file = "PEER/PEER_forest_plot.jpg", width = 1600, height = 500)

txt_params <- fpTxtGp()
txt_params$label$cex <- 1.3
txt_params$ticks$cex <- 1.2

forestplot(
  labeltext = as.matrix(df2$description2),
  mean = as.matrix(df2[, c("odds_ratio", "lower_ci", "upper_ci")]),
  clip = c(0,3.5),
  zero = 1,
  cex = 3,
  lwd.ci = 3,
  graphwidth = unit(150, 'mm'),
  txt_gp = txt_params,
  vertices = TRUE,
  fn.ci_norm = draw_a_simple_mfing_circle
)

dev.off()


# cascade plot

jpeg(file = "PEER/PEER_cascade_plot.jpg")

colors1 <- c("orange2", "cornflowerblue")
dat <- rbind(c(1, 0.85, 0.7, 0.55), c(1, 0.7, 0.4, 0.1))

barplot(dat, beside = TRUE, col = colors1,
  main = "Just an example; see email",
  names.arg = c("CPP", "CCR", "Received result", "Started TARV"),
  ylab = "Proportion remaining"
)

legend("topright", legend = c("Intervention", "Control"), 
  fill = colors1)
dev.off()








