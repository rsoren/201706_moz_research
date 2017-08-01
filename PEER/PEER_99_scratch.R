

# jpeg(filename = "my_plot.jpg")

peer_data <- cbind(
  c(85, 75), 
  c(65, 85),
  c(96, 75), 
  c(85, 85)
)

colnames(peer_data) <- c(
  "Dondo", 
  "1 de Mayo",
  "Munhava",
  "Macurungu"
)

barplot(
  height = peer_data,
  beside = TRUE
)

legend(
  "topright",
  legend = c("Baseline", "Cohort 1"),
  col = gray.colors(2),
  pch = 20
)

abline(v = 200)


# dev.off()




jpeg("my_legend.jpg")

plot.new()

legend(
  x = "topright", 
  legend = c(
    "Variable description 1",
    "Variable description 2",
    "Variable description 3" ),
  col = gray.colors(3),
  pch = 20
)

dev.off()



legend(
  x = 200,
  legend = c(
    "Variable description 1",
    "Variable description 2",
    "Variable description 3" ),
  col = gray.colors(3),
  pch = 20,
  xpd = NA
)



# Facility name under each group
# Intervention or Control under each section of groups
# Legend describing what color represents what variable
