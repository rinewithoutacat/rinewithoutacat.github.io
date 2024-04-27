library(plotly)

# Create a sample Plotly graph
plot_data <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(10, 15, 13, 17, 10)
)

plot <- plot_ly(plot_data, x = ~x, y = ~y, type = 'scatter', mode = 'lines+markers')

# Save the Plotly widget as an HTML file
saveWidget(plot, file = "R/code/plotly_graph.html")
