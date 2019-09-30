# Had to install plot_ly from development
# devtools::install_github("ropensci/plotly")


library(tidyverse)
library(plotly)
library(moderndive)


# Preprocess data ---------------------------------------------------------
set.seed(76)
data(house_prices)

# Re-scale price and size (square footage) by log10 and take a subsample of points
house_prices <- house_prices %>%
  mutate(
    log10_price = log10(price),
    log10_size = log10(sqft_living)
  ) %>%
  sample_n(500)


# Define 3D scatterplot points --------------------------------------------
# Get coordinates of points for 3D scatterplot
x_values <- house_prices$log10_size %>% 
  round(3)
y_values <- house_prices$yr_built %>% 
  round(3)
z_values <- house_prices$log10_price %>% 
  round(3)


# Define regression plane -------------------------------------------------
# Construct x and y grid elements
x_grid <- seq(from = min(x_values), to = max(x_values), length = 50)
y_grid <- seq(from = min(y_values), to = max(y_values), length = 50)

# Construct z grid by computing
# 1) fitted beta coefficients
# 2) fitted values of outer product of x_grid and y_grid
# 3) extracting z_grid (matrix needs to be of specific dimensions)
beta_hat <- house_prices %>% 
  lm(log10_price ~ log10_size + yr_built, data = .) %>% 
  coef()
fitted_values <- crossing(y_grid, x_grid) %>% 
  mutate(z_grid = beta_hat[1] + beta_hat[2]*x_grid + beta_hat[3]*y_grid)
z_grid <- fitted_values %>% 
  pull(z_grid) %>%
  matrix(nrow = length(x_grid)) %>%
  t()

# Define text element for each point in plane
text_grid <- fitted_values %>% 
  pull(z_grid) %>%
  round(3) %>% 
  as.character() %>% 
  paste("log10(price): ", ., sep = "") %>% 
  matrix(nrow = length(x_grid)) %>%
  t()


# Plot using plotly -------------------------------------------------------
plot_ly() %>%
  # 3D scatterplot:
  add_markers(
    x = x_values,
    y = y_values,
    z = z_values,
    marker = list(size = 5),
    hoverinfo = 'text',
    text = ~paste(
      "log10(price): ", z_values, "<br>",
      "year: ", y_values, "<br>",
      "log10(size): ", x_values 
    )
  ) %>%
  # Regression plane:
  add_surface(
    x = x_grid,
    y = y_grid,
    z = z_grid,
    hoverinfo = 'text',
    text = text_grid
  ) %>%
  # Axes labels and title:
  layout(
    title = "3D scatterplot and regression plane",
    scene = list(
      zaxis = list(title = "y: log10(price)"),
      yaxis = list(title = "x2: year"),
      xaxis = list(title = "x1: log10(size)")
    )
  )
