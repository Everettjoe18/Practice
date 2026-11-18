# Monte Carlo Integration of the Normal Density ----
## Coder: Joe Everett
## Estimate the area under the standard normal curve using random points.

# Step 1: Load Packages ----
## Needed Packages: tidyverse
library(tidyverse)

# Step 2: Write the Simulation Function ----
simulatePoints <- function(n, xMin, xMax, yMin, yMax) {
  tibble(
    x = runif(n = n, min = xMin, max = xMax), # Random x positions in the window
    y = runif(n = n, min = yMin, max = yMax) # Random y positions in the window
  )
}

# Step 3: Test the Function ----
Test_Points <- simulatePoints(n = 10, xMin = 2, xMax = 6, yMin = 0, yMax = 4)
nrow(Test_Points) == 10 # Should be true

# Step 4: Generate Points Around the Normal Curve ----
Points_1000 <- simulatePoints(n = 1000, xMin = -4, xMax = 4, yMin = 0, yMax = 0.4)

# Step 5: Flag Points Above or Below the Curve ----
Points_Flagged <- Points_1000 |>
  mutate( # Compare each point to the height of the curve
    flag = case_when(
      y <= dnorm(x = x) ~ "on/below",
      y > dnorm(x = x) ~ "above"
    )
  )

View(Points_Flagged)
str(Points_Flagged)

# Step 6: Estimate the Integral ----
windowArea <- (4 - (-4)) * (0.4 - 0) # Width times height of the window
propBelow <- mean(Points_Flagged$flag == "on/below") # Share of points under the curve
estimateIntegral <- propBelow * windowArea
estimateIntegral # True value is around 1

# Step 7: Build the Normal Curve for Plotting ----
Curve_Normal <- data.frame(x = seq(from = -4, to = 4, by = 0.01)) |>
  mutate(y = dnorm(x = x)) # Height of the curve at each x

# Step 8: Plot the Points and the Curve ----
ggplot(
  data = Points_Flagged,
  mapping = aes(x = x, y = y)
) +
  geom_point(
    mapping = aes(color = flag, shape = flag),
    alpha = 0.6
  ) +
  geom_line(
    data = Curve_Normal,
    color = "#1E407C",
    linewidth = 1
  ) +
  scale_color_manual(
    values = c("#BC204B", "#3EA39E")
  ) +
  labs(
    title = "Monte Carlo Integration of the Normal Density, n=1000",
    x = "x",
    y = "y",
    color = "Flag",
    shape = "Flag",
    caption = paste("Est. Numerical Integration:", round(estimateIntegral, 4))
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )