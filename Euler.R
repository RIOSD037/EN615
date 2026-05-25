setwd("//Users//dan.riosdoria//Documents//EN615")
list.files()

## Use Euler's method to solve dy/dt = t - y^2, y(0) = 1 from t=0 to t = 900 and plot t vs y

h  <- 0.05
t0 <- 0
tf <- 900

# number of steps
N <- as.integer((tf - t0)/h)

# Allocate vectors
t <- numeric(N+1)
y <- numeric(N+1)

# Initial conditions
t[1] <- t0
y[1] <- 1

# Euler iteration 

for (n in 1:N) {
  t[n+1] <- t[n] + h
  y[n+1] <- y[n] + h * (t[n] - y[n]^2)
}


# install.packages("ggplot2")  # if needed
library(ggplot2)

df <- data.frame(t = t, y = y)

p <- ggplot(df, aes(t, y)) +
  geom_line(linewidth = 0.3, color = "steelblue") +
  labs(
    title = "Euler: dy/dt = t - y^2,  y(0)=1  (h=0.05)",
    x = "t", y = "y"
  ) +
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

p

ggsave("euler_plot.png", plot = p, width = 8, height = 4, units = "in", dpi = 300)

