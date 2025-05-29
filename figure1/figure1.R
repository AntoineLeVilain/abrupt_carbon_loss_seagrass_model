setwd("/figure1")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# Load model output data
data <- read.csv("ma_sh.csv")

### FIGURE 1a: Abrupt tipping response ###
# Panel (a) uses real model output for a case where the system shows a tipping point.

# Extract seagrass and bistable equilibria for a specific SH value
tp_seagrass <- data %>% 
  filter(stability == "seagrass" | stability == "bistable") %>%
  filter(sh == 22.7) %>%
  filter(ma >= 0.001 & ma <= 0.004)

# Extract bare and bistable equilibria for the same SH value
tp_bare <- data %>% 
  filter(stability == "bare" | stability == "bistable") %>%
  filter(sh == 22.7) %>%
  filter(ma >= 0.001 & ma <= 0.004)

# Plotting
tp_plot <- ggplot() + 
  geom_line(data = tp_seagrass, aes(x = ma, y = S_seagrass),
            color = "darkgreen", size = 1, linetype = "solid") +
  geom_line(data = tp_seagrass, aes(x = ma, y = S_bistable),
            color = "darkgreen", size = 1, linetype = "dashed") +
  geom_line(data = tp_bare, aes(x = ma, y = S_bare),
            color = "darkgreen", size = 1, linetype = "solid") +
  xlab("ma") + ylab("S") +
  theme_classic() + theme(legend.position = "none")

### FIGURE 1b: Abrupt non-tipping response ###
# Panel (b) uses real model output for a case with **no tipping point**,

abrupt <- data %>%
  filter(sh == 10000) %>%  # Very weak feedback
  filter(ma <= 0.003)

# Fill missing seagrass values with 0 for bare state representation
abrupt$S_seagrass[is.na(abrupt$S_seagrass)] <- 0

abrupt_plot <- ggplot() + 
  geom_line(data = abrupt, aes(x = ma, y = S_seagrass),
            color = "darkgreen", size = 1, linetype = "solid") +
  xlab("ma") + ylab("S") +
  theme_classic() + theme(legend.position = "none")

### FIGURE 1c: Gradual non-tipping response ###
# Panel (c) is mock data representing a gradual, continuous decline

# Create artificial data
x <- seq(0, 9, by = 0.1)
y <- ifelse(x <= 5, -2 * x + 10, 0)  # Linear drop then flatten

dummy_data <- data.frame(x = x, y = y)

# Plot mock gradual transition
linear_plot <- ggplot(dummy_data, aes(x = x, y = y)) +
  geom_line(size = 1, color = "darkgreen") +
  xlab("ma") + ylab("S") +
  theme_classic()

### COMBINE & EXPORT ###
# Combine all three plots side by side: (a) tipping, (b) abrupt non-tipping, (c) gradual
plot <- grid.arrange(tp_plot, abrupt_plot, linear_plot, ncol = 3)

# Save figure as PNG
ggsave(
  "figure1.png",
  plot,
  width = 17,
  height = 4.66,
  dpi = 300
)
