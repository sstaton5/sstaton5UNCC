# Dependent Variable Simulation 

```r
# Load libraries
library(ggplot2)

# 1. Read and prepare data (adjust path)
df <- read.csv("path/to/dv_socioeconomic_dataset_clipped.csv")
dv <- as.numeric(df$Domestic_violence_incide)
if (length(dv) == 0) stop("DVIncidents column not found or has no data")

n_obs <- length(dv)

# 2. Observed summary statistics
observed_mean   <- mean(dv, na.rm = TRUE)
observed_median <- median(dv, na.rm = TRUE)
cat(sprintf("Observed Mean: %.2f\n", observed_mean))
cat(sprintf("Observed Median: %d\n", observed_median))

# 3. Theoretical Poisson simulation (λ = observed mean)
lambda <- observed_mean
set.seed(42)
simulated <- replicate(1000, rpois(n_obs, lambda))

# Sampling distributions
samp_medians <- apply(simulated, 2, median)
samp_means   <- apply(simulated, 2, mean)

# 4. Bootstrap sampling distributions
set.seed(42)
boot_medians <- numeric(1000)
boot_means   <- numeric(1000)
for (i in seq_len(1000)) {
  samp_i <- sample(dv, n_obs, replace = TRUE)
  boot_medians[i] <- median(samp_i, na.rm = TRUE)
  boot_means[i]   <- mean(samp_i, na.rm = TRUE)
}

# 5. Plot sampling distribution of the median
p1 <- ggplot(data.frame(median = samp_medians), aes(median)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Sampling Distribution of the Median",
       x = "Median DVIncidents", y = "Frequency") +
  theme_minimal()
print(p1)

# 6. Plot sampling distribution of the mean
p2 <- ggplot(data.frame(mean = samp_means), aes(mean)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Sampling Distribution of the Mean",
       x = "Mean DVIncidents", y = "Frequency") +
  theme_minimal()
print(p2)
```

---

*Ensure the CSV path is correct and `ggplot2` is installed (`install.packages("ggplot2")`).*
