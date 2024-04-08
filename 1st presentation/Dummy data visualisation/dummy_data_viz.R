# Test comment Ermi
# load libraries
library(tidyverse)
library(table1)

# Make plots -------------------------------------------------------------------

# Import the dummy data
DummyData <- read.csv("1st presentation/Dummy data visualisation/DummyData.csv",
                      quote = "", header = TRUE, sep = ";", check.names = FALSE)
# https://stackoverflow.com/questions/10441437/why-am-i-getting-x-in-my-column-names-when-reading-a-data-frame

# Remove quotes from the header row
removeQuotes <- function(x) gsub("\"", "", x)

names(DummyData) <- removeQuotes(names(DummyData))
# https://stackoverflow.com/questions/60080902/how-to-read-in-a-csv-file-in-r-without-the-quotation-marks

# check the normality of tot.vase.days per garden, species and compound

# histogram
DummyData %>%
  ggplot(aes(x = tot.vase.days)) +
  geom_histogram(binwidth = 1) +
  facet_grid(garden ~ species + compound) +
  theme_minimal()

# shapiro test
shapiro_p <- DummyData %>%
  group_by(garden, species, compound) %>%
  summarise(shapiro = shapiro.test(tot.vase.days)$p.value)

# Make overall boxplot of tot.vase.days per compound
DummyData %>%
  ggplot(aes(x = compound, y = tot.vase.days)) +
  geom_boxplot() +
  theme_minimal()

# Finetuning the overall box plot, highlighting group 15 which has the highest mean
Box_plot_total <- DummyData %>%
  mutate(type = ifelse(compound == 15,"Highlighted","Normal")) %>%
  ggplot(aes(x = factor(compound), y = tot.vase.days, fill = type, alpha = type)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#440154", "grey")) +
  scale_alpha_manual(values = c(0.8, 0.1)) +
  theme_minimal() +
  xlab("Compound") + 
  scale_y_continuous(limits = c(3, 30), breaks = seq(3, 30, by = 5), expand = c(0,0)) +
  ylab("Total vase days") +
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = "bold"),
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.5) 

# boxplot by species
Box_plot_by_species <- DummyData %>%
  ggplot(aes(x = factor(compound), y = tot.vase.days, fill = factor(species))) +
  scale_fill_viridis_d(alpha = 0.6) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(pch = 21, position = position_jitterdodge(), alpha = 0.3, size = 0.5) +
  theme_minimal() +
  xlab("Compound") + 
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5), expand = c(0,0)) +
  ylab("Total vase days") +
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = "bold"),
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6)) +
  labs(fill = "Species")  

# boxplot by garden
Box_plot_by_garden <- DummyData %>%
  ggplot(aes(x = factor(compound), y = tot.vase.days, fill = factor(garden))) +
  scale_fill_viridis_d(alpha = 0.6) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(pch = 21, position = position_jitterdodge(), alpha = 0.3, size = 0.5) +
  theme_minimal() +
  xlab("Compound") + 
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5), expand = c(0,0)) +
  ylab("Total vase days") +
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = "bold"),
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6)) +
  labs(fill = "Garden")

# Save the plots
ggsave("Box_plot_total.jpeg", Box_plot_total, width = 18, height = 10, dpi = 300, unit = "cm")
ggsave("Box_plot_by_species.jpeg", Box_plot_by_species, width = 18, height = 10.13, dpi = 300, unit = "cm")
ggsave("Box_plot_by_garden.jpeg", Box_plot_by_garden, width = 18, height = 10.13, dpi = 300, unit = "cm")

# Make a table -----------------------------------------------------------------

# Convert compound, species and garden to factors
DummyData$compound <- as.factor(DummyData$compound)
DummyData$species <- as.factor(DummyData$species)
DummyData$garden <- as.factor(DummyData$garden)

# Group DummyData by species and garden, summarise mean and sd of tot.vase.days by compound
table_gar_spe <- DummyData %>%
  group_by(species, garden, compound) %>%
  summarise(mean = mean(tot.vase.days), sd = sd(tot.vase.days))

# Stratified by species only
table_spe <- DummyData %>%
  group_by(species, compound) %>%
  summarise(mean = mean(tot.vase.days), sd = sd(tot.vase.days))

# Stratified by garden only
table_gar <- DummyData %>%
  group_by(garden, compound) %>%
  summarise(mean = mean(tot.vase.days), sd = sd(tot.vase.days))

# Overall table
table_overall <- DummyData %>%
  group_by(compound) %>%
  summarise(mean = mean(tot.vase.days), sd = sd(tot.vase.days))

