install.packages("tidyverse")
install.packages("mosaic")
install.packages("usethis") 
install.packages("lmodel2") 
install.packages("sjPlot") 
install.packages("broom") 
install.packages("manipulate") 
install.packages("patchwork") 
install.packages("infer")
install.packages("latticeExtra")
library(lmodel2) 
library(skimr)
library(latticeExtra)
library(sjPlot) 
library(broom) 
library(tidyverse) 
library(mosaic)
library(manipulate) 
library(patchwork) 
library(infer) 
library(usethis)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/AVONETdataset1.csv"
d <- read.csv(f)
d <- d |> select(Species1, Family1, Order1, Beak.Length_Culmen, Beak.Width, Beak.Depth, Tarsus.Length, Wing.Length, Tail.Length, Mass, Habitat, Migration, Trophic.Level, Trophic.Niche, Min.Latitude, Max.Latitude, Centroid.Latitude, Range.Size, Primary.Lifestyle)
skim(d)
d <- d |> mutate(logmass = log(Mass), logrange = log(Range.Size), logbeak = log(Beak.Length_Culmen), logtarsus = log(Tarsus.Length), migration = as.factor(Migration))
d <- (drop_na(d))
plot_trop_logmass <- ggplot(data = d, aes(x = Trophic.Level, y = logmass)) +
                              geom_boxplot() +
                              geom_jitter(alpha = 0.05)
plot_mig_logmass <- ggplot(data = d, aes(x = Migration, y = logmass)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.05)
print(plot_trop_logmass)
print(plot_mig_logmass)
lm1 <- lm(logmass ~ Trophic.Level, data = d)
lm2 <- lm(logmass ~ Migration, data = d)
summary(lm1)
summary(lm2)
d <- d |> mutate(Migration = relevel(factor(Migration), ref = "3"))
lm2relevel <- lm(logmass ~ Migration, data = d)
summary(lm2relevel)
base_F <- aov(logmass ~ Trophic.Level, data = d) |>
  tidy() |>
  filter(term == "Trophic.Level") |>
  pull(statistic)
permuteF <- d |>
  specify(logmass ~ Trophic.Level) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "F")
p.value <- permuteF |>
  get_p_value(obs_stat = base_F, direction = "greater")
plot_trop_beak <- ggplot(data = d, aes(x=Trophic.Niche, y=relatebeak)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.05) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
plot(plot_trop_beak)
plot(plot_life_tars)
histogram(d$Range.Size)
histogram(d$logrange)
ggplot(data = d, aes(x = Migration, y = logrange)) +
  geom_violin() +
  geom_jitter(alpha = 0.05, width = 0.5)
plot(passplot1)
plot(passplot2)
plot(passplot3)
plot(passplot4)
m4 <- aov(relatebeak ~ Primary.Lifestyle, data = passeriforme)
m5 <- aov(relatebeak ~ Trophic.Level, data = passeriforme)
summary(m4)
summary(m5)
