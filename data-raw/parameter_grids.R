library(tidyverse)
set.seed(2022)
# http://gli-calculator.ersnet.org/docs.html


# PFT Testing Grid --------------------------------------------------------
sex <- c("M", "F")
age <- seq(5, 90, 5.2)
height <- seq(50, 230, 20)
race <- c("Caucasian", "AfrAm", "NEAsia", "SEAsia", "Other/mixed")
spirometry_grid <- expand.grid(sex = sex,
                               race = race,
                               age = age,
                               height = height) %>%
  dplyr::slice_sample(n = 1000)

## Recode race and rename columns for online GLI calculator
grid_gli_calculator <- spirometry_grid %>%
  mutate(race = recode(race,
                       Caucasian = "1",
                       AfrAm = "2",
                       NEAsia = "3",
                       SEAsia = "4",
                       `Other/mixed` = "5")) %>%
  rename(ethnic = race)
#write_csv(grid_gli_calculator, "data-raw/gli_test_grid.csv")


# Evaluate GLI vs. Package Predicted Values -------------------------------
grid_gli_calculator <- read_csv("data-raw/gli_test_grid.csv") %>%
  mutate(race = recode(ethnic,
                       `1` = "Caucasian",
                       `2` = "AfrAm",
                       `3` = "NEAsia",
                       `4` = "SEAsia",
                       `5` = "Other/mixed"))

grid_spiro <- grid_gli_calculator %>%
  select(sex, age, height, race)

grid_volumes_dlco <- grid_gli_calculator %>%
  select(sex, age, height)

## Correct AfrAm Women Spirometry predicteds for testing
gli_calculator_output <- read_csv("data-raw/gli_test_grid_GLI.csv")

## Generate predicted values from package.
spirometry_package_output <- spirometry_normals(grid_spiro)
volumes_package_output <- volume_normals(grid_volumes_dlco)
dlco_package_output <- diffusion_normals(grid_volumes_dlco)
dlco_package_output2 <- diffusion_normals(grid_volumes_dlco, SI.units = TRUE)

## Correct AfrAM Women FEV1/FVC spirometry predicteds for testing
gli_calculator_output2 <- gli_calculator_output %>%
  mutate(fev1fvc_lln_new = round(spirometry_package_output$fev1fvc_lln, digits = 3),
         fev1fvc_uln_new = round(spirometry_package_output$fev1fvc_uln, digits = 3),
         fev1fvc_lln = ifelse(ethnic != 2, fev1fvc_lln, fev1fvc_lln_new),
         fev1fvc_uln = ifelse(ethnic != 2, fev1fvc_uln, fev1fvc_uln_new)) %>%
  select(-fev1fvc_lln_new, fev1fvc_uln_new)

write_csv(gli_calculator_output2, "data-raw/gli_test_grid_GLI.csv")

## TODO: Align package and GLI columns for comparison.

### Spirometry
spiro_pkg_long <- spirometry_package_output %>%
  mutate(rank = 1:n()) %>%
  tidyr::pivot_longer(c(-rank, -sex, -age, -height, -race), names_to = "variable", values_to = "pkg")

spiro_gli_long <- gli_calculator_output %>%
  mutate(rank = 1:n()) %>%
  select(rank, starts_with("fev1"), starts_with("fvc"), starts_with("fef2575"), starts_with("fef75")) %>%
  tidyr::pivot_longer(-rank, names_to = "variable", values_to = "gli")

spiro_long <- spiro_pkg_long %>%
  left_join(spiro_gli_long, by = c("rank","variable")) %>%
  mutate(diff = pkg - gli)

spiro_long %>%
  ggplot(aes(x = diff)) +
  geom_histogram() +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

spiro_long %>%
  ggplot(aes(x = pkg, y = gli)) +
  geom_point(alpha = 0.4) +
  geom_abline(linetype = "dashed") +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

spiro_discrepancies <- spiro_long %>%
  filter(variable == "fev1fvc_lln",
         (diff < -0.004) | (diff > 0.0025))

### Lung Volumes
volume_pkg_long <- volumes_package_output %>%
  mutate(rank = 1:n()) %>%
  tidyr::pivot_longer(c(-rank, -sex, -age, -height), names_to = "variable", values_to = "pkg")

volume_gli_long <- gli_calculator_output %>%
  mutate(rank = 1:n()) %>%
  select(rank, starts_with("frc"), starts_with("tlc"), starts_with("rv"), starts_with("erv"), starts_with("ic"), starts_with("vc")) %>%
  select(-starts_with("tlco")) %>%
  rename(rv_tlc_lln = rvtlc_lln,
         rv_tlc_uln = rvtlc_uln) %>%
  tidyr::pivot_longer(-rank, names_to = "variable", values_to = "gli")

volume_long <- volume_pkg_long %>%
  left_join(volume_gli_long, by = c("rank","variable")) %>%
  mutate(diff = pkg - gli)

volume_long %>%
  ggplot(aes(x = diff)) +
  geom_histogram() +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

volume_long %>%
  ggplot(aes(x = pkg, y = gli)) +
  geom_point(alpha = 0.4) +
  geom_abline(linetype = "dashed") +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

### DLCO
dlco_pkg_long <- dlco_package_output %>%
  mutate(rank = 1:n()) %>%
  tidyr::pivot_longer(c(-rank, -sex, -age, -height), names_to = "variable", values_to = "pkg")

dlco_gli_long <- gli_calculator_output %>%
  mutate(rank = 1:n()) %>%
  select(rank, starts_with("dlco"), starts_with("tlco"), starts_with("kco"), starts_with("va")) %>%
  rename(kco_si_lln = kcosi_lln,
         kco_si_uln = kcosi_uln,
         kco_tr_lln = kcotr_lln,
         kco_tr_uln = kcotr_uln) %>%
  tidyr::pivot_longer(-rank, names_to = "variable", values_to = "gli")

dlco_long <- dlco_pkg_long %>%
  left_join(dlco_gli_long, by = c("rank","variable")) %>%
  mutate(diff = pkg - gli)

dlco_discrepancies <- dlco_long %>%
  filter(is.na(pkg) | is.na(gli))

## TODO: Explore any discrepancies discovered.




