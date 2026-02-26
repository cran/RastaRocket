## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning=FALSE,
  message=FALSE,
  results='asis'
)

## ----setup--------------------------------------------------------------------
library(RastaRocket)
library(dplyr)
library(tidyr)
library(labelled)
library(rlang)
library(gtsummary)
library(forcats)

## -----------------------------------------------------------------------------
# Charger le package nécessaire
set.seed(123)  # Pour garantir la reproductibilité

# Création du data frame
data <- data.frame(
  Age = c(rnorm(45, mean = 50, sd = 10), rep(NA, 5)),  # Renommée Age
  sexe = sample(c(0, 1), 50, replace = TRUE, prob = c(0.6, 0.4)),  # Renommée sexe
  quatre_modalites = sample(c("A", "B", "C"), 50, replace = TRUE, prob = c(0.2, 0.5, 0.3)),  # Modalités sans "D"
  traitement = sample(c("BRAS-A", "BRAS-B"), 50, replace = TRUE, prob = c(0.55, 0.45)),  # Nouvelle variable traitement
  echelle = sample(0:5, 50, replace = TRUE)  # Nouvelle variable entière de 0 à 5
)

# Ajouter la modalité "D" comme niveau sans effectif
data$quatre_modalites <- factor(data$quatre_modalites, levels = c("A", "B", "C", "D"))

# Ajouter des labels à la variable sexe
data$sexe <- factor(data$sexe, levels = c(0, 1), labels = c("Femme", "Homme"))

# Aperçu des données



data <- data %>% labelled::set_variable_labels( Age = "Age",
                                                sexe = "sexe",
                                                traitement  = "traitement",
                                                quatre_modalites = "quatres niveaux",
                                                echelle = "Echelle")

## -----------------------------------------------------------------------------
data %>% RastaRocket::desc_var(table_title = "test",
                            by_group = TRUE,
                            var_group = "traitement",
                            group_title = "Traitement",
                            add_total = TRUE,
                            show_n_per_group = TRUE)

## -----------------------------------------------------------------------------
data %>%
  dplyr::select(Age, traitement) %>%
  dplyr::mutate(Age = round(Age)) %>%
  RastaRocket::desc_var(table_title = "test",
                     by_group = TRUE,
                     var_group = "traitement",
                     group_title = "traitement",
                     quali = c("Age"))

## -----------------------------------------------------------------------------
iris %>% RastaRocket::desc_var(table_title = "test",
                            by_group = TRUE,
                            var_group = "Species",
                            group_title = "Species",
                            show_missing_data = TRUE)

## -----------------------------------------------------------------------------
iris %>% RastaRocket::desc_var(table_title = "test",
                            by_group = TRUE,
                            var_group = "Species",
                            group_title = "Species",
                            show_missing_data = FALSE)

## ----second example-----------------------------------------------------------
data %>% desc_var(table_title = "test",
             by_group = TRUE,
             var_group = "traitement",
             group_title = "traitement",
             freq_relevel = TRUE)

## -----------------------------------------------------------------------------
data %>%
  dplyr::mutate(quatre_modalites = forcats::fct_relevel(quatre_modalites,
                                                       "A", "C", "D", "B")) %>%
  desc_var(table_title = "test",
           by_group = TRUE,
           var_group = "traitement",
           group_title = "traitement")

## ----third example------------------------------------------------------------
data %>% desc_var(table_title = "test",
             by_group = TRUE,
             var_group = "traitement",
             group_title = "traitement",
             drop_levels = FALSE)

## -----------------------------------------------------------------------------
data %>% desc_var(table_title = "test",
             by_group = TRUE,
             var_group = "traitement",
             group_title = "traitement")

## -----------------------------------------------------------------------------
data %>% RastaRocket::desc_var(table_title = "test",
             by_group = FALSE,
             var_group = "traitement",
             group_title = "traitement")

## -----------------------------------------------------------------------------
tb1 <- data %>%
  dplyr::select(Age, sexe) %>%
  RastaRocket::desc_var(table_title = "test")

tb2 <- data %>%
  dplyr::select(quatre_modalites) %>%
  RastaRocket::desc_var(table_title = "test")

RastaRocket::intermediate_header(tbls = list(tb1, tb2),
                              group_header = c("Title A", "Title B"))


## -----------------------------------------------------------------------------
data %>%
  RastaRocket::desc_var(table_title = "test",
                     by_group = TRUE,
                     var_group = "traitement",
                     digits = list(r_quanti = 0, r_quali = 1))

## -----------------------------------------------------------------------------
tb1 <- data %>%
  dplyr::select(Age, sexe, traitement) %>%
  RastaRocket::desc_var(table_title = "test",
                     by_group = TRUE,
                     var_group = "traitement",
                     digits = list(r_quanti = 2, r_quali = 2))

tb2 <- data %>%
  dplyr::select(quatre_modalites, traitement) %>%
  RastaRocket::desc_var(table_title = "test",
                     by_group = TRUE,
                     var_group = "traitement",
                     digits = list(r_quanti = 0, r_quali = 1))

gtsummary::tbl_stack(list(tb1, tb2))

## -----------------------------------------------------------------------------
data %>%
  RastaRocket::desc_var(table_title = "test",
                     by_group = TRUE,
                     var_group = "traitement",
                     tests = TRUE)

## -----------------------------------------------------------------------------
data %>%
  RastaRocket::desc_var(table_title = "test",
                     by_group = TRUE,
                     var_group = "traitement",
                     tests = list(Age = "t.test",
                                  sexe = "chisq.test",
                                  echelle = "fisher.test"))

## -----------------------------------------------------------------------------
data %>%
  RastaRocket::desc_var(table_title = "test",
                     by_group = TRUE,
                     var_group = "traitement",
                     tests = list(Age = "t.test",
                                  sexe = "chisq.test",
                                  echelle = "fisher.test")) %>%
  custom_format()

## -----------------------------------------------------------------------------
tb1 <- data %>%
  dplyr::select(Age, sexe, traitement) %>%
  RastaRocket::desc_var(table_title = "test",
                     by_group = TRUE,
                     var_group = "traitement",
                     digits = list(r_quanti = 0, r_quali = 0))

tb2 <- data %>%
  dplyr::select(quatre_modalites, traitement) %>%
  RastaRocket::desc_var(table_title = "test",
                     by_group = TRUE,
                     var_group = "traitement",
                     digits = list(r_quanti = 0, r_quali = 1))

gtsummary::tbl_stack(list(tb1, tb2)) %>%
  custom_format()

## -----------------------------------------------------------------------------
data %>%
  RastaRocket::desc_var(table_title = "test",
                     by_group = TRUE,
                     var_group = "traitement") %>%
  custom_format(align = "left",
                column_size = list(label ~ gt::pct(50),
                                   gt::starts_with("stat") ~ gt::pct(25)))

## -----------------------------------------------------------------------------
# reset theme to default
gtsummary::reset_gtsummary_theme()
# switch to French format
gtsummary::theme_gtsummary_language(language = "fr", decimal.mark = ",", big.mark = " ")

iris %>%
  RastaRocket::desc_var(table_title = "test")

# you can put several tables here, it will keep French format

# back to default format
gtsummary::reset_gtsummary_theme()

