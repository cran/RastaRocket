# RastaRocket

<!-- badges: start -->
  [![R-CMD-check](https://github.com/BiostatUSMR/RastaRocket/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BiostatUSMR/RastaRocket/actions/workflows/R-CMD-check.yaml)
  [![Documentation](https://img.shields.io/badge/docs-online-blue.svg)](https://biostatusmr.github.io/RastaRocket/)
<!-- badges: end -->

## English

### Package installation

Install from CRAN:

install.packages("RastaRocket")   

Or install the latest development version on GitHub. Note that the devtools package must be installed beforehand. Ensure the `build_vignettes = TRUE` option is set during installation to access the included tutorial vignette:

```r
devtools::install_github("BiostatUSMR/RastaRocket", build_vignettes = TRUE)
```

### Access tutorials

After installing the package with `build_vignettes = TRUE`, you can access the `RastaRocket` tutorials contained within the vignette. Two tutorials are included :

#### Setup project

To see how you can setup a project, please look at `SetupProject` vignette 

```r
vignette("SetupProject")
```

#### Description function

To view the tutorial on how to use the descriptive functions, run the following command:

```r
vignette("RastaRocketVignette")
vignette("DescribeAE")
```

#### Adverse Event Description

To view the tutorial on how to use the adverse event descriptive functions, run the following command:

```r
vignette("DescribeAE")
```

If everything is set up correctly, the vignettes should open, providing a detailed guide to using the package.

## Français

### Installation du package

Installation depuis le CRAN :

install.packages("RastaRocket")

Ou installez la dernière version de développement disponible sur GitHub. Notez que le package `devtools` doit être installé au préalable. Assurez-vous que l’option `build_vignettes = TRUE` est activée lors de l’installation pour accéder au tutoriel inclus sous forme de vignette :

```r
devtools::install_github("BiostatUSMR/RastaRocket", build_vignettes = TRUE)
```

### Accéder au tutoriel


Après avoir installé le package avec l'argument `build_vignettes = TRUE`, vous avez accès aux deux tutoriels de `RastaRocket` :

#### Démarrer un projet

Pour voir comment démarrer un projet, vous pouvez regarder la vignette `SetupProject` :

```r
vignette("SetupProject")
```

#### Fonction de description

Pour voir le tutoriel sur les fonctions de description, vous pouvez ouvrir la vignette `RastaRocketVignette` :

```r
vignette("RastaRocketVignette")
```

Si tout est bien installé, les deux vignettes devraient s'ouvrir.
