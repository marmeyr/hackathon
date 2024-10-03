# Guide d'installation
Ce document décrit les différentes étapes d'installation du Datathon depuis une machine vierge pour faire tourner le code source, ainsi que l'installation des dépendances et de l'environnement de développement, les configurations nécessaires, et les commandes à utiliser.

## Prérequis
Avant de commencer, assurez-vous d'avoir les éléments suivants installés sur votre machine :

- Rstudio
- Les packages : orsm, dplyr, sf, tidyverse, ggiraph, FactoMineR, data.table, ggplot2, shinydashboard

## Installation de R
Télécharger R via le lien suivant :
https://cran.r-project.org/bin/windows/base/

## Étapes d'installation des packages sur R

Executez les commandes : 
- ``install.packages("orsm")``
- ``install.packages("dplyr")``
- ``install.packages("sf")``
- ``install.packages("tidyverse")``
- ``install.packages("ggiraph")``
- ...

Puis :
- ``library(orsm)``
- ``library(dplyr)``
- ``library(sf)``
- ``library(tidyverse)``
- ``library(ggiraph)``
- ...

Vous pouvez maintenant utiliser le projet.

# Changement du working directory
Une fois les packages lancés, changez votre répertoire de travail (working directory) :
setwd("...")
