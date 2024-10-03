rm(list = ls())

#Install packages
install.packages("shinydashboard")
install.packages("dplyr")
install.packages("sf")
install.packages("tidyverse")
install.packages("ggiraph")
install.packages("ggplot2")
install.packages("FactoMineR")
install.packages("data.table")

#Import Library
library(shinydashboard)
library(dplyr)
library(sf)
library(tidyverse)
library(ggiraph)
library(ggplot2)
library(FactoMineR)
library(data.table)

#/!\ Change working directory here and extract montpellier zip file
setwd("~/INSEE Data")

montpellier_commune = st_read(dsn = "montpellier", layer = "commune")
montpellier_carreaux = st_read(dsn = "montpellier", layer = "carreaux")
montpellier_espaces_verts = st_read(dsn = "montpellier", layer = "espacesverts")
montpellier_bpe = st_read(dsn = "montpellier", layer = "bpe")
montpellier_soins = st_read(dsn = "montpellier", layer = "soins")

montpellier_commune_seule = montpellier_commune %>% filter(NOM_COM == "MONTPELLIER")


mtp_D201 <- montpellier_bpe %>% filter(str_sub(TYPEQU,1, 4) == "D201")#medecin g
mtp_D232 <- montpellier_bpe %>% filter(str_sub(TYPEQU,1, 4) == "D232")#infirmier
mtp_D303 <- montpellier_bpe %>% filter(str_sub(TYPEQU,1, 4) == "D303")#ambulance
mtp_D307 <- montpellier_bpe %>% filter(str_sub(TYPEQU,1, 4) == "D307")#pharma
mtp_D106 <- montpellier_bpe %>% filter(str_sub(TYPEQU,1, 4) == "D106")#urgences

mtp_sante <- montpellier_bpe %>% filter(str_sub(TYPEQU,1, 4) %in% c("D201",
                                                                    "D232",
                                                                    "D303",
                                                                    "D307",
                                                                    "D106"))

#F107 athle
#F101 natation
#F121 Gymnase
#F120 Remise en forme


mtp_F107 <- montpellier_bpe %>% filter(str_sub(TYPEQU,1, 4) == "F107")# Athle
mtp_F101 <- montpellier_bpe %>% filter(str_sub(TYPEQU,1, 4) == "F101")# Athle
mtp_F203 <- montpellier_bpe %>% filter(str_sub(TYPEQU,1, 4) == "F203")# Athle
mtp_F121 <- montpellier_bpe %>% filter(str_sub(TYPEQU,1, 4) == "F121")# Athle
mtp_F120 <- montpellier_bpe %>% filter(str_sub(TYPEQU,1, 4) == "F120")# Athle

mtp_sport <- montpellier_bpe %>% filter(str_sub(TYPEQU,1, 4) %in% c("F107",
                                                                    "F101",
                                                                    "F203",
                                                                    "F121",
                                                                    "F120"))



montpellier_espaces_verts2 = montpellier_espaces_verts %>%
  filter(substr(ev_typolog,1,2) %in% c("1.", "19", "9.")) %>%
  mutate(nearest = "dist_to_nearest_park")


################################ TRAITEMENT AGE ###############################@

# Liste des colonnes à traiter
cols <- c("Ind_0_3", "Ind_4_5", "Ind_6_10", "Ind_11_17", "Ind_18_24", "Ind_25_39", 
          "Ind_40_54", "Ind_55_64", "Ind_65_79", "Ind_80p")

# Boucle sur chaque colonne pour calculer la proportion et créer une nouvelle colonne
for (col in cols) {
  prop_col <- paste0(col, "_prop")
  montpellier_carreaux[[prop_col]] <- montpellier_carreaux[[col]] / (montpellier_carreaux$Ind - montpellier_carreaux$Ind_inc)
}

# Définir les groupes d'âge
jeunes <- c("Ind_0_3_prop", "Ind_4_5_prop", "Ind_6_10_prop", "Ind_11_17_prop", "Ind_18_24_prop")
moyenne <- c("Ind_25_39_prop", "Ind_40_54_prop")
agee <- c("Ind_55_64_prop", "Ind_65_79_prop", "Ind_80p_prop")

# Extraire les données sans la géométrie
data_no_geom <- st_drop_geometry(montpellier_carreaux)

# Vérifier que les colonnes de proportions sont bien numériques
data_no_geom[jeunes] <- lapply(data_no_geom[jeunes], as.numeric)
data_no_geom[moyenne] <- lapply(data_no_geom[moyenne], as.numeric)
data_no_geom[agee] <- lapply(data_no_geom[agee], as.numeric)

# Calculer les sommes des proportions pour chaque groupe d'âge
data_no_geom$jeunes_prop <- rowSums(data_no_geom[jeunes], na.rm = TRUE)
data_no_geom$moyenne_prop <- rowSums(data_no_geom[moyenne], na.rm = TRUE)
data_no_geom$agee_prop <- rowSums(data_no_geom[agee], na.rm = TRUE)

# Ajouter les nouvelles colonnes à l'objet sf original
montpellier_carreaux$jeunes_prop <- data_no_geom$jeunes_prop
montpellier_carreaux$moyenne_prop <- data_no_geom$moyenne_prop
montpellier_carreaux$agee_prop <- data_no_geom$agee_prop
#########################################################################


montpellier_carreaux2 = montpellier_carreaux %>%
  select(x, y, geometry, jeunes_prop, moyenne_prop, agee_prop) %>%
  mutate(dist_to_nearest_park =
           st_centroid(.) %>%
           st_distance(montpellier_espaces_verts2) %>%
           apply(1, min, na.rm = TRUE)) %>%
  mutate(dist_to_nearest_D201 =
           st_centroid(.) %>%
           st_distance(mtp_D201) %>%
           apply(1, min, na.rm = TRUE)) %>%
  mutate(dist_to_nearest_D232 =
           st_centroid(.) %>%
           st_distance(mtp_D232) %>%
           apply(1, min, na.rm = TRUE)) %>%
  mutate(dist_to_nearest_D303 =
           st_centroid(.) %>%
           st_distance(mtp_D303) %>%
           apply(1, min, na.rm = TRUE)) %>%
  mutate(dist_to_nearest_D307 =
           st_centroid(.) %>%
           st_distance(mtp_D307) %>%
           apply(1, min, na.rm = TRUE)) %>%
  mutate(dist_to_nearest_D106 =
           st_centroid(.) %>%
           st_distance(mtp_D106) %>%
           apply(1, min, na.rm = TRUE)) %>%
  mutate(dist_to_nearest_F107 =
           st_centroid(.) %>%
           st_distance(mtp_F107) %>%
           apply(1, min, na.rm = TRUE)) %>%
  mutate(dist_to_nearest_F101 =
           st_centroid(.) %>%
           st_distance(mtp_F101) %>%
           apply(1, min, na.rm = TRUE)) %>%
  mutate(dist_to_nearest_F203 =
           st_centroid(.) %>%
           st_distance(mtp_F203) %>%
           apply(1, min, na.rm = TRUE)) %>%
  mutate(dist_to_nearest_F121 =
           st_centroid(.) %>%
           st_distance(mtp_F121) %>%
           apply(1, min, na.rm = TRUE)) %>%
  mutate(dist_to_nearest_F120 =
           st_centroid(.) %>%
           st_distance(mtp_F120) %>%
           apply(1, min, na.rm = TRUE)) %>%
  mutate(dist_to_nearest_F = (dist_to_nearest_F107+
                                  dist_to_nearest_F101+
                                  dist_to_nearest_F121+
                                  dist_to_nearest_F120)/4) %>%
  mutate(dist_to_nearest_D = (dist_to_nearest_D201+
                                  dist_to_nearest_D232+
                                  dist_to_nearest_D303+
                                  dist_to_nearest_D307+
                                  dist_to_nearest_D106)/5) %>%
  mutate(plus_proche_D =
           st_centroid(.) %>%
           st_distance(mtp_sante) %>%
           apply(1, min, na.rm = TRUE)) %>%
  mutate(plus_proche_F =
           st_centroid(.) %>%
           st_distance(mtp_sport) %>%
           apply(1, min, na.rm = TRUE))


########################################################################################
#                                                                                      #
#                                        OVERALL                                       #
#                                                                                      #
########################################################################################

montpellier_carreaux_overall = montpellier_carreaux2 %>%  mutate(rating = log((1.5*dist_to_nearest_D201+
                                                                          dist_to_nearest_D232+
                                                                          2*dist_to_nearest_D303+
                                                                          dist_to_nearest_D307+
                                                                          2*dist_to_nearest_D106+
                                                                          dist_to_nearest_park+
                                                                          dist_to_nearest_F107+
                                                                          dist_to_nearest_F101+
                                                                          dist_to_nearest_F121+
                                                                          dist_to_nearest_F120)/ 12.5))
         


min_rate = min(montpellier_carreaux_overall$rating)
max_rate = max(montpellier_carreaux_overall$rating)
plage = max_rate - min_rate


montpellier_carreaux_overall = montpellier_carreaux_overall %>% mutate(rating = abs(10 * (rating - min_rate)/plage - 10))



montpellier_carreaux_overall <- montpellier_carreaux_overall %>%
  mutate(tooltip_info = paste0(
    "Rating: ", round(rating, 2), "\n",
    "Espace vert le plus proche: ", round(dist_to_nearest_park, 2), "\n",
    "Distance moyenne santé: ", round(dist_to_nearest_D, 2), "\n",
    "Centré de santé le plus proche: ", round(plus_proche_D, 2), "\n",
    "Distance moyenne sport: ", round(dist_to_nearest_F, 2), "\n",
    "Infrastructure sportive la plus proche: ", round(plus_proche_F, 2), "\n"
  ))

plot <- ggplot() +
  geom_sf_interactive(data = montpellier_commune, aes(tooltip = NOM_COM)) +
  geom_sf_interactive(data = montpellier_carreaux_overall, aes(fill = rating, tooltip = tooltip_info)) +
  scale_fill_viridis_c(option = "magma") +
  ggtitle("Cumul des inégalités à Montpellier") +
  coord_sf(crs = 2154,
           xlim = st_bbox(montpellier_commune_seule)[c(1, 3)],
           ylim = st_bbox(montpellier_commune_seule)[c(2, 4)])

# Afficher le graphique interactif
girafe(ggobj = plot)








########################################################################################
#                                                                                      #
#                                       SANTE                                          #
#                                                                                      #
########################################################################################
montpellier_carreaux_sante = montpellier_carreaux2 %>%  mutate(
  rating_age = log(
    (
      (jeunes_prop*0.5 + moyenne_prop + agee_prop*2)*
        ((1.5*dist_to_nearest_D201+
            dist_to_nearest_D232+
            2*dist_to_nearest_D303+
            dist_to_nearest_D307+
            2*dist_to_nearest_D106)
         / 8.5)
    )
  )
)




montpellier_carreaux_sante = montpellier_carreaux_sante %>%  mutate(rating = log((1.5*dist_to_nearest_D201+
                                                                                 dist_to_nearest_D232+
                                                                                 2*dist_to_nearest_D303+
                                                                                 dist_to_nearest_D307+
                                                                                 2*dist_to_nearest_D106)/ 8.5))



min_rate = min(montpellier_carreaux_sante$rating)
max_rate = max(montpellier_carreaux_sante$rating)
plage = max_rate - min_rate

montpellier_carreaux_sante = montpellier_carreaux_sante %>% mutate(rating = abs(10 * (rating - min_rate)/plage - 10))


##### CARTE SANS AGE ######
ggplot() +
  geom_sf(data = montpellier_commune) +
  geom_sf(data = montpellier_carreaux_sante, aes(fill = rating)) +
  scale_fill_viridis_c(option = "magma") +
  #geom_sf(data = montpellier_espaces_verts2, fill = "darkolivegreen3") +
  #geom_sf(data = montpellier_bpe2, col = "firebrick") +
  ggtitle("Proximité des services de santé à Montpellier") +
  coord_sf(crs = 2154,
           xlim = st_bbox(montpellier_commune_seule)[c(1,3)],
           ylim = st_bbox(montpellier_commune_seule)[c(2,4)]) 





#####Carte avec le facteur age#####
min_rate_age = min(montpellier_carreaux_sante$rating_age)
max_rate_age = max(montpellier_carreaux_sante$rating_age)
plage = max_rate_age - min_rate_age

montpellier_carreaux_sante = montpellier_carreaux_sante %>% mutate(rating_age = abs(10 * (rating_age - min_rate_age)/plage - 10))

ggplot() +
  geom_sf(data = montpellier_commune) +
  geom_sf(data = montpellier_carreaux_sante, aes(fill = rating_age)) +
  scale_fill_viridis_c(option = "magma") +
  #geom_sf(data = montpellier_espaces_verts2, fill = "darkolivegreen3") +
  #geom_sf(data = montpellier_bpe2, col = "firebrick") +
  ggtitle("Proximité des services de santé à Montpellier, pondération par l'âge") +
  coord_sf(crs = 2154,
           xlim = st_bbox(montpellier_commune_seule)[c(1,3)],
           ylim = st_bbox(montpellier_commune_seule)[c(2,4)]) 


mtp_sante2 = mtp_sante %>% filter(TYPEQU %in% c("D303", "D106"))
ggplot() +
  geom_sf(data = montpellier_commune) +
  geom_sf(data = montpellier_carreaux_sante, aes(fill = rating)) +
  scale_fill_viridis_c(option = "magma") +
  #geom_sf(data = montpellier_espaces_verts2, fill = "darkolivegreen3") +
  geom_sf(data = mtp_sante2, aes(col = TYPEQU)) +
  ggtitle("Proximité des services de santé à Montpellier") +
  coord_sf(crs = 2154,
           xlim = st_bbox(montpellier_commune_seule)[c(1,3)],
           ylim = st_bbox(montpellier_commune_seule)[c(2,4)]) 








########################################################################################
#                                                                                      #
#                                       SPORT                                          #
#                                                                                      #
########################################################################################

montpellier_carreaux_sport = montpellier_carreaux2 %>%  mutate(rating_age = log((jeunes_prop*1.2 + moyenne_prop*1.2 + agee_prop*0.8)*(  dist_to_nearest_F107+
                                                                                                                                          dist_to_nearest_F101+
                                                                                                                                          dist_to_nearest_F203+
                                                                                                                                          dist_to_nearest_F121+
                                                                                                                                          dist_to_nearest_F120)/5))

montpellier_carreaux_sport = montpellier_carreaux_sport %>%  mutate(rating_sans_age = log((dist_to_nearest_F107+
                                                                                             dist_to_nearest_F101+
                                                                                             dist_to_nearest_F203+
                                                                                             dist_to_nearest_F121+
                                                                                             dist_to_nearest_F120)/5))


min_rate_age = min(montpellier_carreaux_sport$rating_age)
max_rate_age = max(montpellier_carreaux_sport$rating_age)
plage = max_rate_age - min_rate_age

montpellier_carreaux_sport = montpellier_carreaux_sport %>% mutate(rating_age = abs(10 * (rating_age - min_rate_age)/plage - 10))

#Carte avec le facteur age
ggplot() +
  geom_sf(data = montpellier_commune) +
  geom_sf(data = montpellier_carreaux_sport, aes(fill = rating_age)) +
  scale_fill_viridis_c(option = "magma") +
  #geom_sf(data = montpellier_espaces_verts2, fill = "darkolivegreen3") +
  #geom_sf(data = montpellier_bpe2, col = "firebrick") +
  ggtitle("Proximité des infrastructures sportives, pondération par l'âge") +
  coord_sf(crs = 2154,
           xlim = st_bbox(montpellier_commune_seule)[c(1,3)],
           ylim = st_bbox(montpellier_commune_seule)[c(2,4)]) 


min_rate_sans_age = min(montpellier_carreaux_sport$rating_sans_age)
max_rate_sans_age = max(montpellier_carreaux_sport$rating_sans_age)
plage = max_rate_sans_age - min_rate_sans_age

montpellier_carreaux_sport = montpellier_carreaux_sport %>% mutate(rating_sans_age = abs(10 * (rating_sans_age - min_rate_sans_age)/plage - 10))


#Carte sans le facteur age
ggplot() +
  geom_sf(data = montpellier_commune) +
  geom_sf(data = montpellier_carreaux_sport, aes(fill = rating_sans_age)) +
  scale_fill_viridis_c(option = "magma") +
  #geom_sf(data = montpellier_espaces_verts2, fill = "darkolivegreen3") +
  #geom_sf(data = mtp_sport, col = TYPEQU) +
  ggtitle("Proximité des infrastructures sportives") +
  coord_sf(crs = 2154,
           xlim = st_bbox(montpellier_commune_seule)[c(1,3)],
           ylim = st_bbox(montpellier_commune_seule)[c(2,4)]) 

### AVEC POINTS #####
ggplot() +
  geom_sf(data = montpellier_commune) +
  geom_sf(data = montpellier_carreaux_sport, aes(fill = rating_sans_age)) +
  scale_fill_viridis_c(option = "magma") +
  #geom_sf(data = montpellier_espaces_verts2, fill = "darkolivegreen3") +
  geom_sf(data = mtp_sport, aes(col = TYPEQU)) +
  ggtitle("Proximité des infrastructures sportives") +
  coord_sf(crs = 2154,
           xlim = st_bbox(montpellier_commune_seule)[c(1,3)],
           ylim = st_bbox(montpellier_commune_seule)[c(2,4)]) 





########################################################################################
#                                                                                      #
#                                CLUSTER OVERALL                                       #
#                                                                                      #
########################################################################################


cluster <- montpellier_carreaux_overall %>% mutate(cluster_group = if_else(rating > 7.5, "7,5 - 10",
                                                                if_else(rating > 5, "5 - 7,5","0 - 5")))


ggplot() +
  geom_sf(data = montpellier_commune) +
  geom_sf(data = cluster, aes(fill = cluster_group)) +
  ggtitle("Espaces verts et services de santé à Montpellier") +
  coord_sf(crs = 2154,
           xlim = st_bbox(montpellier_commune_seule)[c(1,3)],
           ylim = st_bbox(montpellier_commune_seule)[c(2,4)]) 



#Methode kmeans

# Extraire la colonne "rating"
ratings <- cluster$rating

# Calculer la somme des carrés totales (inertie) pour différents nombres de clusters
set.seed(123)  # Pour la reproductibilité
wss <- vector("numeric", length = 15)
for (k in 1:15) {
  kmeans_result <- kmeans(ratings, centers = k, nstart = 20)
  wss[k] <- kmeans_result$tot.withinss
}

# Tracer le critère du coude
elbow_plot <- data.frame(k = 1:15, wss = wss)
ggplot(elbow_plot, aes(x = k, y = wss)) +
  geom_point() +
  geom_line() +
  ggtitle("Critère du coude pour déterminer le nombre optimal de clusters") +
  xlab("Nombre de clusters k") +
  ylab("Inertie intra-classe totale (WSS)")

# Choisir le nombre optimal de clusters (4 car le coude se trouve à 4)
optimal_k <- 4

# Appliquer k-means avec le nombre optimal de clusters
final_kmeans <- kmeans(ratings, centers = optimal_k, nstart = 20)

# Ajouter les résultats des clusters au dataframe initial
cluster$clustering_kmeans <- final_kmeans$cluster

# Calculer la moyenne des ratings pour chaque cluster
cluster_means <- cluster %>%
  group_by(clustering_kmeans) %>%
  summarise(mean_rating = mean(rating)) %>%
  arrange(mean_rating)

# Réordonner les clusters en fonction des moyennes
cluster_reorder <- match(cluster$clustering_kmeans, cluster_means$clustering_kmeans)

# Attribuer les nouveaux labels de clusters
cluster$clustering_kmeans <- cluster_reorder

ggplot() +
  geom_sf(data = montpellier_commune) +
  geom_sf(data = cluster, aes(fill = clustering_kmeans)) +
  ggtitle("Clustering par kmeans") +
  coord_sf(crs = 2154,
           xlim = st_bbox(montpellier_commune_seule)[c(1,3)],
           ylim = st_bbox(montpellier_commune_seule)[c(2,4)]) 


########################################################################################
#                                                                                      #
#                                STAT DESCRIPTIVES                                     #
#                                                                                      #
########################################################################################


nb_sante = mtp_sante %>% group_by(TYPEQU) %>% count()
nb_sport = mtp_sport %>% group_by(TYPEQU) %>% count()

montpellier_carreaux3 <- subset(montpellier_carreaux2, select=-c(x, y))
montpellier_carreaux3 <- montpellier_carreaux2[ , !(names(montpellier_carreaux2) %in% c("geometry"))]

montpellier_carreaux2 <- as.data.table(montpellier_carreaux2)
montpellier_carreaux2[, geometry := NULL]
montpellier_carreaux3 <- montpellier_carreaux2

montpellier_carreaux3 <- subset(montpellier_carreaux3, select=-c(x, y))

pca <- PCA(montpellier_carreaux3)

# Afficher les valeurs propres
pca$eig

