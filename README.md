<a name="readme-top"></a>

# Analyse de donnees opportunistes pour rechercher une potentielle tendance d'évolution temporelle de populations de mammifères en Bretagne 


<hr style="border: 1px solid #000;">


\
<!-- TABLE OF CONTENTS -->

<details>

<summary>Table of Contents</summary>

<ol>

<li><a href="#le-projet">Le projet</a></li>

<li><a href="#comment-utiliser">Comment utiliser?</a></li>

<li><a href="#contact">Contact</a></li>

</ol>

</details>

------------------------------------------------------------------------

<!-- Le projet -->

## Le projet

Ce projet est réalisé pendant un stage d'ingénieur en data science de 3 mois au sein du Groupe Mammalogique Breton en 2025. Il est encadré par Franck Simonnet. Il a pour mission d'étudier 2 bases de données participatives (VisioNature et GeoNature) afin de voir s'il est possible d'en tirer des informations de tendances temporelles. La base de donnée DIR Ouest a également été utilisé afin d'apporter des informations sur les collisions routières. Le rapport suivant explique les choix et apporte des explications au code fourni dans ce répertoire:

*Insérer_lien_vers_le_rapport*

<p align="right">

(<a href="#readme-top">Remonter</a>)

</p>

------------------------------------------------------------------------

<!-- Comment utiliser -->

## Comment utiliser?

Ce projet a été codé avec R 4.5.0

1.  Tout d'abord, importer les bases de données manquantes. Les données utilisées sont

    -   MasqueBretagneConti OU Une carte de la Bretagne continentale avec un buffer de 200m dessus. (Un fichier existe pour le transformer)

    -   Les famille de paysage

    -   Variables structurantes (la distance au littoral n'est pas nécessaire, du code existe dans un fichier pour le créer)

    -   Base de donné de collisions routières de la DIR Ouest

    -   GeoNature, GeoNature rongeurs, ..

    -   VisioNature, VisioNatures rongeurs ...

2.  Puis, dans R -\> divers.R, dans la fonction set_wd, changer le chemin de vos fichiers pour qu'elle correspondent a votre Working Directory. <br>

3.  Ensuite, ouvrir le main.R, pour lancer les deux commandes suivantes:<br>

``` r
source("init.R")
devtools::load_all()
wd <- set_wd()
```

Le fichier "init.R" installera certains packages. `{r} devtools::load_all()` lancera les fonctions nécessaire.

4.  Vous pourrez enfin lancer, a l'aide de `{r} rmarkdown::render()` les fichiers qui vous intéressent, sans devoir télécharger de package ou autre. Vous aurez ensuite le fichier a ouvrir depuis le dossier "output".

<p align="right">

(<a href="#readme-top">Remonter</a>)

</p>

------------------------------------------------------------------------

<!-- CONTACT -->

## Contact

Valentine CLEACH - [valentine.cleach\@gmail.com](mailto:valentine.cleach@gmail.com){.email}

Lien du projet: <https://github.com/valentinecleach/AnalyseDonneesOpportunisteGMB>

<p align="right">

(<a href="#readme-top">Remonter</a>)

</p>
