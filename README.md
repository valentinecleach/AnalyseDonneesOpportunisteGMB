# Analyse de donnees opportunistes pour rechercher une potentielle tendance d'évolution temporelle de populations de mammifères en Bretagne

<hr style="border: 1px solid #000;">

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

Ce projet est réalisé pendant un stage d'ingénieur en data science au sein du Groupe Mammologique Breton en 2025. Il est encadré par Franck Simonnet.

<p align="right">

(<a href="#readme-top">remonter</a>)

</p>

------------------------------------------------------------------------

<!-- Comment utiliser -->

## Comment utiliser?

Ce projet a été codé avec R 4.5.0

1.  Tout d'abord, importer les bases de données manquantes.

2.  Puis, dans src -\> functions -\> divers.R, dans la fonction set_wd, changer le chemin de vos fichiers pour qu'elle correspondent a votre Working Directory. <br>

3.  Ensuite, ouvrir le main.R, pour lancer les deux commandes suivantes:<br>

``` r
source("init.R")
wd <- set_wd()
```

Le fichier init.R installera certains packages et importera les fonctions nécéssaire au lancement du code.

4.  Vous pourrez enfin lancer, a l'aide de `{r} knitr::knit()` les fichiers qui vous intéressent, sans devoir telecharger de packages ou autre.

<p align="right">

(<a href="#readme-top">remonter</a>)

</p>

------------------------------------------------------------------------

<!-- CONTACT -->

## Contact

Valentine CLEACH - [valentine.cleach\@gmail.com](mailto:valentine.cleach@gmail.com){.email}

Lien du projet: <https://github.com/valentinecleach/AnalyseDonneesOpportunisteGMB>

<p align="right">

(<a href="#readme-top">remonter</a>)

</p>
