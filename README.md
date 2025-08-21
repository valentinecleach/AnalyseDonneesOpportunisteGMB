<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#le-projet">Le projet</a>
    </li>
    <li>
      <a href="#comment-utiliser">Comment utiliser?</a>
    </li>
    <li><a href="#contact">Contact</a></li>
    </ol>
</details>

<!-- Le projet -->
## Le projet

**Analyse de donnees opportunistes pour rechercher une potentielle tendance d'evolution temporelle de populations de mammiferes en Bretagne**

Ce projet est réalisé pendant un stage d'ingénieur en data science au sein du Groupe Mammologique Breton en 2025. Il est encadré par Franck Simonnet. 
<br>
Il est codé avec R 4.5.0

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- Comment utiliser -->
## Comment utiliser?


1. Tout d'abord, importer les bases de données manquantes.

2. Puis, dans src -> functions -> divers.R, dans la fonction set_wd, 
changer le chemin de vos fichiers pour qu'elle correspondent a votre Working Directory.
<br>

3. Ensuite, ouvrir le main.R, pour lancer les deux commandes suivantes:<br>
```r
source("init.R")
wd <- set_wd()
```

4. Vous pourrez enfin lancer, a l'aide de  ```{r} knitr::knit() ``` les fichiers qui vous intéressent, sans devoir telecharger de packages ou autre.


<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- CONTACT -->
## Contact
Valentine CLEACH - valentine.cleach@gmail.com

Lien du projet: https://github.com/valentinecleach/AnalyseDonneesOpportunisteGMB

<p align="right">(<a href="#readme-top">back to top</a>)</p>