# AdventOfCode

Un premier projet pour pouvoir pousser les solutions d'AOC.

J'ai ajouté du code inspiré de [Colin Fraser](https://colin-fraser.net/post/a-quick-tutorial-on-importing-data-from-advent-of-code-into-r/) pour pouvoir télécharger les input des problèmes directement via R.

TLDR : il faut se logguer sur le site, et copier la valeur du cookie de session :

-   aller sur <https://adventofcode.com/2022/day/1/input>
-   Appuyer sur F12 : ça lance l'inspecteur Chrome ou l'outil de développement Firefox
-   Dans réseau ou network, regarder pour la ressource input la valeur de la du cookie de session
-   copier cette valeur lors du prompt Rstudio
