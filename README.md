# statistics

This repository is created for purposes of statistical methods classes. It contains particular methods helpful for topics of the course.

Installation process to the Rstudio:

    install.packages("devtools")

    library(devtools)

    devtools::install_github("gcardas/cardasStat")

    library(cardasStat)

UPDATING PROCESS

(in console) :

    remove.packages("cardasStat")

then check the path of the libraries (in console):

    .libPaths() 
    
copy the path without ""

then open TERMINAL and type:

    rm -rf copied_path/cardasStat

WARNING! if in the outcome of .libPaths() were more than one path, run the command above for each of them


QUIT the R studio, do not save enviroment
OPEN it again


then run the command (in console):

    devtools::install_github("gcardas/cardasStat", force = TRUE)

it should work now!


----
t test when I have data

    t.test(x, mu=2, alternative = "two.side", coef.leve = 0.95, var.equal = FALSE)



Approximations

P(X = a)     = P(a − 0.5 ≤ Y ≤a + 0.5),

P(a < X < b) = P(a + 0.5 ≤ Y ≤b − 0.5),

P(a ≤ X ≤ b) = P(a − 0.5 ≤ Y ≤b + 0.5),

P(a < X ≤ b) = P(a + 0.5 ≤ Y ≤b + 0.5),

P(a ≤ X < b) = P(a − 0.5 ≤ Y ≤b − 0.5).
