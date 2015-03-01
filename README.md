## metaheuR

This package is being developed as a tool for teaching the course [_bilaketa heuristikoak_](http://www.ehu.eus/p200-content/eu/pls/entrada/plew0040.htm_asignatura_next?p_sesion=&p_cod_idioma=EUS&p_en_portal=S&p_cod_centro=226&p_cod_plan=GINFOR20&p_anyoAcad=act&p_pestanya=3&p_menu=principal&p_cod_asig=26222&p_ciclo=X&p_curso=4&p_vengo_de=asig_cursos) (search heuristics) at the [University of the Basque Country](www.ehu.eus). It is by no means a package to solve actual optimization problems, as the main concern is **not an efficient implementation**.

The package can be installed using the R package [devtools](http://cran.r-project.org/web/packages/devtools/index.html). 

```r
if (!require("devtools"))
  install.packages("devtools")

devtools::install_github("b0rxa/metaheuR")
```

The package is at a very early stage of development so no stable version is available. Many basic aspects of the package may change, so use it with caution.

To access the avialable vignettes for the package, type:

```r
library("metaheuR")
browseVignettes("metaheuR")
```
The currently available vignettes are:

* [First steps](http://htmlpreview.github.io/?https://github.com/b0rxa/metaheuR/blob/master/inst/doc/metaheuR_first_steps.html)
* [Optimization problems](https://github.com/b0rxa/metaheuR/blob/master/inst/doc/metaheuR_optimization_problems.html)