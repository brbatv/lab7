## ----include=FALSE-------------------------------------------------------
library(devtools)
install_github("brbatv/lab7",force=TRUE)
library(lab7ab)
library(nycflights13)

## ----echo=FALSE,warning=FALSE--------------------------------------------
colnames(weather)

## ----echo=FALSE,warning=FALSE--------------------------------------------
colnames(flights)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

