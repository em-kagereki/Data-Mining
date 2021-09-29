# install.packages("remotes")

setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")


library(xaringanBuilder)
build_pdf("Presentation.Rmd")
build_pdf("Presentation.Rmd")

## https://stackoverflow.com/questions/54968311/xaringan-export-slides-to-pdf-while-preserving-formatting

## How to convert the more complex ones

if (interactive()) {
  xaringan::decktape("Presentation.Rmd", "xaringan.pdf", 
                     docker = FALSE)
}