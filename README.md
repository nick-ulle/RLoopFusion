
# RLoopFusion

This package implements the loop fusion algorithm described by Kennedy and
McKinley for `for`-loops in R. For more details about the algorithm, see the
supplemental paper.

## Installation

1. Download and install R from <http://r-project.org/>

2. Install the R packages [graph], [RBGL], and [Rgraphviz] from Bioconductor. 
   These must be installed according to the linked directions.

[graph]: http://www.bioconductor.org/packages/release/bioc/html/graph.html
[RBGL]: http://www.bioconductor.org/packages/release/bioc/html/RBGL.html
[Rgraphviz]: http://www.bioconductor.org/packages/release/bioc/html/Rgraphviz.html

3. Install the R package devtools from CRAN, with
   `install.packages("devtools")`.

4. Finally, install this package from GitHub, with
   `devtools::install_github("RLoopFusion", "nick-ulle")`

### Note

If you have a pre-built binary, skip steps 3 and 4, open a shell terminal and
run:

```
R CMD INSTALL RLoopFusion_0.1-0.tar.gz
```

## Usage

See the package vignette "RLoopFusion":

```r
library(RLoopFusion)
vignette("RLoopFusion")
```
