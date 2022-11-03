# CovILD-Plus
Analysis pipeline for the 12-month follow-up of the CovILD study

## Terms of use

To reference or use the analysis results in your publication, please cite our GitHub repository and, if available, the accompanying publication in _ERJ open research_ (currently in press). The raw data are available upon request to the senior author [Prof. Judith Löffler-Ragg](mailto:judith.loeffler@i-med.ac.at).

## Usage

Source `exec_all.R` to launch the complete pipeline:

```r

source('exec_all.R')

```
Required development packages are available from my GitHub: 

```r
devtools::install_github('PiotrTymoszuk/soucer')
devtools::install_github('PiotrTymoszuk/ExDA')
devtools::install_github('PiotrTymoszuk/lmqc')
devtools::install_github('PiotrTymoszuk/kinet')
devtools::install_github('PiotrTymoszuk/clustTools')
devtools::install_github('PiotrTymoszuk/figur')

```

## Contact

The repository is curated by [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com). Data requests should be addressed to [Prof. Judith Löffler-Ragg](mailto:judith.loeffler@i-med.ac.at).
