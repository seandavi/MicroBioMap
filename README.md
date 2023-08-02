# Microbiome Compendium

  <!-- badges: start -->
  [![R-CMD-check](https://github.com/seandavi/MicroBioMap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/seandavi/MicroBioMap/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

Our dataset includes over 170,000 samples of publicly available 16S rRNA amplicon sequencing data, all processed using the same pipeline and reference database.

See the [full documentation](https://seandavi.github.io/MicroBioMap/articles/overview.html).

## Installation

For the smoothest installation experience, use the [`BiocManager` Bioconductor 
package](https://bioconductor.org/packages/BiocManager).

```
BiocManager::install('seandavi/MicroBioMap')
```

## Usage

Load the compendium using:

```{r}
library(MicroBioMap)
cpd <- get_compendium()
```

The resulting object is a `TreeSummarizedExperiment` object. Currently, the
"tree" part of the TreeSummarizedExperiment is not populated, but that is
on the roadmap.

