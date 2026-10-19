# nddspiro2

<!-- badges: start -->
[![R-CMD-check](https://github.com/njh51/nddspiro2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/njh51/nddspiro2/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Extract and tidy spirometry data from XML files exported by ndd EasyWare
spirometry devices, and interpret lung function results following the
ERS/ATS 2022 technical standard on interpretive strategies. XML parsing is
done with `xml2` and results are returned as `data.table`s. Contributions and
modifications are welcome.

## Installation

Install from GitHub with the `remotes` package:

```r
remotes::install_github("njh51/nddspiro2")
```

## Use cases

### Extract spirometry parameters

```r
library(nddspiro2)

doc = xml2::read_xml("path/to/ndd.xml")

# all spirometry parameter IDs in the file, e.g. "FEV1", "FVC", "PEF", ...
ls_params(doc)
```

`read_spiro()` returns one row per trial with patient demographics, test and
trial metadata, and for each parameter its value, predicted value, percent
predicted, LLN, z-score and unit (e.g. `FEV1_value`, `FEV1_zscore`).

```r
# default parameters: FEV1, FVC and FEV1_FVC, all trials
spiro = read_spiro(doc)

# selected parameters, columns follow the order given
spiro = read_spiro(doc, parameters = c("FEV1", "FVC", "FEV1_FVC", "PEF"))

# best trials only: `trace = n` keeps trials with rank 1 to n
spiro = read_spiro(doc, trace = 1)

# without percent predicted columns
spiro = read_spiro(doc, pct_pred = FALSE)
```

### Interpret lung function patterns (ERS/ATS 2022)

The interpretation functions are vectorised and use z-score limits of normal
(LLN = -1.645, ULN = +1.645).

```r
spiro = read_spiro(doc, trace = 1)

spiro[, `:=`(
  # normal, obstructive, prism (preserved ratio impaired spirometry) or possible mixed
  pattern = lft_patterns(
    fev1fvc_zscore = FEV1_FVC_zscore,
    fvc_zscore = FVC_zscore,
    fev1_zscore = FEV1_zscore
  ),
  # normal, mild, moderate or severe
  fev1_severity = lft_severity(FEV1_zscore)
)]
```

Spirometry alone cannot confirm restriction. When lung volumes are available,
TLC defines restriction, and RV/TLC or FRC/TLC separate simple from complex
restriction:

```r
lft_patterns(
  fev1fvc_zscore, fvc_zscore, fev1_zscore,
  tlc_zscore = tlc_z, rv_tlc_zscore = rv_tlc_z, frc_tlc_zscore = frc_tlc_z
)
```

## Reference

Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard on
interpretive strategies for routine lung function tests. *Eur Respir J*
2022; 60: 2101499. doi:10.1183/13993003.01499-2021
