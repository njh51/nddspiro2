# nddspiro2 0.2.0

## Breaking changes

* `get_pid()` and `get_params()` have been renamed to `ls_pid()` and
  `ls_params()`.

* `read_spiro()` no longer has the `wide` and `tidy_colorder` arguments. It
  always returns one row per trial (wide format), with columns ordered as
  patient/test/trial fields followed by `<parameter>_<value|pred|pctpred|lln|zscore|unit>`
  in the order given by `parameters`.

## New features

* New lung function interpretation functions following the ERS/ATS 2022
  technical standard on interpretive strategies (Stanojevic et al., Eur Respir J
  2022; 60: 2101499), all vectorised and based on z-score limits of normal:
  * `lft_patterns()`: ventilatory pattern from spirometry, optionally refined
    with TLC, RV/TLC and FRC/TLC (normal, obstructive, PRISm, non-specific,
    simple/complex restrictive, possible mixed, mixed).
  * `lft_volume_pattern()`: lung volume pattern (restriction, hyperinflation,
    air trapping, large lungs).
  * `lft_gas_transfer()`: DLCO interpretation using VA and KCO.
  * `lft_severity()`: severity grading (normal, mild, moderate, severe).
  * `lft_bdr()` and `lft_bdr_response()`: bronchodilator response as percent of
    predicted, and a positive response (> 10% of predicted) for a single
    variable; combine FEV1 and FVC results with `|`.

* New `recode_ethnicity_gli()` and `recode_gender_gli()` to recode character or
  factor demographics into GLI-2012 numeric classes.

## Improvements

* `read_spiro()` is 3-8x faster on large files: test- and trial-level fields
  are read once per test and trial instead of once per parameter, and all
  parameter values are read with a single XPath query.

* `read_curve()` is about 2x faster: sampling values of all trials are parsed
  in one vectorised step.

* `read_spiro()` and `read_curve()` with `trace` no longer risk the libxml2
  "growing nodeset hit limit" error on large files.

## Bug fixes

* `read_spiro()` now keeps the order of `parameters` in the output columns.
  Previously, parameters containing an underscore (e.g. `FEV1_FVC`) could be
  moved to the end.

* `read_curve()` no longer merges the curves of different trials that share the
  same patient, test and trial metadata.

* `read_spiro()` no longer aggregates trials that share the same metadata.

## Other

* Added unit tests (testthat) and a GitHub Actions R-CMD-check workflow.

# nddspiro2 0.1.0

* Initial release with `read_spiro()`, `read_curve()`, `get_pid()` and
  `get_params()`.
