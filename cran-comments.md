## Resubmission — v1.7.2

The last version accepted to CRAN was v1.6.4 (published 2026-03-26).
A prior submission of v1.7.0 was not completed; this submission supersedes
it. No breaking changes are introduced (see post-hoc note below).

Changes since v1.6.4 are documented in NEWS.md. In brief:

* Two new exported functions: `ternStyle()` and `classify_normality()`
* New parameters across `ternG()`, `ternD()`, `ternP()`, `word_export()`,
  `ternB()`: `force_normal`, `force_continuous`, `show_p`, `show_missing`,
  `show_missingness`, `missing_indicators`, `zero_to_dash`,
  `percentage_compute`, `categorical_posthoc`, `p_adjust`,
  `p_adjust_display`, `round_decimal`, `font_family`, `citation`,
  `open_doc`, `plain_header`, `abbreviation_footnote`, `variable_footnote`, `index_style`;
  `bold_sig` in `ternStyle()` and `word_export()`; `mode`, `extra_na`,
  `drop_cols` in `ternP()`
* Post-hoc testing now fully supported: pairwise continuous comparisons
  (Games-Howell / Dunn's test with compact letter display, `post_hoc = TRUE`)
  were silently non-functional in v1.6.4 due to `rstatix` and `multcompView`
  being in `Suggests`; both are now in `Imports`. New: `categorical_posthoc`
  adds Haberman adjusted standardized residual analysis for categorical
  variables in 3+ group comparisons. Note: CLD letter ordering now follows
  standard `multcompLetters()` alphabetical convention rather than
  center-based re-labeling; statistical conclusions are unchanged.
* Bug fixes: C-level `fisher.test()` segfault on large contingency tables;
  degenerate single-level categorical crash; `methods_filename = NULL`
  crash; three `categorical_posthoc` edge-case crashes; blank-string factor
  levels producing `NA (NA%)`; blank pages and citation bleed in `ternB()`;
  `bold_sig` column-name mismatch after internal header renaming
* CRAN compliance: `<<-` eliminated throughout; bare `set.seed()` replaced
  with `withr::with_seed()`; `rstatix` and `multcompView` promoted to
  `Imports`

---

## R CMD check results

* macOS Sonoma 14.3, R 4.5.1 aarch64-apple-darwin23.6.0 (local) —
  0 errors | 0 warnings | 0 notes
* Windows Server 2022, R-devel r90099 x86_64-w64-mingw32 (win-builder) —
  0 errors | 0 warnings | 0 notes

---

## Test environments

* Local: macOS Sonoma 14.3, R 4.5.1, aarch64-apple-darwin23.6.0
* win-builder: Windows Server 2022, R-devel r90099, x86_64-w64-mingw32

---

## Downstream dependencies

No packages on CRAN currently depend on TernTables.
