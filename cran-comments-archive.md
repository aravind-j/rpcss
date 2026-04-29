# Version 0.1.1 - First submission

- Fixed all issues in
  <https://cran-archive.r-project.org/web/checks/2025/2025-04-05_check_results_rpcss.html>.
- The email address of the maintainer is updated from
  <j.aravind@icar.gov.in> to <j.aravind@icar.org.in>. I am unable to
  send the confirmation for the same from the older email. I am unable
  to send the confirmation for the same from the older email as I have
  lost access to it. I have already updated my other packages PGRdup,
  EvaluateCore and germinationmetrics. Will be updating augmentedRCBD
  too ASAP.

### Test environments

- local Windows 10 Pro 25H2, R-release (R 4.5.3) & R-devel (R 4.6.0
  Pre-release).
- local Ubuntu 20.04, R-release (R 4.5.3) & R-devel (R 4.6.0
  Pre-release).
- win-builder, R-release (R 4.5.3) & R-devel (R 4.6.0 Pre-release).
- github macOS Sequoia 15.7.4, R-release (R 4.5.3).
- github Ubuntu 24.04.4, R-release (R 4.5.3), R-oldrel-1 (R 4.4.3) &
  R-devel (R 4.6.0 Pre-release).

### R CMD check results

- There were no ERRORs or WARNINGs.
- There is a NOTE - “misspelled words in DESCRIPTION, Hamon, Noirot,
  PCSS, al and et”, which can be ignored.
- There were no further NOTEs other than the maintainer change one.

# Version 0.1.0 - Third submission

- DESCRIPTION: Added the (year) in brackets.
- Replaced `\dontrun{}` with `\donttest{}` in examples taking \> 5s for
  execution.
- Fixed missing `\value` tags in documentation by adding the value in
  `print.pcss.core.Rd`. For generics `contrib.Rd` and `coreplot.Rd`,
  merged the documentation with the specific method with `\value` tags.

### Test environments

- local Windows 11 Pro 24H2, R-release (R 4.4.2) & R-devel (R 4.4.0
  Pre-release).
- Modified 24H2 `r-lib/actions` Github Action Ubuntu 24.04, R-release (R
  4.4.2) & R-devel (R 4.4.0 Pre-release).
- Modified 24H2 `r-lib/actions` Github Action macOS 14 arm64, R-release
  (R 4.4.2)
- win-builder, R-release (R 4.4.2) & R-devel (R 4.4.0 Pre-release).

### R CMD check results

- There were no ERRORs or WARNINGs.
- There is a NOTE - “misspelled words in DESCRIPTION, Hamon, Noirot,
  PCSS, al and et”, which can be ignored.

# Version 0.1.0 - Second submission

- Fixed examples with time \> 10s with `dontrun`.

### Test environments

- local Windows 11 Pro 24H2, R-release (R 4.4.2) & R-devel (R 4.4.0
  Pre-release).
- Modified 24H2 `r-lib/actions` Github Action Ubuntu 24.04, R-release (R
  4.4.2) & R-devel (R 4.4.0 Pre-release).
- Modified 24H2 `r-lib/actions` Github Action macOS 14 arm64, R-release
  (R 4.4.2)
- win-builder, R-release (R 4.4.2) & R-devel (R 4.4.0 Pre-release).

### R CMD check results

- There were no ERRORs or WARNINGs.
- There is a NOTE - “misspelled words in DESCRIPTION, Hamon, Noirot,
  PCSS, al and et”, which can be ignored.

# Version 0.1.0 - First submission

### Test environments

- local Windows 11 Pro 24H2, R-release (R 4.4.2) & R-devel (R 4.4.0
  Pre-release).
- Modified 24H2 `r-lib/actions` Github Action Ubuntu 24.04, R-release (R
  4.4.2) & R-devel (R 4.4.0 Pre-release).
- Modified 24H2 `r-lib/actions` Github Action macOS 14 arm64, R-release
  (R 4.4.2)
- win-builder, R-release (R 4.4.2) & R-devel (R 4.4.0 Pre-release).

### R CMD check results

- There were no ERRORs or WARNINGs.
- There is a NOTE - “misspelled words in DESCRIPTION, Hamon, Noirot,
  PCSS, al and et”, which can be ignored.
