# Version 0.1.0 - Third submission

* DESCRIPTION: Added the (year) in brackets.
* Replaced `\dontrun{}` with ` \donttest{}` in examples taking > 5s for execution. 
* Fixed missing `\value` tags in documentation by adding the value in `print.pcss.core.Rd`. For generics `contrib.Rd` and `coreplot.Rd`, merged the documentation with the specific method with `\value` tags.

### Test environments
* local Windows 11 Pro 24H2, R-release (R 4.4.2) & R-devel (R 4.4.0 Pre-release).
* Modified 24H2 `r-lib/actions` Github Action Ubuntu 24.04, R-release (R 4.4.2) & R-devel (R 4.4.0 Pre-release).
* Modified 24H2 `r-lib/actions` Github Action macOS 14 arm64, R-release (R 4.4.2)
* win-builder, R-release (R 4.4.2) & R-devel (R 4.4.0 Pre-release).


# Version 0.1.0 - Second submission

* Fixed examples with time > 10s with `dontrun`.

### Test environments
* local Windows 11 Pro 24H2, R-release (R 4.4.2) & R-devel (R 4.4.0 Pre-release).
* Modified 24H2 `r-lib/actions` Github Action Ubuntu 24.04, R-release (R 4.4.2) & R-devel (R 4.4.0 Pre-release).
* Modified 24H2 `r-lib/actions` Github Action macOS 14 arm64, R-release (R 4.4.2)
* win-builder, R-release (R 4.4.2) & R-devel (R 4.4.0 Pre-release).

### R CMD check results
* There were no ERRORs or WARNINGs.
* There is a NOTE - "misspelled words in DESCRIPTION, Hamon, Noirot, PCSS, al and et", which can be ignored.

# Version 0.1.0 - First submission

### Test environments
* local Windows 11 Pro 24H2, R-release (R 4.4.2) & R-devel (R 4.4.0 Pre-release).
* Modified 24H2 `r-lib/actions` Github Action Ubuntu 24.04, R-release (R 4.4.2) & R-devel (R 4.4.0 Pre-release).
* Modified 24H2 `r-lib/actions` Github Action macOS 14 arm64, R-release (R 4.4.2)
* win-builder, R-release (R 4.4.2) & R-devel (R 4.4.0 Pre-release).

### R CMD check results
* There were no ERRORs or WARNINGs.
* There is a NOTE - "misspelled words in DESCRIPTION, Hamon, Noirot, PCSS, al and et", which can be ignored.
