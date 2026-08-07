# nmr.parser

Reads Bruker IVDr NMR data into R: acquisition and processing parameters, processed
spectra, and the XML reports Bruker writes alongside them.

## Install

```r
# install.packages("remotes")
remotes::install_github("phenological/nmr-parser")
```

## What it reads

Everything below is read from a Bruker expno folder.

| Data | File | Reader |
| --- | --- | --- |
| Acquisition and processing parameters | `acqus`, `pdata/1/procs` | `readParams()`, `readParam()` |
| Processed spectrum | `pdata/1/1r` | `readSpectrum()` |
| Sample title | `pdata/1/title` | `readTitle()` |
| ERETIC factor | `QuantFactorSample.xml`, `eretic_file.xml` | `readEretic()`, `readEreticF80()` |
| Quality control | `*_qc_report*.xml` | `readQc()` |
| Lipoproteins | `*lipo*.xml` | `readLipo()` |
| Small molecules | `*quant*.xml` | `readQuant()` |
| PACS | `*pacs*.xml` | `readPacs()` |

## Quick start

`readExperiment()` does the lot, for one expno or for thousands.

```r
library(nmr.parser)

expno <- system.file("HB-COVID0001", "10", package = "nmr.parser")
exp <- readExperiment(expno)
#> readExperiment >> acqus: 1/1
#> readExperiment >> procs: 1/1
#> readExperiment >> qc: 1/1
#> ...

names(exp)
#> "acqus" "procs" "qc" "title" "eretic" "spec" "lipo" "pacs" "quant"
```

Each element is a `data.table` with one row per expno and a `path` column to join on.

Read only what you need:

```r
exp <- readExperiment(expno, opts = list(what = c("acqus", "quant")))

exp$quant[, c("value.Ethanol", "unit.Ethanol", "refMax.Ethanol")]
#>    value.Ethanol unit.Ethanol refMax.Ethanol
#> 1:         0.000       mmol/L           0.82
```

One column per compound and per attribute, so a run of samples comes out ready to
`rbind()`.

Spectra come back interpolated onto a common ppm grid and corrected for ERETIC, so
they can be stacked into a matrix directly:

```r
exp <- readExperiment(expno, opts = list(
  what = "spec",
  specOpts = list(fromTo = c(-0.1, 10), length.out = 44079)
))

spec <- exp$spec$spec[[1]]
spec$spec          # data.table of x (ppm) and y (intensity)
spec$info          # SF, PHC0, PHC1, SR, ereticFactor, uncalibrated
```

## Finding expnos

`scanFolder()` walks a folder tree and returns the expnos matching an experiment
type. `EXP` and `PULPROG` are matched as patterns.

```r
lof <- scanFolder("~/data/rack01", options = list(EXP = "PROF_PLASMA", PULPROG = "noesy"))
exp <- readExperiment(lof$file)
```

Called without those options it lists what it found and prompts, which needs an
interactive session.

## Individual readers

Each returns `list(data, version)`, so the report version travels with the values.

```r
lipo <- readLipo(file.path(expno, "pdata", "1", "lipo_results.xml"))

lipo$data[1:2, c("id", "value", "unit")]
#>                                      id  value  unit
#> Main Parameters, Triglycerides, TG TPTG 139.84 mg/dL
#> Main Parameters, Cholesterol, Chol TPCH 203.74 mg/dL
```

Reference ranges ride along in `refMax`, `refMin` and `refUnit`.

`extend_lipo()` adds the parameters derived from those 112: total lipids and
cholesterol esters (`_calc`), lipid composition within a particle (`_pct`), and
distribution across subfractions (`_frac`).

```r
nrow(extend_lipo(lipo)$data)
#> 316
```

## Reference tables

Compound names, units and reference ranges, without needing a dataset to hand:

```r
getLipoTable()    # 112 lipoprotein parameters
getSmTable()      # small molecules, "SER" or "URI"
getQcTable()      # quality control tests
getPacsTable()    # PACS parameters
```

## Messages

Progress and diagnostics go to stderr and can be silenced:

```r
suppressMessages(readExperiment(lof$file))
```

A missing or unreadable file is reported and skipped, so one bad expno does not
stop a run.

## License

MIT, see [LICENSE.md](LICENSE.md).
