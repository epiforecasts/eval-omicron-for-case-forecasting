# Evaluating the impact of modelling strain dynamics on short-term COVID-19 forecast performance

This repository contains the code, and analysis used to evaluate the impact of modelling strain dynamics on short-term COVID-19 forecast performance. In addition it also contains the write up of this work.

## Resources

- [`writeups`](writeups/): Summary paper and additional supplementary information.
- `_targets.R` and [`targets`](targets/): Analysis workflow and modular sections.
- [`data`](data/): Input data and summarised output generated by steps in the analysis.
- Rendered summary paper.
- Rendered SI.
- [`analyses`](analyses/): Ad-hoc analyses not part of the overarching worflow.
- [`NEWS.md`](NEWS.md): Dated development notes.
- `.devcontainer`: Resources for reproducibility using `vscode` and `docker`.

## Reproducibility

### Dependencies

This project uses `renv` to store its package dependencies. This means that any call to R will result in the project trying to bootsrap its dependencies. Alternatively this project comes with a `.devcontainer` for use with `vscode` and or GitHub codespaces. Finally, if desired the supplied `Dockerfile` may be used to recreate the development environment.

Optionally a list of dependencies may be generated using the following:

```r
renv::dependencies() 
```

If `cmdstanr` has not been installed previously then stan may need to be installed. This can be done using the following,

```r
cmdstanr::install_cmdstan()
```

### Analyses

All analyses have been implemented using a `targets` workflow in `_targets.R` with modules stored in `targets`. The full analysis can be recreated using the following,

```bash
. _targets.sh
```

Note that this is a computationally heavy pipeline and running it from end to end will require sufficient compute resources and some time. An archived version of this pipeline can be downloaded using the following,

```bash
Rscript get-targets-archive.R
```

The worflow can be explored using the following targets commands in an interactive R session.

- Run the workflow using all available workers.

```r
targets::tar_make_future(workers = future::availableCores())
```

- Explore a graph of the workflow.

```r
targets::tar_visnetwork(targets_only = TRUE)
```

- Watch the workflow as it runs.

```r
targets::tar_watch(targets_only = TRUE)
```

To understand the workflow in more detail see the supplementary information and the summary paper.
