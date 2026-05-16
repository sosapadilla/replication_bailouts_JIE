# Replication Package: "Optimal Bailouts in Banking and Sovereign Crises"
*by Sewon Hur, Zeynep Yom, and Cesar Sosa-Padilla*<br>
*Forthcoming in the Journal of International Economics*

## Overview

This folder contains the replication files for the paper's baseline model, appendix tables, and appendix sensitivity figures. The package mixes three workflows:

1. **Fortran solvers.** These solve the quantitative model and write plain-text outputs such as `graphs_*.txt`, `results.out`, and welfare objects.
2. **MATLAB runners.** These compile the relevant Fortran files, run no-bailout and bailout variants in sequence when needed, collect moments, and export tables/figures.
3. **Stata scripts.** These are used only for the empirical figures and Appendix Figure C.5.

This replication package includes several convenience runners and pre-generated output folders. In many cases, figures can be regenerated directly from saved run folders without rerunning Fortran.

## Software Requirements

| Software                  | Version tested    | Purpose                                               |
| ------------------------- | ------------------|  ---------------------------------------------------- |
| GNU Fortran (`gfortran`)  | 12+               | Compiling and running the model solver                |
| MATLAB                    | R2022a or later   | Post-processing, table construction, figure generation|
| Stata                     | 17+               | Empirical figures and Appendix Figure C.5             |

Most MATLAB runners call `gfortran` via `system()`. On macOS, they assume Homebrew's toolchain is available under `/opt/homebrew/bin`. If your compiler lives elsewhere, adjust the `setenv('PATH', ...)` line near the top of the MATLAB runner you use.

## Top-Level Directory Structure

```
Replication/
|-- README.md
|-- data/
|-- do/
|-- fortran/
|-- matlab/
|-- figures/
|-- Tables2-4/
|-- Tables-section_C1/
|-- Tables-section_C2/
|-- Tables-section_C3/
|-- Tables-section_C4/
|-- Tables-section_C6/
|-- Tables-section_C7/
|-- Tables-section_C8/
|-- Figures-section_C5/
|-- Figure-section_C7/
|-- Figure_C5_section_C8/
\-- Figure-C6-section_C8/
```

## Baseline Empirical and Model Outputs

### Empirical Figures

Figures 1 and A.1 are produced from empirical data using Stata.

```
cd do/
do eurostat_plots_do.do
```

Inputs are read from `data/`. Final outputs are written to `figures/`, including updated PNG/PDF versions of the empirical plots.

### Baseline Model Figures (Figures 2–12)

The baseline model solver lives in `fortran/`. The standard workflow is:

```
cd fortran/
gfortran -O3 bailouts_optimal.f90 -o bailouts_optimal.out
./bailouts_optimal.out
```

Then run:

```
cd matlab/
plot_bailouts
```

This reads the `graphs_*.txt` files in `fortran/` and writes the main model figures to `figures/`. 

### Baseline Tables 2–4

```
cd Tables2-4/
gfortran -O3 bailouts_optimal_table.f90 -o bailouts_optimal_table.out
./bailouts_optimal_table.out
```

Setting `workflow_mode = 1` generates Tables 2 and 3. Setting `workflow_mode = 2` generates Table 4.

This writes:

- `table2_model_vs_data.csv`
- `table3_regime_comparison.csv`
- `table4_model_comparison.csv`


## Appendix Table Replication

### Table C.1 — Bailouts during exclusion

```
cd Tables-section_C1/
run_single_comparison_table
```

This compiles and runs the local `bailouts_optimal_nb.f90` and `bailouts_optimal_bl.f90` files, then writes the comparison table to `Tables-section_C1/single_comparison_run/`.

### Table C.2 — Alternative Welfare Weight ($\mu=0.5$)

```
cd Tables-section_C2/
run_table_C2_mu05
```

This runner uses the local Fortran pair:

- `bailouts_optimal_mu05_nobailout.f90`
- `bailouts_optimal_mu05_bailout.f90`

It writes results to `Tables-section_C2/table_C2_mu05_replication_run/`, including CSV and Excel versions of the simulated moments table.

### Table C.3 — Moral Hazard Extension

```
cd Tables-section_C3/
run_bailout_nobailout_table
```

This runner compiles and executes:

- `bailouts_extensions_nobailout.f90`
- `bailouts_extensions_bailout.f90`

Outputs are collected in `Tables-section_C3/bailout_nobailout_comparison_run/`.

### Table C.4 — Announcement Effect

```
cd Tables-section_C4/
run_announcement_effect_table
```

This script sweeps over the announcement-effect parameter using:

- `bailouts_extensions_eta_nobailout.f90`
- `bailouts_extensions_eta_bailout.f90`

Results are written to `Tables-section_C4/announcement_effect_replication_run/`.

### Table C.6 — Debt Maturity Robustness

```
cd Tables-section_C6/
run_table_c5_maturity
```

This runner uses the long-duration Fortran pair:

- `bailouts_optimal_ltd_nb.f90`
- `bailouts_optimal_ltd_bl.f90`

and exports the maturity table to `Tables-section_C6/table_c5_maturity_runs/table_c5_results.csv`. The folder also contains `report_tables.f90`, which supports formatted table reporting.

### Table C.7 — Storage Economy

```
cd Tables-section_C7/
run_storage_economy_table
```

The storage-economy table uses:

- `bailouts_storage_nobailout.f90`
- `bailouts_storage_bailout.f90`

The Fortran code reads calibration inputs from the five scalar text files:

- `param_calib_AA.txt`
- `param_calib_alpha_k.txt`
- `param_calib_beta.txt`
- `param_calib_k_epsilon_frac.txt`
- `param_calib_scale_mk.txt`

Outputs are written to `Tables-section_C7/storage_economy_replication_run/`.

### Table C.8 — Sensitivity Analysis

```
cd Tables-section_C8/
run_sensitivity_batch_two_execs
```

This runner uses namelist-driven Fortran files:

- `bailouts_optimal_nb.f90`
- `bailouts_optimal_bl.f90`

and writes the main sensitivity outputs to:

- `Tables-section_C8/sensitivity_batch_runs/sensitivity_results_long.csv`
- `Tables-section_C8/sensitivity_batch_runs/sensitivity_results_wide.csv`


## Appendix Figure Replication

### Figure C.4 — Storage Technology Sensitivity

```
cd Figure-section_C7/
run_figure_c4_tk_replication
```

This figure uses the local storage-economy Fortran pair and the MATLAB plotting script `figure_c4_welfare_tk.m`. The current version of the runner uses pre-calibrated $\bar A$ values rather than recalibrating $\bar A$ on the fly for each $\Gamma_k$ case. Final outputs include:

- `Figure-section_C7/Figure_C4_welfare_Tk.pdf`
- `Figure-section_C7/welfare_plot_tk.png`

The folder may also contain intermediate saved runs in `figure_c4_tk_runs/`.

### Figure C.5 — $\omega$ Sensitivity

```
cd Figure_C5_section_C8/
run_figure_c5_omega_replication
```

The plotting script is `figure_c5_welfare_omega.m`. The folder contains saved run directories under `figure_c5_omega_runs/`; these can be used to redraw the figure without rerunning Fortran if the saved outputs are already present.

Final outputs include:

- `Figure_C5_section_C8/welfgain_omega4.png`
- `Figure_C5_section_C8/Figure_C5_welfare_omega.pdf`

### Figure C.6 — $\theta$ Sensitivity

```
cd Figure-C6-section_C8/
run_figure_c6_theta_replication
```

This figure uses the MATLAB plotting script `figure_c6_welfare_theta.m`. Final outputs include:

- `Figure-C6-section_C8/welfgain_theta.png`
- `Figure-C6-section_C8/welfare_plot_theta.pdf`

### Figure for Appendix C.5 — State-Contingent Bailout Restrictions

This is the separate Stata-based workflow stored in `Figures-section_C5/`:

```
cd Figures-section_C5/
stata -b do analysis-optimal.do
stata -b do make_figures.do
```

The folder contains its own `README.md`. From the supplied files, the final figures can be rebuilt quickly from `results.csv` and `t_max_b.xlsx`.


## Important Notes About Saved Outputs

- Many folders already contain compiled executables, solver text outputs, generated CSV files, and figure exports. These are included so that users can inspect or redraw results without rerunning every heavy computation.
- Several MATLAB runners create or reuse `nb_files/` subdirectories. These store the no-bailout benchmark objects used by the bailout run when computing welfare gains.
- macOS metadata files such as `.DS_Store` can be ignored.



## Data Sources

| File                              | Source                        | Notes                         |
| --------------------------------- | ----------------------------- | ----------------------------- |
| `data/GDP_data_wb.csv`            | World Bank Open Data          | Real GDP data                 |
| `data/eurostat_total_list.csv`    | Eurostat                      | Fiscal aggregates             |
| `data/banking_crisis_dates.dta`   | Laeven and Valencia (2018)    | Banking crisis dates          |
| `matlab/rgdp_yield.xlsx`          | Datastream/FRED               | Real GDP and sovereign yields |



## Notes for Replicators

- The code has been tested primarily on macOS.
- If you only want to regenerate figures from saved outputs, you can often skip the Fortran runs and execute only the MATLAB or Stata plotting scripts.
- The sensitivity and figure folders under Appendix C.4–C.8 are intentionally modular. Each folder contains the local Fortran files and runner needed for that exercise.
- Some appendix workflows are computationally heavy when rebuilt from scratch. Where saved run directories are already present, plotting-only workflows are usually much faster.


## Contact

For questions about this replication package, please contact the authors:
Sewon Hur, sewonhur@yonsei.ac.kr
Cesar Sosa-Padilla, cesarspa@gmail.com
Zeynep Yom, zeynep.yom@villanova.edu
