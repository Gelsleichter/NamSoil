<p><strong>Overview</strong></p>
<p>
  This repository contains the complete set of R and Google Earth Engine (GEE) scripts used to produce national-scale digital soil maps for Namibia at 90&nbsp;m spatial resolution. The workflow covers all stages of the mapping pipeline — from soil profile data preprocessing and depth harmonisation, through feature selection, hyperparameter tuning, Random Forest modelling, map post-processing, uncertainty quantification, spectral modelling, and external validation.
</p>

<p>
  The code is organised into six sequential modules, each stored in its own folder with dedicated <code>input/</code>, <code>output/</code>, and <code>script/</code> sub-directories. A master document (<code>Project_Overview_pipeline_environment.txt</code>) describes the full directory tree, the recommended execution order, and the R computing environment required for reproducibility.
</p>

<p><strong>Soil Properties Mapped</strong></p>
<p>
  The pipeline was applied to map 16 soil properties across three standard depth intervals (0–30, 30–60, 60–100&nbsp;cm): Sand (%), Silt (%), Clay (%), Bulk Density (kg/dm³), pH (water), Organic Carbon (%), Total Nitrogen (mg/kg), Phosphorus (mg/kg), Calcium (mg/kg), Magnesium (mg/kg), Potassium (mg/kg), Sodium (mg/kg), CEC (cmol<sub>c</sub>/kg), Base Saturation (%), Electrical Conductivity – 1:2.5 (µS/cm), and Electrical Conductivity – saturated paste (µS/cm).
</p>

<p><strong>Pipeline Structure</strong></p>

<table>
<thead>
<tr><th>Module</th><th>Folder</th><th>Description</th></tr>
</thead>
<tbody>
<tr>
  <td><b>1</b></td>
  <td><code>1_data_treatement/</code></td>
  <td>Soil profile data cleaning, table joining, missing-value treatment, and equal-area spline depth harmonisation to the three standard depth intervals. Includes exploratory data assessment scripts, histograms, boxplots, and spatial distribution plots.</td>
</tr>
<tr>
  <td><b>2</b></td>
  <td><code>2_feature_selection_tuning/</code></td>
  <td>Boruta-based feature selection and Random Forest hyperparameter tuning (ntree, mtry, nodesize, sampsize) using the regression matrix exported from GEE. Outputs per-property configuration files consumed by the GEE modelling step.</td>
</tr>
<tr>
  <td><b>2.5</b></td>
  <td><code>2.5_GEE/</code></td>
  <td>Google Earth Engine scripts for covariate preparation (lithological indices, Landsat spectral bands, ASTER mineral indices, terrain and climate layers), regression matrix export, and the core Random Forest modelling with 20-iteration bootstrap. Outputs Cloud-Optimised GeoTIFFs (mean, 5<sup>th</sup> and 95<sup>th</sup> percentiles) and per-iteration performance metrics. The full GEE repository is accessible at: <a href="https://code.earthengine.google.com/?accept_repo=users/Namibia_map/Soil_properties">https://code.earthengine.google.com/?accept_repo=users/Namibia_map/Soil_properties</a> or via <code>git clone https://earthengine.googlesource.com/users/Namibia_map/Soil_properties</code>.</td>
</tr>
<tr>
  <td><b>3</b></td>
  <td><code>3_final_maps/</code></td>
  <td>Post-processing of GEE outputs: applies physically meaningful min/max limits per property, computes the 90% prediction interval width (PI<sub>90</sub>), and generates publication-ready cartographic plots (PNG).</td>
</tr>
<tr>
  <td><b>4</b></td>
  <td><code>4_PICP/</code></td>
  <td>Computes the 90% Prediction Interval Coverage Probability (PICP<sub>90</sub>) to evaluate whether the bootstrap-derived uncertainty bands contain the observed values at the expected nominal rate.</td>
</tr>
<tr>
  <td><b>5</b></td>
  <td><code>5_modeling_spectra_data_sfa/</code></td>
  <td>Independent spectral modelling workflow: merges mid-infrared (MIR) spectra with wet chemistry reference data from the Soils for Africa (S4A) project, trains predictive models (Cubist, PLSR, Random Forest), and summarises results. Used to generate external validation predictions.</td>
</tr>
<tr>
  <td><b>6</b></td>
  <td><code>6_external_validation_sfa/</code></td>
  <td>External validation: extracts predicted values from the Namibia maps, SoilGrids, and iSDAsoil at independent sample locations; harmonises units and depths; computes validation metrics (RMSE, R², CCC) and produces comparison plots.</td>
</tr>
</tbody>
</table>

<p><strong>Repository File Tree</strong></p>
<pre>
Namibia_soil_maps_2024_2026/
├── Project_Overview_pipeline_environment.txt
│
├── 1_data_treatement/
│   ├── input/
│   │   ├── NamProfCleaned_21Mar2024.xlsx
│   │   └── National_boundary.shp (.dbf, .prj, .shx)
│   ├── metadata/
│   │   └── selected soil properties.txt
│   ├── output/
│   │   ├── datasets/ (splines, exploratory, GEE-ready exports)
│   │   ├── plots/    (histograms, boxplots, spatial distributions)
│   │   └── shapes/   (soil point shapefiles per property)
│   └── script/
│       ├── 1_Soil_data_join_tables_v2_preparation_for_splines.R
│       ├── 2_Soil_data_treatment_missing_values_depth_function_splines_new_0_30_60_100.R
│       ├── 2.1_Plots_centroids_Soil_data_treatment_…_splines_0_5_15_30_60_100_200_.R
│       ├── 3_plot_depth_hist.R
│       ├── 4_plot_hist_boxplot_splined_data_0_30_60_100.R
│       └── Exploratory_analyses_plots/
│           ├── Soil_data_assessment_…_Soil_classes_complete_info_OC_pH_BS_Sand_Clay_EC_BD.R
│           ├── Soil_data_assessment_…_Subsurface.R
│           └── Soil_data_assessment_…_Top_horiz.R
│
├── 2_feature_selection_tuning/
│   ├── input/
│   │   └── extracted_points_Nam_0_30_60_100_cm.csv
│   ├── output/
│   │   ├── plots/
│   │   └── properties_settings/  (per-property tuning configs)
│   └── script/
│       ├── 1_tuning_feature_selection_rf.R
│       ├── 2_check_data.R
│       └── 3_rename_github_gee_js.R
│
├── 2.5_GEE/
│   ├── input/  → GEE assets (see Readme.txt)
│   ├── outputs/ → Google Drive / GEE assets
│   └── script/
│       ├── 1_lithological_indexes
│       ├── 2_gee_covariates_preparation
│       ├── 3_Export_regression_matrix_to_R_tunning
│       ├── 4_Extract_modeling_metrics_COG
│       ├── 5_thumbnail_all_covars_legend  (independent/optional)
│       └── properties_settings/
│           ├── as_txt/   (48 config files, one per property × depth)
│           └── as_js_for_GEE/
│
├── 3_final_maps/
│   ├── input/
│   │   └── earth_engine_Nam_modeling_output_*/ (GeoTIFFs + metrics CSVs)
│   ├── output/
│   │   ├── dataset/  (prop.limits.csv, ref_vals.csv, summary.limits.csv)
│   │   └── plots/    (PNG maps: mean, lower, upper, range)
│   └── script/
│       └── make_maps_nam_v7_min_max_lg.R
│
├── 4_PICP/
│   ├── input/
│   ├── output/plots/
│   └── script/
│       ├── 0_PICP_source_function.R
│       └── 1_PICP.R
│
├── 5_modeling_spectra_data_sfa/
│   ├── input/
│   │   ├── s4a_submissions_Namibia_2023-09-28.xlsx
│   │   ├── S4A_Wetchem_20240911_NA.xlsx
│   │   └── spectral_data_Namibia_20250716.csv
│   ├── output/
│   │   ├── dataset/
│   │   ├── graphs_plots/
│   │   ├── metadata/
│   │   └── modeling_output/
│   └── script/
│       ├── 0_plot_fun_source.R
│       ├── 1_merge_spectra_mir_wetchem.R
│       ├── 2_modeling.R
│       └── 3_summarize_results.R
│
└── 6_external_validation_sfa/
    ├── input/
    │   ├── s4a_submissions_Namibia_2023-06-23.csv
    │   ├── S4A_Wetchem_20240911_NA.xlsx
    │   ├── S4A_Wetchem_20240911_NA_Topsoil_Samples_Coordinates.csv
    │   └── units_conversion.csv
    ├── output/plots/
    └── script/
        ├── 1_extract_samples_points__soil_grids_isda_Nam_maps.R
        ├── 2_units_conversion_depths_interpolation_v3_loop_datasets.R
        └── 3_validation_metrics_plots.R
</pre>

<p><strong>Input Data</strong></p>
<p>
  Model training was based on analytical data from the <em>Namibian Soil Profile Database (NSPD2025)</em> (<a href="https://zenodo.org/records/17618737">https://zenodo.org/records/17618737</a>). Environmental covariates (65 layers covering terrain, climate, remote sensing, geology, and land cover) were assembled in Google Earth Engine. External validation used independent soil data from the Soils for Africa (S4A) project in Namibia.
</p>

<p><strong>Computing Environment</strong></p>
<p>
  R scripts were developed and tested under R&nbsp;4.3.3 on Ubuntu&nbsp;24.04 LTS. Key R packages include: <code>terra</code> (spatial data handling), <code>ranger</code> (Random Forest), <code>Boruta</code> (feature selection), <code>tuneRanger</code> / <code>mlr</code> (hyperparameter tuning), <code>caret</code> (model evaluation), <code>ithir</code> (depth splines), <code>Cubist</code> and <code>pls</code> (spectral modelling), <code>ggplot2</code> (visualisation), and <code>doParallel</code> / <code>foreach</code> (parallel processing). Geospatial libraries: GDAL&nbsp;3.8.4, GEOS&nbsp;3.12.1, PROJ&nbsp;9.4.0. The complete <code>sessionInfo()</code> output and driver list are provided in <code>Project_Overview_pipeline_environment.txt</code>.
</p>

<p><strong>How to Run</strong></p>
<p>
  Recommended execution order:
</p>
<ol>
  <li><code>1_data_treatement</code> — Soil data cleaning and depth harmonisation</li>
  <li><code>2_feature_selection_tuning</code> — Feature selection and RF tuning in R</li>
  <li><code>2.5_GEE</code> — Covariate assembly and RF modelling in Google Earth Engine</li>
  <li><code>3_final_maps</code> — Map post-processing and cartographic outputs</li>
  <li><code>4_PICP</code> — Prediction interval coverage evaluation</li>
  <li><code>5_modeling_spectra_data_sfa</code> — Spectral modelling (optional / independent)</li>
  <li><code>6_external_validation_sfa</code> — External validation (optional / independent)</li>
</ol>
<p>
  Each module reads from its own <code>input/</code> folder and writes to <code>output/</code>. Intermediate outputs propagate to downstream modules as described in the readme files within each folder.
</p>

<p><strong>Related Datasets and Publications</strong></p>
<p>
  The predicted soil maps (GeoTIFF and PNG) for each property and depth interval are published as separate Zenodo records. Quick-start scripts for reading, cropping, and exporting NamSoil layers directly from Zenodo — without downloading the full files — are available at: <a href="https://github.com/Gelsleichter/acquire_NamSoil/">https://github.com/Gelsleichter/acquire_NamSoil/</a>.
</p>
<p>
  A full methodological description, model evaluation framework, and interpretation of results are provided in:<br/>
  <em>[Publication DOI to be added]</em>
</p>
