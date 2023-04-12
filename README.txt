This repository contains the  R code for the project "Evolutionary determinants of reproductive seasonality: a theoretical approach" 
by Lugdiwine Burtschell, Jules Dezeure, Elise Huchard & Bernard Godelle

All code to repeat the analyses is provided here and instructions are given below.

The code in the file will automatically load all necessary input data from this repository.

1. Compile model from C++ code:
-From RStudio: File->New project->New Directory->R package using Rcpp->Use "RepSeason" as Directory name
-Copy file from "Rcpp" folder into the "src" folder of the project
-In RStudio: Build->Install and Restart

2. Use file "Model_validation.R" to recreate Table S5

3. Use file "Strategies_comparison.R" to recreate Fig 3 and Figure S5

4. Use file "Heatmap_data.R" to recreate heatmaps with changes in ecology and life history (used in the next step)
(/!\ the program is time consuming, we recommend you use powerfull computers or reduce the number of simulations and/or strategies tested)
You can skip this part and use datsets already simulated in folder "Heatmaps"

5.Use file "Plot_Heatmaps.R" to recreate Fig 2, 4, 5, 6, S6 and S7


