
# Clean up datasets -------------------------------------------------------

## --> run jupyter notebook "create_simba2_datasets"

#source("ssym_dataset_1.R")

## --> run jupyter notebook "ssym_dataset"

#source("ssym_dataset_2.R")

# Generate new datasets ---------------------------------------------

source("code/generate_b1112_datasets.R")
source("code/generate_synon_testdata.R")

# Fit models --------------------------------------------------------------

source("code/fit_models.R")

# Make predictions -------------------------------------------------------------

source("code/predict_models.R")
source("code/predict_Simba_I_model.R")
source("code/predict_synon.R")

# Make figures ------------------------------------------------------------

source("code/figures.R")

# Make tables -------------------------------------------------------------
   
source("code/table_1.R")
source("code/tables_S_2_3.R")
source("code/table_4.R")