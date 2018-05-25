# Files

#Not Reproducible due to identifiable information 

- `init.R`: Primary pipeline used to convert raw data from survey and other sources into final data available at `/data/..`

# NOTE: you will not be able to run init.R directly as it uses identifying data to locate households and generate weights. Here we share our pipeline for data creation for transparency. We use the barrio id's generated and merge them back into a raw but censored dataset to then further delineate the cleaning steps we undertook.

# Reproducible

- `base.R`: base functions

- `make_session_info.R`: just creates a file called `session_info.txt` which records the environment that ran this code successfully.

- `primary_data_creation.R`: uses the raw censored data and performs cleaning steps to create analytic datasets, here we clearly define our cleaning steps.

- `secondary_data_creation.R`: creation of RDS for figures

- `spatial_functions.R`: functions created to perform spatial cleaning, household localization and weight generation

# Correspondence
For any issues with anonymization or major issues with the functionality of the script please create an issue

## License
The data collected and presented is licensed under the [Creative Commons Attribution 3.0 license](http://creativecommons.org/licenses/by/3.0/us/deed.en_US), and the underlying code used to format, analyze and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).
