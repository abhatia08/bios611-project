## US county-level variation in characteristics influencing participation in outdoor recreation

This repository contains data and scripts used for the final project of [BIOS 611- Principles of Data Science](https://github.com/Vincent-Toups/datasci611), at the University of North Carolina at Chapel Hill  

![](https://user-images.githubusercontent.com/17418954/145687283-d785cdfb-6b13-4e63-a6a3-e04b08a56112.jpg)

### Abstract
The term “Nature Gap” sheds light on the racial and economic disparities in access to greenspace, unequal distribution of nature, and the unjust experience of people of color in the outdoors across the United States. Systematic practices such as economic segregation, redlining, forced migration, racial violence, and intimidation in the outdoors have been prevalent for decades, have perpetuated the racial divide. Contemporary examples during the pandemic of Christian Cooper “Birding While Black” in Central Park, and Ahmaud Arbery murdered while jogging down a boulevard in Georgia show the risk and difficulty endured by people of color while in outdoor spaces.  

Access to public open spaces provides communities with the opportunity to engage in physical activity and build community while incentivizing the conservation of biodiversity during the looming climate crisis. Factors typically influencing the use of these spaces include (but are not limited to) population demographics, proximity, and community recreational expenditure. For this analysis, we will use publicly available county-specific data to examine how these key factors influencing participation in outdoor recreation intersect with the proportion of the population that is people of color (i.e. not non-Hispanic white).  

---
## Project structure 

  - `./scripts/` contains all the scripts needed to reproduce the analysis. The scripts are intended to be run in order of their number.
  - `./source_data/` contains publicly-available data used in the analytic pipeline.
  - `./derived_data/` contains all processed data that results from our `./scripts/` pipeline.
---
## Using this project

**Note: You will need [Docker](https://www.docker.com/) and the ability to run Docker as the current user in order to work with this project.*

##### Step 1: 
Fork and clone this repository to your local environment (see [here](https://docs.github.com/en/get-started/quickstart/fork-a-repo) for more information)

##### Step 2

Build the docker image by typing:
```
docker build . -t abhi08
```

##### Step 3

To start RStudio, enter the following command:
_Note: You may subtitute  `password123` for any password you prefer, and use that password in Step 4_
```
docker run \
    -p 8787:8787 \
    -p 8080:8080 \
    -e PASSWORD=password123 \
    -v "$(pwd)":/home/rstudio \
    -it abhi08
```
:warning: _The above code was written for a Windows machine. Powershell users may need to substitute `$(pwd)` with `${pwd}` instead._

##### Step 4. 

* Once the Rstudio is running connect to it by visiting [localhost:8787](http://localhost:8787/) in your browser 
* Enter `rstudio` as the username, and `password123` as the password (or any other password you specified in step 3). 

---
## Shiny 

Start the R shiny app associated with this project using one of the two methods:

1. From the command line:

```
docker run \
    -p 8787:8787 \
    -p 8080:8080 \
    -e PASSWORD=password123 \
    -v "$(pwd)":/home/rstudio \
    -it abhi08 sudo -H -u rstudio\
    /bin/bash -c "cd ~/scripts; Rscript 99_shiny.R"
```
*OR*

2. Navigating to the terminal within RStudio and typing the following:
  
``` 
make shiny
```
And it should be accessible on [localhost:8080](http://localhost:8080/) on your browser


---
## Makefile

To use the [`Makefile`](Makefile) included in this repository to build components of the project, run the following code in the terminal within RStudio.

### 1.  Clean all derived data, figures, and restart (initial step)
```
make clean
```
### 2. Clean source data and produce derived datasets.
  _Note: If you would like to run these scripts manually, instead of using the included Makefile, please follow the numerical order specified in the script name (starting with `01_clean_yelp.R`)_
```
# Cleaning source data
make derived_data/yelp_tidy.csv

make derived_data/population_by_age.csv

make derived_data/percent_nonwhite_pop.csv

make derived_data/ahrf_subset.csv

make derived_data/ahrf_subset.csv

# Dataset needed for analysis
make derived_data/plotting_data.csv
```
### 3. Make figures (univariate and bivariate plots)
_Note: Each Make target creates multiple plots in the `/figures` directory that are finally stitched together to create a single figure (output in .pdf and .png format)_
```
# Create all figures
make figures

# Create all Univariate figures
make figures/fig_univariate_all

# Create all Bivariate figures
make figures/fig_bivariate_retail

make figures/fig_bivariate_api
```
### 4. Make the shiny visualization (primary output for this project)
_Note: This was more useful for the purposes of the project compared to a static report, and was a more interesting way to display and interact with the data. This dashboard will be hopefully maintained beyond the scope of the class, and the code and source data will evolve over time accordingly._
```
make shiny
```

### 5. Make the final report 
:construction: _Note: For this project, the intended primary output was the shiny dashboard. However, the final report includes the proposal that laid out the objectives for this analysis. Subsequent versions may include additional analysis_ :construction:
```
make report
```

---
### Issues 

Please report issues to the [issues page](https://github.com/abhatia08/bios-611-project/issues).
