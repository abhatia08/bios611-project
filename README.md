## US county-level variation in characteristics influencing participation in outdoor recreation

This repository contains data and scripts used for the final project of [BIOS 611- Principles of Data Science](https://github.com/Vincent-Toups/datasci611), at the University of North Carolina at Chapel Hill  


### Abstract
The term “Nature Gap” sheds light on the racial and economic disparities in access to greenspace, unequal distribution of nature, and the unjust experience of people of color in the outdoors across the United States. Systematic practices such as economic segregation, redlining, forced migration, racial violence, and intimidation in the outdoors have been prevalent for decades, have perpetuated the racial divide. Contemporary examples during the pandemic of Christian Cooper “Birding While Black” in Central Park, and Ahmaud Arbery murdered while jogging down a boulevard in Georgia show the risk and difficulty endured by people of color while in outdoor spaces.  

Access to public open spaces provides communities with the opportunity to engage in physical activity and build community while incentivizing the conservation of biodiversity during the looming climate crisis. Factors typically influencing the use of these spaces include (but are not limited to) population demographics, proximity, and community recreational expenditure. For this analysis, we will use publicly available county-specific data to examine how these key factors influencing participation in outdoor recreation intersect with the proportion of the population that is people of color (i.e. not non-Hispanic white).  

---
## Project structure 
:construction: *The structure of this project will change considerably over time* :construction:

  - `./scripts/` contains all the scripts needed to reproduce the analysis. 
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

```
docker run \
    -p 8787:8787 \
    -e PASSWORD=password123 \
    -v "$(pwd)":/home/rstudio \
    -it abhi08
```

Once the Rstudio is running connect to it by visiting
https://localhost:8787 in your browser, and enter `rstudio` as the username, and `password123` as the password. 

---
## Shiny (Homework 10)

To explore preliminary county-level data in the dataset, start the R shiny app using one of the two methods:

1. From the command line:

```
docker run \
    -p 8080:8080 \
    -p 8787:8787 \
    -e PASSWORD=password123 \
    -v "$(pwd)":/home/rstudio \
    -it abhi08 sudo -H -u rstudio\
    /bin/bash -c "cd ~/scripts; Rscript shiny.R"
```
*OR*

2. Navigating to the terminal within RStudio and typing the following:
  
``` 
cd scripts; Rscript shiny.R
```
And it should be accessible on [localhost:8787](http://localhost:8787/) on your browser



---
### Issues 

Please report issues to the [issues page](https://github.com/abhatia08/bios-611-project/issues).