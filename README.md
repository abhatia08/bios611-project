## US county-level variation in characteristics influencing participation in outdoor recreation

This repository contains data and scripts used for the final project of [BIOS 611- Principles of Data Science](https://github.com/Vincent-Toups/datasci611), at the University of North Carolina at Chapel Hill  


### Abstract
The term “Nature Gap” sheds light on the racial and economic disparities in access to greenspace, unequal distribution of nature, and the unjust experience of people of color in the outdoors across the United States. Systematic practices such as economic segregation, redlining, forced migration, racial violence, and intimidation in the outdoors have been prevalent for decades, have perpetuated the racial divide. Contemporary examples during the pandemic of Christian Cooper “Birding While Black” in Central Park, and Ahmaud Arbery murdered while jogging down a boulevard in Georgia show the risk and difficulty endured by people of color while in outdoor spaces.  

Access to public open spaces provides communities with the opportunity to engage in physical activity and build community while incentivizing the conservation of biodiversity during the looming climate crisis. Factors typically influencing the use of these spaces include (but are not limited to) population demographics, proximity, and community recreational expenditure. For this analysis, we will use publicly available county-specific data to examine how these key factors influencing participation in outdoor recreation intersect with the proportion of the population that is people of color (i.e. not non-Hispanic white).  

### Issues 

Please report issues via email or the [issues page](https://github.com/abhatia08/bios-611-project/issues).

## Project structure 
:construction: *The structure of this project will change considerably over time* :construction:

  - `./scripts/` contains all the scripts needed to reproduce the analysis. 
  - `./source_data/` contains publicly-available data used in the analytic pipeline.
  - `./derived_data/` contains all processed data that results from our `./scripts/` pipeline.
## Getting Started

Build the docker image by typing:
```
docker build . -t abhi08
```

And then start RStudio by typing:

```
docker run -p 8787:8787 -e PASSWORD=password -v "$(pwd)":/home/rstudio -i -t abhi08
```

Once the Rstudio is running connect to it by visiting
https://localhost:8787 in your browser. 



