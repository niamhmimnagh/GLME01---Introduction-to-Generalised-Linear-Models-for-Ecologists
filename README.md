# GLME01 - Introduction to Generalised Linear Models for Ecologists
This repository contains all materials for the Introduction to Generalised Linear Models (GLMs) course. It includes datasets, example scripts, and lecture slides used throughout the sessions.

---

 📂 Repository Structure

```text
GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/
│
├── data/                  # Datasets used in the course
│   ├── plant_biomass.txt/.csv        # Continuous response (Gaussian/identity)
│   ├── disease_presence.txt/.csv     # Binary response (logistic)
│   └── ... other datasets ...
│
├── scripts/               # R code for demos and data creation
│   ├── simulate_data.R            # Reproducible code to generate datasets
│   ├── coding_demo.R              # Coding demonstrations shown in lectures
│   └── ... additional scripts ...
│
├── slides.pdf             # PDF of the lecture slides
│   ├── intro to GLMs 1            
│   ├── intro to GLMs 2          
│   └── ... additional scripts ...
│
└── README.md              # This file
```
📖 Course Overview

This course introduces the GLM framework, moving from the classical linear model to models for non-Gaussian data using appropriate link functions and distributions. We emphasize interpretation, diagnostics, and clear communication of results.

    Topics covered

    - Recap of linear models and assumptions
    - GLM components: random component (distribution), systematic component (linear predictor), link
    - Common GLMs:
        -Logistic regression (binary outcomes)
        -Logistic regression (binomial outcomes)
        -Multinomial logistic regression (more than two outcomes)
        -Ordinal logistic regression (ordered outcomes)
        - Poisson regression (counts)
        - Quasi-models (overdispersed data)
        - Negative Binomial regression (overdispersed data)
        - Beta Binomial regression (overdispersed data)
        - Zero-inflated regression
    - Fitting and interpreting GLMs in R (glm)
    - Brief introduction to GLMMs
    - Brief introduction to Bayesian statistics
    - Model diagnostics and assumption checks


📊 Data

The data/ folder contains simulated datasets used for exercises and demonstrations:

    plant_biomass.txt/.csv — Mean annual plant biomass vs rainfall (Gaussian/identity).

    disease_presence.txt/.csv — Disease presence (0/1) with predictors such as age (logistic).

    See scripts for import examples.

💻 Scripts

The scripts/ folder contains reproducible R code for:

    simulate_data.R — Code used to generate the datasets stored in data/.

    coding_demo.R — Step-by-step coding demonstrations (simulation, fitting, diagnostics, predictions, plots).

    Additional helper scripts as needed for particular sessions.

📑 Slides
The slides/ folder contains pdf. files of all lecture slides. 

    
🔧 Getting Started

  Clone this repository

    git clone https://https://github.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists.git

Open in R/RStudio (recommended).

Install required packages (used across demos; install as needed):

    install.packages(c(
    "ggplot2",    # plotting
    "MASS",       # negative binomial (glm.nb)
    "DHARMa",     # residual diagnostics for GLMs
    "pROC",       # ROC/AUC for logistic models
    "AER",        # dispersion test for Poisson models
    "pscl",       # zero-inflated Poisson / negative binomial models
    "effects",    # partial effect plots for GLMs
    "car",        # VIF, Anova, diagnostic tools
    "performance" # model performance checks
    ))

Run the demos

    Open files in scripts/ and run top-to-bottom to reproduce examples.

    Load datasets from data/:

        # Tab-delimited example
        biomass <- read.delim("data/plant_biomass.txt", header = TRUE)

        # CSV example
        disease <- read.csv("data/disease_presence.csv", header = TRUE)

        Or read directly from GitHub raw links if preferred.

Contributing

Issues and pull requests are welcome (typos, improvements to code comments, additional examples).
