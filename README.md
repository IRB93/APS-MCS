# Atypical Parkinsonian Syndromes - Multidimensional Clinical Scale (APS-MCS) Shiny Application

This Shiny application is a part of the paper "Development and Validation of a New Clinical Rating Scale for Differential Diagnosis of Atypical Parkinsonian Syndromes" and provides an interactive tool for assessing the APS-MCS.

## Accessing the Application

You can access the live application directly at the following link: [https://atypicalparkinsoniansyndromes.shinyapps.io/shinyapps_scale/]

## Description

This application allows users to:

- Enter clinical data.
- Automatically calculate the scores for the APS-MCS clinical scale.
- Automatically calculate the probabilities for each diagnosis (Multiple System Atrophy vs. 4-Repeat Tauopathies) based on the APS-MCS

## Usage

1. **Initial Questions:**
   - Answer the initial questions to determine eligibility for further assessment.

2. **Domain Assessments:**
   - If eligible, navigate through the 'Frontal Domain,' 'Parietal Domain,' and 'Cerebellar Domain' tabs to perform assessments and enter the scores for each item.

3. **Results:**
   - Visualize raw scores for each domain, the adjusted total score, and the diagnostic probability for each category.

## Requirements

- A modern web browser (Chrome, Firefox, Safari, Edge).
- An internet connection.

## Data

The application uses pre-calculated probabilities stored in the `probabilidades.csv` file. This file must be in the same directory as the `app.R` file for the application to function correctly.

## Running Locally

If you wish to run the application locally, follow these steps:

1. Clone the repository:
    ```bash
    git clone https://github.com/IRB93/APS-MCS.git
    ```

2. Navigate to the project directory:
    ```bash
    cd APS-MCS
    ```

3. Ensure you have the required R packages installed:
    ```R
    install.packages(c("shiny", "shinydashboard", "shinyjs"))
    ```

4. Ensure the `probabilidades.csv` file is in the same directory as `app.R`.

5. Run the application:
    ```R
    shiny::runApp()
    ```
