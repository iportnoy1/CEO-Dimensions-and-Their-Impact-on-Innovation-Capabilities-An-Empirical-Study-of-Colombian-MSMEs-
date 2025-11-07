# CEO Dimensions and Their Impact on Innovation Capabilities  
### An Empirical Study of Colombian MSMEs  

**Authors:**  
Tobías Alfonso Parodi Camaño¹*, Jhon Víctor Vidal Durango², Iván Portnoy²  
¹Universidad de Córdoba  
²Universidad de la Costa  
\*Corresponding author: [tobiasparodic@correo.unicordoba.edu.co](mailto:tobiasparodic@correo.unicordoba.edu.co)  

---

## Abstract
Innovation in micro, small, and medium-sized enterprises (MSMEs) often stems from CEO traits and leadership. This study examines how CEO personality, transformational leadership, and innovative behavior drive innovation capabilities in Colombian MSMEs.  
Grounded in the **HEXACO model** and **transformational leadership theory**, a sequential mediation model was tested using data from **106 CEOs**. Structural equation modeling (SEM) revealed that **Openness to Experience** and **Conscientiousness** predict **transformational leadership**, which in turn promotes **innovative behavior**, enhancing **organizational innovation capabilities**.  
The findings highlight leadership as a critical pathway through which CEO traits shape innovation outcomes in resource-constrained firms, contributing to **upper-echelons theory** and offering practical implications for MSME leadership development.

---

## Data and Methods
- **Design:** Cross-sectional, correlational study  
- **Sample:** 106 CEOs of Colombian MSMEs across diverse sectors (services, manufacturing, agribusiness, technology)  
- **Instruments:**
  - **HEXACO-60** (Ashton & Lee, 2009) – CEO personality traits  
  - **Multifactor Leadership Questionnaire (MLQ-5X)** – leadership style  
  - **Innovative Behavior Scale** (Janssen, 2000; Salessi & Etchevers, 2020)  
  - **Innovation Capabilities Scale** (Calik et al., 2017) – firm-level innovation  
- **Statistical analysis:**  
  - Descriptive statistics, reliability tests (Cronbach’s α, KMO, Bartlett’s test)  
  - **Structural Equation Modeling (SEM)** using the `lavaan` package in R  
  - Fit indices: CFI = 0.92, TLI = 0.90, RMSEA = 0.09, SRMR = 0.07  
  - Software: **R 4.1.2**, packages `lavaan`, `psych`, `GPArotation`, and `corrplot`  

---

## Theoretical Framework
This repository operationalizes a **theory-driven model** integrating:  
1. **HEXACO Personality Framework** — Openness and Conscientiousness as core CEO traits  
2. **Transformational Leadership Theory** — Leadership as the behavioral mediator  
3. **Innovative Behavior Theory** — Mechanisms linking leadership and innovation  
4. **Dynamic Capabilities Theory (Teece, 2007)** — Innovation capabilities as firm-level outcomes  

---

## Key Findings
- **Openness to Experience** (β = .27, *p* < .001) and **Conscientiousness** (β = .45, *p* < .001) → **Transformational Leadership**  
- **Transformational Leadership** (β = .81, *p* < .001) → **Innovative Behavior**  
- **Innovative Behavior** (β = .57, *p* = .001) → **Innovation Capabilities**  
- Sequential mediation confirmed: personality → leadership → behavior → innovation  

These results underscore the central role of CEO leadership in converting personal traits into organizational innovation outcomes.

---


---

## Data Availability
All datasets and R scripts used for this study are **freely available** at this repository and archived on Zenodo:  
**DOI:** [10.5281/zenodo.17516039](https://doi.org/10.5281/zenodo.17516039)

---

## Citation
If you use this repository, please cite:  

> Parodi Camaño, T. A., Vidal Durango, J. V., & Portnoy, I. (2025).  
> *CEO Dimensions and Their Impact on Innovation Capabilities: An Empirical Study of Colombian MSMEs.*  
> Universidad de Córdoba & Universidad de la Costa.  
> DOI: [10.5281/zenodo.17516039](https://doi.org/10.5281/zenodo.17516039)

---

## Acknowledgements
We thank the CEOs who voluntarily participated in this study and the research groups at **Universidad de Córdoba** and **Universidad de la Costa** for their support during the data collection phase.  
Special thanks to **Professor Adriano Eurípedes Martins** for his valuable feedback during the conceptual development of this research.

---

## License
This repository is released under the **MIT License**, an [OSI-approved open-source license](https://opensource.org/licenses/MIT).

You are free to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of this software, provided that the original authors are credited.

**Software availability statement (per F1000 policy):**  
• Source code available from: [https://github.com/iportnoy1/CEO-Dimensions-and-Their-Impact-on-Innovation-Capabilities-An-Empirical-Study-of-Colombian-MSMEs-](https://github.com/iportnoy1/CEO-Dimensions-and-Their-Impact-on-Innovation-Capabilities-An-Empirical-Study-of-Colombian-MSMEs-)  
• Archived software available from: [https://doi.org/10.5281/zenodo.17516039](https://doi.org/10.5281/zenodo.17516039)  
• License: [MIT License](https://opensource.org/licenses/MIT)
---
