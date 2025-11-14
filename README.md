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

## Data Availability

All data underlying the results reported in this study are freely available under the **Creative Commons Attribution 4.0 International (CC BY 4.0)** license.  
The datasets include all variables, descriptive statistics, and raw values used to compute means, standard deviations, and figures reported in the paper, as well as the data required to replicate the structural equation modeling (SEM) and correlation analyses.

No embargo applies. All datasets are openly accessible and include the numerical values used to compute reported statistics, construct figures, and reproduce all analyses.  
No data were excluded for privacy, ethical, or security reasons.

---

The archived repository includes all R source files, scripts for SEM model replication, figure generation, and dataset documentation, ensuring full reproducibility of the results reported in this paper.

---

## License
- Data is licensed under CC BY 4.0.
- Software is licensed under MIT.
