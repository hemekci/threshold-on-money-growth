Here’s a ready-to-use `README.md` file for your GitHub repo:

```markdown
# Is there a threshold on money growth?

Replication files for the article:

**Başçı, E., Emekci, H., & Apaydın, M. O. (2025). "Is there a threshold on money growth?"**

---

## 📖 Overview
This repository provides replication data and R scripts for the study analyzing threshold effects of money growth on inflation in G20 countries (1999–2023). Hansen’s (2017) regression kink model is applied to identify non-linear monetary transmission.

---

## 📂 Repository Structure
```

├── data/                # Processed and raw datasets (Nominal GDP, Real GDP, M3)
├── scripts/             # R codes for data preparation, models, tests, and figures
│   ├── 01\_data\_prep.R
│   ├── 02\_linear\_model.R
│   ├── 03\_threshold\_model.R
│   ├── 04\_bootstrap\_tests.R
│   └── 05\_figures.R
├── results/             # Replication outputs (tables and figures)
└── README.md            # Project documentation

````

---

## 🔧 Requirements
R version ≥ 4.2 with the following packages:

```R
install.packages(c("tidyverse", "plm", "boot", "lmtest", "sandwich"))
````

---

## 🚀 Usage

1. Clone the repository:

   ```bash
   git clone https://github.com/username/money-threshold.git
   cd money-threshold
   ```
2. Run the scripts in order (`01_data_prep.R` → `05_figures.R`).
3. Results (tables, bootstrap tests, figures) will be saved in `/results`.

---

## 📊 Data Sources

* [World Bank – World Development Indicators](https://data.worldbank.org)
* [OECD Data Portal](https://data-explorer.oecd.org)
* [Bank of Russia](https://www.cbr.ru)
* [Central Bank of Argentina](https://www.bcra.gob.ar)

---

## ✍️ Citation

If you use this code or data, please cite:

Başçı, E., Emekci, H., & Apaydın, M. O. (2025). *Is there a threshold on money growth?*
\[Journal name, volume(issue), pages] (forthcoming).

---

## 📜 License

The code is released under the [MIT License](LICENSE).
The dataset is available under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

```

---

👉 Do you want me to also generate a ready-to-drop **`LICENSE` file in MIT format** so you don’t have to create it manually?
```
