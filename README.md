# Nicaraguan Banking System Indicators (NBSI)

## Project Overview

Nicaragua is a small, open economy in Central America with 2024 GDP per capita of US$2,865. Its banking sector remains shallow—total gross loans amount to just 31.9 percent of GDP—yet it ranks among the most profitable in the region. This project presents a comprehensive set of banking-system indicators built from publicly available data produced by the *Superintendencia de Bancos y Otras Instituciones Financieras* (SIBOIF).

The repository includes:

- Download and preprocessing script for SIBOIF banking-system data — [View code](https://github.com/bejarano-ch/Nicaraguan-Banking-System-Indicators/blob/main/codes/download_data_SIBOIF.R) 
- Download official exchange-rate data from the Central Bank of Nicaragua to convert monetary series — [View code](https://github.com/bejarano-ch/Nicaraguan-Banking-System-Indicators/blob/main/codes/download_exchange_rate.R)    
- Create interactive Tableau dashboards:  
  - Statement of Financial Position — [View dashboard](https://public.tableau.com/app/profile/christian.bejarano5490/viz/StatementofFinancialPosition/Dashboard)  
  - Income Statement — [View dashboard](https://public.tableau.com/app/profile/christian.bejarano5490/viz/IncomeStatement_17496622854550/Dashboard1)

## Project Structure

```
├── codes/           # R scripts for data processing and analysis
│   ├── download_data_SIBOIF.R    # Script for downloading SIBOIF data
│   └── download_exchange_rate.R   # Script for downloading exchange rate data
├── data/           # Raw and processed data files
├── tableau/        # Tableau workbooks and visualizations
└── NBSI.Rproj      # R project configuration
```

## Contact

Christian Bejarano - christian.bejarano.ch@gmail.com

Project Link: [https://github.com/bejarano-ch/Nicaraguan-Banking-System-Indicators](https://github.com/bejarano-ch/Nicaraguan-Banking-System-Indicators) 