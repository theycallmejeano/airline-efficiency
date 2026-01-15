# Airline Efficiency

Code for two-stage network DEA analysis examining airline operational efficiency (2018-2024), using the R package [deaR](https://cran.r-project.org/web/packages/deaR/deaR.pdf)

## Files and Folders
- `order_book_cleaning.R` - Data wrangling and preparation
- `final_model_DEA.R` - DEA analysis and regression modeling
- `utilities.R` - Helper functions
- `data/` - Input data (manufacturer order books, airline financial reports)

## Method

Two-stage network DEA decomposing airline operations:
- **Stage 1:** Fleet + Operating Expenses = Available Seat Kilometers (ASK)
- **Stage 2:** ASK + Labor = Revenue + Load Factor