# Fake News Detection using NLP

## Overview

A lightweight R Shiny dashboard demonstrating how simple language patterns can be used to classify synthetic news examples. It is an educational NLP demo, not a fact-checking tool.

## Problem Statement

Explore how text features and word frequencies can support a basic classification demonstration while making the limitations of automated news analysis clear.

## Objectives

- Build an interactive dashboard for exploring a small news corpus.
- Demonstrate simple text classification and text statistics.
- Visualize label, category, article-length, and vocabulary patterns.

## Features

- Dashboard KPIs and interactive Plotly charts.
- News analyzer with a REAL/FAKE pattern prediction, confidence estimate, statistics, and detected keywords.
- Searchable, filterable dataset table with full-text inspection.
- NLP analytics including article lengths and the 15 most common non-stopwords.
- Reproducible generator for 500 fully synthetic articles.

## Technologies

R, Shiny, bslib, tidytext, ggplot2, Plotly, dplyr, and DT.

## Dataset

`data/news.csv` contains 500 synthetic records with an ID, title, category, source, short article text, and demo label. No real news articles are used.

## Project Structure

```text
fake-news-detection-nlp/
├── data/
│   └── news.csv
├── app.R
├── generate_data.R
├── requirements.R
└── README.md
```

## Installation

From this project folder, run:

```r
source("requirements.R")
source("generate_data.R")
```

`requirements.R` installs only packages that are missing.

## How to Run

Open this folder as the working directory in R or RStudio, then run:

```r
shiny::runApp()
```

## Limitations

The classifier uses smoothed word-frequency evidence learned once from a small synthetic dataset. The displayed confidence is a simple score, not a calibrated probability. It can reflect writing patterns only; it cannot determine whether real-world information is true or false.

## Future Improvements

- Evaluate the simple classifier with a held-out synthetic test set.
- Add user-friendly examples and clearer model evaluation metrics.
- Replace synthetic examples with a carefully licensed, quality-reviewed educational dataset.
