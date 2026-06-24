# Digital Donation UX Optimization — A/B Simulation & Statistical Analysis

## Overview
Designed and executed A/B simulation models to evaluate donor behavior and identify 
optimal preset donation configurations to maximize conversion revenue. Used Monte Carlo 
simulation, hypothesis testing, and power analysis to produce statistically rigorous 
recommendations for digital donation UX design.

## Tools & Methods
- **Language:** R
- **Statistical Tests:** Proportion test, Student's t-test, Welch's t-test
- **Simulation:** Monte Carlo simulation (1,000 iterations per scenario)
- **Additional:** Power analysis (Cohen's d), 95% confidence intervals, 
  Type I/II error classification

## Research Questions
1. Does the size of a preset donation amount (small vs. large) affect donation frequency?
2. Does the preset donation amount influence the average donation value?

## Key Results
| Analysis | Method | Sample Size |
|----------|--------|-------------|
| Donation frequency comparison | Proportion test | n = 1,102 |
| Average donation value comparison | T-test + Monte Carlo (1,000 runs) | n = 551 per group |
| Power analysis | Cohen's d, 80% power threshold | Calculated per scenario |

## Methodology Highlights
- Simulated control and experiment groups across 1,000 iterations to assess 
  result stability
- Calculated Type I error rates (false positives) and Type II error rates 
  (false negatives) across scenarios
- Applied power analysis to determine minimum sample size requirements 
  before drawing conclusions
- Used 95% confidence intervals throughout to quantify uncertainty in effect 
  size estimates

## Project Structure
- `APAN_5300_Final_Project_Simulation_Code.R` — Full simulation script including 
  proportion tests, t-tests, Monte Carlo simulation, power analysis, and 
  results visualization

## Background
This project was completed as part of the APAN 5300 course in the MS in Applied 
Analytics program at Columbia University. The goal was to apply simulation-based 
statistical methods to a real-world UX decision: which preset donation amounts 
drive the highest conversion and revenue for a digital fundraising platform.

## Author
**An Truong**  
MS Applied Analytics, Columbia University  
[LinkedIn](https://linkedin.com/in/antruong9699) | 
[Portfolio](https://antruong-portfolio.netlify.app)
