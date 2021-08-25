# Predicting-Wage-of-FIFA-Soccer-Player
Situation: 
Soccer is considered almost the most influential sport all over the world. And FIFA, the International Federation of Association Football, was founded to oversee international competition among nations. 

Task:
This research targets on predict FIFA soccer players’ wages based on 12745 players with 80 attributes, such as players’ overall, special, potential scores, and so on, reflecting player’s strength and ability. The data was provided by FIFA Website. 

Action:
1. Data exploration
I found that the response variable Wage has a range from 6 to 650305 with a mean value of 11433. This large range implies that there are some outliers that we need to examine more closely. The players with extremely low wages are those who have NA in the variable Club. This finding suggests that NAs in the variable Club are useful, and Club is a good candidate to predict the response variable Wage because players who do not in a club would have a mean wage significantly lower than those in the clubs.

2. Methodology
1)	Instead of deleting NAs in Club, I replaced the “NA” values with a new category “None”. To make the model more valid and not influenced by extreme values, I removed the players with wages less than 1000 since such a low annual wage does not make sense and is not reasonable. 
2)	I used a correlation matrix to examine the correlation between numerical predictors and the response variable Wage. The variables Overall, Potential, Special are highly correlated with Wage, indicating that they can be good variable candidates for the model. 
3)	After selecting variables, I performed power transformation on the variables Wage, Club, Overall, Potential, and Special to stabilize variances and improve the validity of measures of association. 
4)	I plotted the diagnostics plots to examine the validity of the model. After examined the diagnostic plots, I decided to use weighted least squares to solve the non-constant variance issue of the variable Overall by adding different weights on it.

Result:
This multiple linear regression model has an R squared of 0.95, indicates that the model explains 95% variability of the response data around its mean. Using this model, we can predict a FIFA soccer players’ wages base on his performance information.
