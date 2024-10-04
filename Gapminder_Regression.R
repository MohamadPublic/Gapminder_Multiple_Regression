library(plotly)

# Read in the data
gapminder_data = read.csv("gapminder.csv", header = TRUE)

# Clean the data by removing missing values
gapminder_data_clean = na.omit(gapminder_data)

# Filter for the year 2011
clean_2011 = subset(gapminder_data_clean, year == 2011)

# Create the linear model
model = lm(life_expectancy ~ infant_mortality + fertility, data = clean_2011)

summary(model)

# Add the regression plane
clean_2011$predicted_life_expectancy = predict(model, clean_2011)

# Create 3D scatter plot with regression plane
fig = plot_ly(clean_2011, 
               x = ~infant_mortality, 
               y = ~fertility, 
               z = ~life_expectancy, 
               type = 'scatter3d', 
               mode = 'markers', 
               marker = list(size = 5, color = 'blue')) 


fig = fig %>% add_trace(
  z = ~predicted_life_expectancy, 
  x = ~infant_mortality, 
  y = ~fertility, 
  type = 'mesh3d', 
  opacity = 0.5
)

fig = fig %>% layout(scene = list(
  xaxis = list(title = "Infant Mortality"),
  yaxis = list(title = "Fertility"),
  zaxis = list(title = "Life Expectancy")),
  title = "Life Expectancy as a function of Infant Mortality and Fertility (2011)"
)

fig

# Customize the layout
fig = fig %>% layout(scene = list(
  xaxis = list(title = "Infant Mortality"),
  yaxis = list(title = "Fertility"),
  zaxis = list(title = "Life Expectancy")),
  title = "Life Expectancy as a function of Infant Mortality and Fertility (2011)"
)

# Show plot
fig


################################## Direct Solution ############################

gapminder_data = read.csv("gapminder.csv", header = TRUE)

# Clean the data by removing missing values
gapminder_data_clean = na.omit(gapminder_data)

# Filter for the year 2011
clean_2011 = subset(gapminder_data_clean, year == 2011)

# Create matrix X (with infant mortality and fertility columns)
X = as.matrix(cbind(1, clean_2011[, c("infant_mortality", "fertility")]))

# Create vector y (with life expectancy values)
y = clean_2011$life_expectancy

beta_hat = solve(t(X) %*% X) %*% t(X) %*%y

beta_hat
