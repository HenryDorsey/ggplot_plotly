library(ggplot2)
library(plotly)
library(stringr)
library(tidyverse)
library(dplyr)
library(reshape)
library(usmap)
library(GGally)



# Data pulled from USAgovspending, American Community Survey, and the CDC
# Data is by county
data <- read.csv('~/Winter2021/Stat6367/Rgraphing.csv', stringsAsFactors = F)
data <- data[,2:length(colnames(data))]

colnames <- colnames(data)

# Grabs the time series data
ts_cols <- sapply(colnames, str_detect, pattern='X[0-9]{4}.[0-9]{2}.[0-9]{2}')
rename_ts_cols <- sapply(colnames, str_replace, pattern='X', replace='')
colnames(data) <- rename_ts_cols

non_ts_cols <- 1-ts_cols

county_0 <- data[1,ts_cols]

# Simplest non-ggplot plot
plot(data$people.with.private.health.insurance, data$unemployment.rate)

# Scatter Plot recreated in ggplot
g1 <- ggplot(data = data, mapping=aes(people.with.private.health.insurance, unemployment.rate)) + 
         geom_point()

g1

# OK so we can look at the data, but its ugly. Lets spruce it up
g1 <- g1 +  theme_bw() +
  labs(
    x = "People with Private Insurance",
    y = "Unemployment %",
    title = 'Analysis of Employment and Insurance',
    subtitle = "Unemployment by Private Insurance",
    caption = "Data courtesy of American Community Survey"
  )
g1

# Looks better
# We can add some additional information to the graph
g1 +
  stat_smooth(method = "loess",
                         col = "#990000",
                         se = F,
                         size = 1)
g1

# Ok maybe a little more?
g1 <- g1 + 
  geom_point(aes(color=State)) +
  stat_smooth(method = "loess",
                      col = "#990000",
                      se = T,
                      size = 1)
g1

# Parsimony is something to keep in mind whenever plotting results.
# Be clear and concise

# Histogram
gg_hist <- ggplot(data, mapping=aes(white))+
  geom_histogram(bins=30, col = "black", fill = "cadetblue3") + 
  theme_bw() +
  labs(title = 'Histogram of White',
       x='White')

gg_hist

# Looks like it could use a transform
# Can explore on the fly
ggplot(data, mapping=aes(white)) +
  geom_histogram(bins=30, col = "black", fill = "cadetblue3",  aes(x=log(white))) + 
  theme_bw() +
  labs(title = 'Histogram of log(White)',
       x = 'log(White)')


# Check Assumptions
pairs <- ggpairs(non_ts_data[,1:6], title='Pairwise Plots')
pairs

# Time Series
ts_data <- dplyr::select(data, which(non_ts_cols==0))
colnames(ts_data) <- sapply(colnames(ts_data), str_replace, pattern='X', replace='')

ts_data$countyFIPS <- data$countyFIPS
#ts_data$county_name <- data$county_name

ts_melt <- melt(ts_data, id.vars='countyFIPS')

set.seed(3)
counties <- sample(data$countyFIPS, 5)

ts_melt <- dplyr::rename(ts_melt, date=variable, infections=value)

ts_melt$date <- as.Date(ts_melt$date, format='%Y.%m.%d')

subsample <- dplyr::filter(ts_melt, countyFIPS%in%counties)
subsample$countyFIPS <- as.factor(subsample$countyFIPS)
gg_ts <- ggplot(subsample, aes(date, infections, color=countyFIPS)) +
  geom_line() +
  theme_bw() + 
  labs(title='Select County Inffections Over Time',
       xlab='Date',
       ylab='Infections')
  

gg_ts
  
# Facet Wrap
west_coast_states <- c('CA', 'OR', 'WA', 'NV', 'AZ', 'ID')
west_coast <- dplyr::filter(data, State%in%west_coast_states)
gg_facetwrap <- ggplot(west_coast, aes(employed.people.with.16.yrs.and.over, people.with.private.health.insurance)) + 
  geom_point() +
  geom_smooth(method='loess') +
  facet_wrap(~State)

gg_facetwrap


# Heatmap
non_ts_data <- dplyr::select(data, which(non_ts_cols==1))
non_ts_data <- non_ts_data[,sapply(non_ts_data, is.numeric)]
correlation <- cor(non_ts_data)
covariance <- cov(non_ts_data)
nts_colnames <- colnames(non_ts_data)

datamelt <- melt(correlation)

gg_heat<-ggplot(datamelt, aes(X1, X2)) +
  geom_tile(aes(fill=value)) +
  theme(axis.text.x = element_text(angle = 35, hjust=1))
  

# Fun package: usmap
tx <- dplyr::filter(non_ts_data, StateFIPS==48)
tx <- dplyr::rename(tx, fips=countyFIPS)

gg_tx <- plot_usmap(data = tx, values = "population", include = c("TX"), color = "red") + 
  scale_fill_continuous(low = "white", high = "red", name = "Population", label = scales::comma) + 
  labs(title = "Texas Pop", subtitle = "County Level") +
  theme(legend.position = "right")
  
gg_tx

# Plotly

# All of the graphs we've made so far can be made interactive using plotly, or more specifically, ggplotly

ggplotly(g1)

ggplotly(gg_hist)

ggplotly(gg_facetwrap)

ggplotly(gg_heat)

ggplotly(gg_tx)

# Extra Annotation
fig <- data %>%
  plot_ly(x = ~people.with.private.health.insurance, 
  y = ~unemployment.rate,
  split = ~State,
  text = paste("State",data$State,
                "<br>Population", data$population,
                "<br>White: ", data$white,
                "<br>Black: ", data$black.or.african.american,
                "<br>Native American: ", data$american.indian.and.alaska.native,
                "<br>Asian: ", data$asian,
                "<br>Hawaiian or Pacific Islander: ", data$native.hawaiian.and.other.pacific.islander,
                "<br>Other Races: ", data$other.races),
  hoverinfo=text,
  type = 'scatter',
  mode = 'markers'
)
fig

fig <- fig %>% layout(
  title = "Private Health Insurance by Unemployement",
  scene = list(
    xaxis = c(title = "Private Health Insurance"),
    yaxis = c(title = "Unemployement")
))
fig

# Animation
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

fig <- subsample %>%
  accumulate_by(~date)
fig$frame <- as.numeric(fig$frame)

fig <- fig %>%
  plot_ly(
    x = ~date, 
    y = ~infections,
    split = ~countyFIPS,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F)
  )
fig <- fig %>% layout(
  xaxis = list(
    title = "Date",
    zeroline = F,
    #autorange = T,
    range=list(min(subsample$date, max(subsample$date)))
  ),
  yaxis = list(
    title = "Infections",
    zeroline = F,
   # autorange=T,
    range=list(min(subsample$infections, max(subsample$infections)))
    
  )
) 
fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  hide = T
)
fig <- fig %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)

### Cool unrelated animations that showcase the power of plotly

n <- 100
theta <- runif(n, 0, 2*pi)
u <- runif(n, -1, 1)
fig <- plot_ly(x = ~sqrt(1 - u^2) * cos(theta), y = ~sqrt(1 - u^2) * sin(theta), z = ~u)
fig <- fig %>% layout(
  title = "Layout options in a 3d scatter plot",
  scene = list(
    xaxis = list(title = "Cos"),
    yaxis = list(title = "Sin"),
    zaxis = list(title = "Z")
  ))

fig

# wild
dat <- fromJSON(file='https://raw.githubusercontent.com/plotly/plotly.js/master/test/image/mocks/gl3d_cone-wind.json')

fig <- plot_ly(
  type="cone",
  x= dat$data[[1]]$x,
  y= dat$data[[1]]$y,
  z= dat$data[[1]]$z,
  u= dat$data[[1]]$u,
  v= dat$data[[1]]$v,
  w= dat$data[[1]]$w,
  text="-> wind <-",
  hoverinfo="u+v+w+text",
  marker = list(
    colorscale = "Viridis",
    cmin=0,
    cmax=100
  )
) 
fig <- fig %>%
  layout(
    scene= list(
      aspectratio= list(x= -1.57, y= 1.36, z= 0.58)
    )
  )

fig


