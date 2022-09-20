gapminder = read.csv(here::here("~/Downloads/bootcamp-2022-main/data/gapminder5.csv"), stringsAsFactors = FALSE)
gapminder

year = 2012
if(any(gapminder$year==year)){
  print(paste("There are records for year", year))
} else {
  print(paste("There are no records for year", year))
}

country = unique(gapminder$country)
overall_mean = mean(gapminder$pop)
for (i in country) {
  if (mean(gapminder$pop[gapminder$country==i]) < overall_mean) {
    mean_le = mean(gapminder$lifeExp[gapminder$country==i])
    print(paste("Mean Life Expectancy in", i, "is", mean_le))
  } 
}

continent = unique(gapminder$continent)
for (i in continent) {
  if (mean(gapminder$lifeExp[gapminder$continent==i]) < 50) {
    print(paste(i,"'s mean life expectancy is smaller than 50."))
  } else if (mean(gapminder$lifeExp[gapminder$continent==i]) > 70) {
    print(paste(i,"'s mean life expectancy is greater than 70."))
  } else {
    print(paste(i,"'s mean life expectancy is between 50 and 70."))
  }
}

my_function <-
  function(dataset) {
    col = colnames(dataset)
    for (i in col) {
      print(paste("name: ", i, " classes: ", mode(dataset[,i])))
    }
  }

my_function(gapminder)

my_function2 <-
  function(vec, include_median=FALSE) {
    print(paste("The mean of the vector is ", mean(vec), " and the standard deviation of the vector is ", sd(vec)))
    print(paste("The standard deviation of the vector is ", sd(vec)))
    if (include_median) {
      print(paste("The median of the vector is ", median(vec)))
    }
  }

my_function2(c(1,1,2,2,3,4,5,6), TRUE)

gapminder

plot(log(gdpPercap)~log(lifeExp),data=gapminder)



