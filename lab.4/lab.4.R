library(rvest)

countries <- c("Netherlands", "Switzerland", "Croatia", "United Arab Emirates", 
               "Egypt")
colors <- rainbow(5)
years <- 2014:2021
mains = c('Индекс качества жизни (чем выше, тем лучше)',
          'Индекс покупательной способности (чем выше, тем лучше)',
          'Индекс безопасности (чем выше, тем лучше)',
          'Индекс медицинского обслуживания (чем выше, тем лучше)',
          'Индекс прожиточного минимума (чем ниже, тем лучше)',
          'Отношение цены на жильё к доходу (чем ниже, тем лучше)',
          'Индекс времени движения на дороге (чем ниже, тем лучше)',
          'Индекс загрязнения (чем ниже, тем лучше)',
          'Климатический индекс (чем выше, тем лучше)'
)

data_by_year <- function(year){
  url_by_year <- sprintf("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=%d", year)
  page_by_year <- read_html(url_by_year)
  table_node <- html_nodes(page_by_year, "table#t2")
  table <- as.data.frame(html_table(table_node))
  table <- table[-1]
  return(table)
}

all_countries <- lapply(years, data_by_year)
names(all_countries) <- years
  
select_data <- lapply(all_countries, subset, Country %in% countries)

select_data <- lapply(select_data, function(df) {
  df[df == "-"] <- NA
  return(df)
})


plot_every_indicator <- function(select_data, countries, years, colors) {
  
  indicators <- colnames(select_data[[1]])[-1]
  for (i in 1:length(indicators)) {
    indicator <- indicators[i]
    main <- mains[i]
    result <- t(`colnames<-`(do.call(cbind, lapply(select_data, function(df) {
      values <- as.numeric(df[[indicator]])
      df <- as.data.frame(values)
      rownames(df) <- countries
      df
    })), years))
    
    result <- as.data.frame(result)
    
    mn <- min(result, na.rm = TRUE)
    mx <- max(result, na.rm = TRUE)
    
    par(mar = c(5, 4, 4, 10), xpd = TRUE)
    
    plot(years, result[[1]], xlab = 'год', ylab = "значение",
         ylim = c(mn - 13, mx + 13), main = main, cex.main = 1,
         col = colors[1], type = 'o', lty = 1, pch = 19)
    
    lines(years, result$'Switzerland', type='o', col=colors[2], lty=1, pch = 19)
    lines(years, result$'Croatia', type='o', col=colors[3], lty=1, pch=19)
    lines(years, result$'United Arab Emirates', type='o', col=colors[4], lty=1, pch=19)
    lines(years, result$'Egypt', type='o', col=colors[5], lty=1, pch=19)
    
    legend("topright", inset = c(-0.45, 0), xpd = TRUE,
           legend = countries, col = colors, lty = 1, pch = 19,
           bty = "n", cex = 0.7)
  }
}

plot_every_indicator(select_data, countries, years, colors)