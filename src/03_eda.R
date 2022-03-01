

#DataExplorer::plot_correlation(data1)
DataExplorer::create_report(data1)

data1 %>% head

plot_histogram(data1)
plot_density(data1)
plot_boxplot(data1, by = "target")

plot_qq(data1, by = "target")

