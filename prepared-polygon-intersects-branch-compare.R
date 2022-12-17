x <- read.csv('/tmp/zoz') %>% mutate(branch = 'proposed')
y <- read.csv('/tmp/qqp') %>% mutate(branch = 'main')

bind_rows(x, y) %>%
	mutate(test_type = str_c('Testing against ', test_type)) %>%
ggplot(aes(x = target_points, y=time, col=method)) +
	geom_line() +
	facet_grid(branch~test_type) +
	scale_x_log10() + scale_y_log10() +
	labs(x = 'Number of vertices in prepared polygon')
