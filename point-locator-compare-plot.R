library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

b <- read.csv('/tmp/bench', skip = 10) %>%
	mutate(name = str_remove(str_replace(name, fixed('<'), '/'), fixed('>'))) %>%
	separate(name,
			 into = c('benchmark', 'impl', 'npts', 'ntests'),
			 sep = '/',
			 convert = TRUE)

ggplot(b, aes(x = ntests, y = cpu_time, col = impl)) +
	geom_line() +
	facet_wrap(~npts) +
	scale_y_log10() +
	scale_x_log10()

b %>%
	select(impl, npts, ntests, cpu_time) %>%
	pivot_wider(names_from = impl,
				values_from = cpu_time) %>%
	mutate(ipa_wins = IndexedPointInAreaLocator < SimplePointInAreaLocator) %>%
	filter(ipa_wins) %>%
	group_by(npts) %>%
	slice_min(ntests) %>%
ggplot(aes(x = npts, y = ntests)) +
	geom_line() +
	expand_limits(y = 0) +
	labs(x = 'Number of vertices in prepared polygon (sine star)',
		 y = 'Number of PIP tests to offset IPA creation',
		 title = 'When does IndexedPointInAreaLocator outperform SimplePointInAreaLocator?')
	
	
