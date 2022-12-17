library(stringr)
library(dplyr)
library(purrr)
library(ggplot2)
library(tibble)
library(tidyr)

repo <- '/home/dan/dev/libgeos'
branches <- c(
  'main',
  'contains-delegate-prepared'
  #'prepared-polygon-segstr-alloc'
  #'segmentnodelist-bulk-add'
  #'concrete-coordseq',
  #'math-flags'
  #'concrete-coordseq-buf',
  #'segstr-devirt'
)
  #'concrete-coordseq-multi')

test_geosop <- function(a, b, r = 1, args, lib_dir) {
  args <- c('-q', '-t', '-r', r, '-a', a, args)

  geosop_out <- system2('bin/geosop', args,
                        stdout = TRUE,
                        stderr = TRUE,
                        env = sprintf('LD_LIBRARY_PATH=%s', lib_dir))
  geosop_out |>
    str_extract('[\\d,]+(?= usec)') |>
    str_remove_all(fixed(',')) |>
    as.numeric() / 1e6
}

test_pip <- function(a, b, r, lib_dir, args) {
  if (args == 'reuse point') {
    index <- 2
  } else {
    index <- 1
  }

  cmd_args <- c(a, r)

  pip_out <- system2('bin/perf_geospreparedcontains', cmd_args,
                     stdout = TRUE,
                     stderr = TRUE,
                     env = sprintf('LD_LIBRARY_PATH=%s', lib_dir))

  times <- pip_out |>
    str_extract('(?<=in )[\\d,]+$') |>
    discard(is.na) |>
    str_remove_all(fixed(',')) |>
    as.numeric()

  times[index] / 1e3
}

test_intersection <- function(a, b, r, lib_dir, args) {
  cmd_args <- c(a, args)
  out <- system2('bin/perf_intersection', cmd_args,
                 stdout = TRUE,
                 stderr = TRUE,
                 env = sprintf('LD_LIBRARY_PATH=%s', lib_dir))
  time <- out[length(out)] |>
    str_remove_all(fixed(',')) |>
    as.numeric()

  time / 1e6
}

test_bench <- function(a, b, r, lib_dir, args) {
  out <- system2(file.path('bin', args[1]),
                 sprintf('--benchmark_filter="%s"', args[2]),
                 stdout = TRUE,
                 stderr = TRUE,
                 env = sprintf('LD_LIBRARY_PATH=%s', lib_dir))
  time <- as.integer(str_extract_all(out[length(out)], '\\d+')[[1]][2])
}

benchmarks <- tribble(
  ~name, ~times, ~fn, ~a, ~b, ~r, ~args,
  #'pip watersheds (GEOSPreparedContainsXY)',    10, test_pip,    '~/data/wsa_individual.wkt', NA, 500, 'reuse point',
  #'pip watersheds (GEOSPreparedContains)',    10, test_pip,    '~/data/wsa_individual.wkt', NA, 500, 'create new point',
  #'buffered watershed union',       3, test_geosop, '~/data/wsa_individual_buf.wkt', NA, 1, 'unaryUnion',
  #'watershed union',    5, test_geosop, '~/data/wsa_individual.wkt', NA, 1, 'unaryUnion',
  #'cluster countries', 10, test_geosop, '~/data/world.wkt',          NA, 5, c('clusterWithin', '1e-3'),
  #'australia isvalid',  5, test_geosop, '~/data/australia.txt', NA, 5, 'isValid',
  #'watersheds isvalid', 5, test_geosop, '~/data/wsa_individual.wkt', NA, 5, 'isValid',
  #'landcov isvalid',    5, test_geosop, '~/data/invalid_land_cover.wkt', NA, 5, 'isValid',
  #'buffered watershed intersection', 5, test_intersection, '~/data/wsa_individual_buf.wkt', NA, NA, 5000,
  #'watershed intersection', 5, test_intersection, '~/data/wsa_individual.wkt', NA, NA, 5000,
  #'triangle intersection (from papua.wkt)', 25, test_intersection, '~/data/triangles.wkt', NA, NA, 5000,
  #'country intersection (world.wkt)', 25, test_intersection, '~/data/world.wkt', NA, NA, 5000,
  #'stream line intersection', 25, test_intersection, '~/data/vt_orange_hydro.wkt', NA, NA, 5000,
  #'rotated stream line intersection', 25, test_intersection, '~/data/vt_orange_hydro_rot.wkt', NA, NA, 5000,
  #'australia buffer',   3, test_geosop, '~/data/australia.txt',      NA, 1, c('buffer', '1e-3'),
  #'county buffer',      3, test_geosop, '~/data/tl_2021_counties.wkt', NA, 1, c('buffer', '1e-3'),
  #'2d point query', 15, test_bench, NA, NA, NA, c('perf_spatial_index', 'BM_STRtree2DQuery<TemplateSTRtree')
  'tx county-mcd contains', 10, test_intersection, '~/data/tx_county_and_mcd.txt', NA, NA, c('all', 'contains', 'none')
)

get_arg <- function(args, arg) {
  ind <- which(args == arg)
  if (length(ind) == 0) {
    return(NA_character_)
  } else {
    return(args[ind + 1])
  }
}

get_branch_dirs <- function(repo) {
  treelist <- system2('git',
                      c('-C', repo, 'worktree', 'list'),
                      stdout = TRUE)
  branches <- str_match(treelist, '\\[([\\w.-]+)\\]')[,2]
  dirs <- str_extract(treelist, '^[^\\s]+')

  ret <- as.list(dirs[!is.na(branches)])
  names(ret) <- branches[!is.na(branches)]
  ret
}

branch_dirs <- get_branch_dirs(repo)

for (branch in branches) {
  build_dir <- file.path(branch_dirs[[branch]], 'cmake-build-release')
  setwd(build_dir)
  system2('ninja', c('geosop',
                     'perf_geospreparedcontains',
                     'perf_intersection',
                     'perf_spatial_index'), stdout = FALSE)
}

results <- map_dfr(seq_len(nrow(benchmarks)), function(row) {
  bm_name <- benchmarks[row, ]$name
  bm_iter <- benchmarks[row, ]$times
  bm_args <- benchmarks[row, ]$args[[1]]
  bm_fun <- benchmarks[row, ]$fn[[1]]
  bm_a <- benchmarks[row, ]$a
  bm_b <- benchmarks[row, ]$b
  bm_r <- benchmarks[row, ]$r

  map_dfr(seq_len(bm_iter), function(i) {
    map_dfr(sample(branches), function(branch) {
      build_dir <- file.path(branch_dirs[[branch]], 'cmake-build-release')

      setwd(build_dir)

      lib_dir <- file.path(build_dir, 'lib')

      cat(bm_name, ': ', branch)
      time_elapsed <- bm_fun(a = bm_a, b = bm_b, r = bm_r, args = bm_args, lib_dir=lib_dir)
      cat(' ', time_elapsed, '\n')

      data.frame(name = bm_name,
                 branch = branch,
                 a = bm_a,
                 b = bm_b,
                 args = paste(bm_args, collapse = ' '),
                 time = time_elapsed)
    })
  })
})

results %>%
  #mutate(branch = if_else(branch == 'main', 'main', 'this pr')) %>%
  ggplot() +
  geom_jitter(aes(x = branch, y = time), width = 0.1) +
  facet_wrap(~name, scales = 'free') +
	expand_limits(y = 0)

results %>%
  group_by(branch, name) %>%
  summarise(time = mean(time)) %>%
  pivot_wider(name,
              names_from = branch,
              values_from = time) %>%
  #rename(this_pr = `concrete-coordseq-buf`) %>%
  mutate(speedup_pct = 100 *(`prepared-polygon-segstr-alloc` - `intersects-delegate-prepared`) / `intersects-delegate-prepared`)
