benford_df <- tibble(
  dig = as.character(0:9),
  benford1 = c(0, log10(1 + 1 / (1:9))),
  benford2 = c(0.11968, 0.11389, 0.10882, 0.10433, 0.10031, 0.09668,
    0.09337, 0.09035, 0.08757, 0.08495)
)

benford1 <- function(df, var, title = "", breaks = 0.1, limit = 0.4) {
  df %>% 
    rename(N = all_of(var)) %>% 
    filter(N > 0) %>% 
    mutate(N = str_sub(N, 1, 1)) %>% 
    count(N) %>% 
    mutate(Dane = n / sum(n)) %>% 
    left_join(benford_df, by = c("N" = "dig")) %>% 
    rename(`Rozkład Benforda` = benford1) %>% 
    pivot_longer(c(Dane, `Rozkład Benforda`), 
      names_to = "źródło", values_to = "proporcja") %>% 
    mutate(źródło = fct_relevel(źródło, "Dane")) %>% 
    ggplot(aes(N, proporcja, fill = źródło)) +
    geom_col(position = "dodge") +
    labs(x = "Pierwsza cyfra", y = "Częstotliwość", title = title, fill = "") +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5)
    ) +
    scale_fill_manual(values = c("#0072B2", "#56B4E9")) +
    scale_y_continuous(labels = percent_format(), breaks = seq(0, 0.4, by = breaks),
      limits = c(0, limit))
}

benford2 <- function(df, var, title = "", breaks = 0.02, limit = 0.12) {
  df %>% 
    rename(N = all_of(var)) %>% 
    filter(N > 10) %>% 
    mutate(N = str_sub(N, 2, 2)) %>% 
    count(N) %>% 
    mutate(Dane = n / sum(n)) %>% 
    left_join(benford_df, by = c("N" = "dig")) %>% 
    rename(`Rozkład Benforda` = benford2) %>% 
    pivot_longer(c(Dane, `Rozkład Benforda`), 
      names_to = "źródło", values_to = "proporcja") %>% 
    mutate(źródło = fct_relevel(źródło, "Dane")) %>% 
    ggplot(aes(N, proporcja, fill = źródło)) +
    geom_col(position = "dodge") +
    labs(x = "Druga cyfra", y = "Częstotliwość", title = title, fill = "") +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5)
    ) +
    scale_fill_manual(values = c("#0072B2", "#56B4E9")) +
    scale_y_continuous(labels = percent_format(), breaks = seq(0, 0.4, by = breaks),
      limits = c(0, limit))
}
