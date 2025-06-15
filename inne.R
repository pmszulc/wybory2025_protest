# Istnieje rozszerzenie prawa Benforda na inne cyfry niż pierwsza. Poniżej
# porównanie drugiej cyfry dla liczby uprawnionych z rozkładem Benforda (jest on
# inny, niż w przypadku pierwsze cyfry).

wyb25 %>% 
  filter(Nupr_2 > 0) %>% 
  benford2("Nupr_2", "Liczba uprawnionych do głosowania, 2. tura 2025")

n_dane <- wyb25 %>% 
  filter(Nupr_2 > 10) %>% 
  mutate(Nupr_2 = str_sub(Nupr_2, 2, 2)) %>% 
  count(Nupr_2) %>% 
  pull(n)
prop_benf <- benford_df$benford2 / sum(benford_df$benford2) # nie sumuje się do 1
chisq.test(x = n_dane, p = prop_benf)

benford2(wyb25, "Trzaskowski_2", "Liczba głosów, Trzaskowski, 2. tura 2025")
benford2(wyb25, "Nawrocki_2", "Liczba głosów, Nawrocki, 2. tura 2025")
benford2(wyb20, "Duda_2", "Liczba głosów, Duda, 1. tura 2020")