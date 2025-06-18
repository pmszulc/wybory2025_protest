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

benford1(wyb20, "Duda_2", "Liczba głosów, Duda, 2. tura 2020", limit = NA)
wyb20 %>% 
  filter(Duda_2 < 1500) %>% 
  ggplot(aes(Duda_2)) +
  geom_histogram()
wyb25 %>% 
  filter(Nawrocki_2 < 1500) %>% 
  ggplot(aes(Nawrocki_2)) +
  geom_histogram()


wyb20 %>% summarise(sum(Duda_2 > 1500)) # tylko tyle pominąłem
wyb20 %>% summarise(mean(Duda_2 > 99 & Duda_2 < 1000))
wyb25 %>% summarise(mean(Nawrocki_2 > 99 & Nawrocki_2 < 1000))

wyb20 %>% summarise(mean(Duda_2 >= 1000))
wyb25 %>% summarise(mean(Nawrocki_2 >= 1000))



# Zależność zwiększonej liczby głosów nieważny i poparciem dla Trzaskowskiego w I turze

wyb25proc %>% 
  filter(Nniew2x_1 > 0, Nniew2x_2 > 0, Trzaskowski_1 < 60) %>% 
  mutate(wzrost2x = Nniew2x_2 / Nniew2x_1) %>% 
  pivot_longer(c(Trzaskowski_1, Trzaskowski_2)) %>% 
  ggplot(aes(value, wzrost2x, col = name)) +
  #geom_point() +
  geom_smooth() +
  scale_y_log10()
# ale czy to wpłynęło negatywnie? może tam, gdzie był największy wzrost 2x,
# Trzaskowski dostał najwięcej?

wyb25proc %>% 
  filter(Nniew2x_1 > 0, Nniew2x_2 > 0) %>% 
  mutate(wzrost2x = Nniew2x_2 / Nniew2x_1) %>% 
  ggplot(aes(Mentzen_1, wzrost2x)) +
  #geom_point() +
  geom_smooth() +
  scale_y_log10()
















