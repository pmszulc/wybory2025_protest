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
wyb20 %>% summarise(sum(Duda_2 > 1500)) # tylko tyle pominąłem
wyb20 %>% summarise(mean(Duda_2 > 99 & Duda_2 < 1000))
# podobnie z Bidenem
  
wyb25 %>% 
  filter(Trzaskowski_2 < 1500) %>% 
  ggplot(aes(Trzaskowski_2)) +
  geom_histogram()




wyb25 %>% 
  count(Frekwencja_2_zaokr) %>% 
  ggplot(aes(Frekwencja_2_zaokr, n)) +
  geom_point() +
  geom_line()

wyb25 %>% 
  count(Frekwencja_1_zaokr) %>% 
  filter(Frekwencja_1_zaokr >= 92) %>% 
  ggplot(aes(Frekwencja_1_zaokr, n)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, NA))

wyb25 %>% 
  filter(Frekwencja_2_zaokr >= 99) %>%
  select(Typ_obwodu, Siedziba, Frekwencja_1, Frekwencja_2, Nupr_1, Nupr_2) %>% 
  view()
# jest część stałych obwodów z dużą frekwencją i wysokim Nupr, ale część powinno takie być
# naturalnie (przedłużenie rozkładu). Policz ile i sprawdź (około 20 dla 99 i 100%)






















