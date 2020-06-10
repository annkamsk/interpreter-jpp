# Interpreter

Uruchomienie:
```
    make
    stack exec "Interpret-exe" <path-to-testfile>  
```
Testy:
```
    make
    ./test.sh
```
Skrypt sprawdza różnicę między oczekiwanym a aktualnym wyjściem programu, więc jeśli nie pokazuje żadnych różnic, 
to znaczy, że zakończył się pomyślnie (również jeśli w przypadku testów z `mybad/` program zakończył się błędem).

### Funkcje
 - Możliwość przekazywania przez zmienną (`mygood/pass_by_var.latb`) i przez wartość (większość pozostałych przykładów).
 - Funkcja o nazwie `main` stanowi punkt wejścia do programu: jej definicja od razu uruchamia wszystkie zawarte w niej instrukcje
 - Możliwa rekurencja
 - Funkcja `print` przyjmuje argumenty dowolnego typu (tj. int, string, boolean, Array, funkcje i null 
 (ten typ mają tylko niezainicjalizowane zmienne)) i wyświetla ich stringową reprezentację (test: `mygood/print_any_type.latb`)
 
### Array
- Indeksowane od 0
- Zmienne typu `Array` są niemodyfikowalne - operacje get, set, push i remove zwracają nową tablicę
- Operacja len zwraca długość tablicy
- Operacje get, set i remove oczekują, że element pod indeksem, do którego się odwołują, będzie istniał. 
W przeciwnym razie program kończy się błędem i komunikatem "Index out of bounds" (testy: `mybad/array_remove_out_of_bounds.latb`, `mybad/array_set_out_of_bounds.latb`)
- Operacja `push` umożliwia dodawanie nowych elementów
- Array może zawierać elementy dowolnego typu: również typu Array (test: `mygood/array_2d.latb`)    
 
### Typechecking
- Sprawdzanie poprawności typów i deklaracji zmiennych następuje przed uruchomieniem programu
- Jeśli na tym etapie wystąpił dowolny błąd, program nie zostanie uruchomiony i zwrócony zostanie komunikat "Failure by type checking"
- Typechecker stara się znaleźć jak najwięcej błędów: w przypadku znalezienia błędu zbiera komunikat o nim, 
ale kontynuuje sprawdzanie reszty programu tak, jakby sprawdzany typ był taki jak oczekiwany 
(lub jeśli nie ma takiej możliwości, to zakłada jakiś domyślny typ)
- Typechecker kończy sprawdzanie, jeśli sprawdzi cały program albo napotka niezadeklarowaną zmienną 
lub wywołanie funkcji ze złą liczbą argumentów

### Zmienne
- Możliwa jest powtórna deklaracja zmiennych - typechecking wówczas zwróci warning, ale pozwoli programowi działać dalej
- Jeśli ponowna deklaracja dzieje się wewnątrz bloku while, to zmienna zewnętrzna jest przysłaniana na czas trwania bloku
- testy: `mygood/local_scope_var_change.latb`, `mygood/local_scope_var.latb`



