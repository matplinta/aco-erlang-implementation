# aco-erlang-implementation
Implementation of Ant Colony Optimization (ACO) in Erlang language. Solving travelling salesman problem (TSP). Project realized for Computational Intelligence class.

---
## About
Program finds near-optimal solution for travelling salesman problem (TSP) using Ant Colony Optimization algorithm. Structurally program consists of 4 types of processes (besides the main process). These are _node_, which quantity is equal to the number of cities defined in a [TSPLIB data file](http://elib.zib.de/pub/mp-testdata/tsp/tsplib/tsp/index.html), _ant_, which quantity is defined by the user, _technical ant_, which is an ant created solely to give evaporation and die orders to all nodes upon certain conditions met, and lastly _master_ which is a checking process for finding the best solution and other such utilities. 

## Usage
To compile: `./compile`.
To run:     `./run <path_to_tsplib> <ants> <iterations>`.

If you wish to run directly with _Erlang_: 
```
erl -pz ebin -s main start problems/oliver30.tsp 30 100
```
or in _Eshell_:
```
Eshell V10.6  (abort with ^G)
1> c(main), c(node), c(reader), c(ant), c(master).                % to compile
2> main:start().                % note to change initial parameters in start() method accordingly!
```




## Mechanizm działania
```erlang
node(NodeNo, DistanceTo, Pheromones) 
```
* każdy węzeł posiada informację o swoim numerze oraz mapach dystansu do każdego innego węzła oraz mapę wartości feromonu do innych węzłów
```erlang
ant(Master, NodesPids, Distance, Path) 
```
* mrówka posiada pid procesu mastera, mapę pidów każdego węzła oraz własny przebyty dystans wraz z listą przebytych węzłów
* w czasie drogi mrówka wysyła zapytanie do procesu obecnego węzła gdzie ma iść dalej, na co w odpowiedzi dostaje mapy dystansu i wartości feromonu; na ich podstawie mrówka oblicza prawdopodobieństwa każdej krawędzi wychodzącej od tego węzła
* kolejny węzeł na drodze mrówki wybierany(losowany) jest metodą _Roulette Wheel_, dzięki czemu omijamy lokalne minima i stopniowo widać jak uzyskiwane wyniki się polepszają
* każda mrówka po zakończeniu drogi wysyła stosowną informację do procesu _mastera_, który to odpowiednio uwzględni tę informację u siebie; mrówka natomiast potem aktualizuje wartości feromonu w każdym węźle zgodnie z obliczoną wartością (**należy tutaj zaznaczyć, iż mrówka wysyła informację do obu węzłów tworzących krawędź, dzięki czemu informacje w dwóch procesach nie rozchodzą się i są identyczne**), po czym rozpoczyna swoją podróż na nowo, ponownie losując początkowy węzeł.
* proces _master_ przechowuje aktualnie najlepszy wynik oraz ilość już zaraportowanych wyników; gdy wartość ta przekroczy liczbę zdefiniowanych w systemie mrówek, zakładamy, iż minęła jedna iteracja i do procesu _technical ant_ wysyłana jest informacja o potrzebie ewaporacji wartości feromonu
* zgodnie ze współczynnikiem ewaporacji odparowywana jest porcja feromonu na wszystkich węzłach
* po przekroczeniu założonej z góry ilości iteracji, program jest przerywany, procesy wszystkie poza głównym są zabijane, a najlepszy wynik wraz z czasem obliczeń jest wyświetlany

## Slurm
**Zeus Erlang 21.3**
```
module load plgrid/apps/erlang/21.3
```

## Notatki z konsultacji

__Dywagacje i rozważania nt. ewaporacji__ 
* ROZWAŻANE: każda mrówka po przejściu ścieżki do krawędzi wysyła info o zwiększeniu feromonu, a do pozostalych wysyła: evaporuj -> wtedy synchronizacja między chodzeniem mrówek a parowaniu, n więcej komunikatów w systemie -> kapkę ciężko  
* ROZWAŻANE: mrówka specjalistka -> jak doszłą do trasy, (jednej pojdzie lepiej, drugiej gorzej), ona zapinguje proces odparowywacz, który ...  
* NIE WOLNO: każdy z procesów krawędzi sam będzie siebie odparowywał -> najgorsze problemy synchronizacji w erlangu to pytanie zegara o coś  
* ROZWAŻANIE: ileś mrówek przeszło przez krawędź, krawędź sobie zlicza liczbę odwiedzin, i wtedy sobie odlicza feromony -> może być pechowa krawędź, przez którą żadna mrówka nie przejdzie :(
* SOLUTION vol. 1 -> jeden zegarowany proces i zobaczymy co się będzie działo, ale to jest wtedy zależne od prędkości wykonania  
* SOLUTION vol. 2 -> w jakiś sposób to uzależnić od przebiegu algorytmu  
* Warunkiem stopu będzie jakiś czas określony, bądź w iteracjach mrówki (pojedynczej?)

---


WĘZEŁ JAKO PROCES:
* węzeł: N-1 krawędzi, info dokąd dojść, komplet informacji o feromonach
* jak mrówki dochodza do węzła z innych stron to mogą się informacje rozejść (trza to rozwiązać, jakoś średnią wyciągnąć czy coś -> chodzi o info o tablicy feromonu zawarte na węzłach połaczonych krawędzią, żeby te dane były TE SAME a nie rozbieżne)
* kierunkowość nam jest niepotrzebna

* mrówka może mieć w pamięci lub dostać mapę aktualnych wartości feromonów na krawędzi 
* wartość feromonu zależy od długości, dlatego dopiero po przebyciu całej ścieżki można odpowiednio updatować wartości feromonu
* odparowywanie: jeden dedykowany proces, który przez wszystkie węzły przechodzi i odparowuje, może niech to będzie mrówka która przechodzi przez wszystkie węzły. Niech mrówka chodzi sobie sekwencyjnie i odparowuje, i tu nie będzie problemu z kierunkowością. Może niech ona też godzi te rozjazdy miedzy kierunkami, żeby uśredniać? Dobry pomysł, parować i zmniejszać te rozjazdy. Fajnie.

https://books.google.pl/books?id=pg9f4rmV-akC&lpg=PA413&ots=il3o_t9JHf&dq=tsplib%20%22oliver30%22%20best&pg=PA413#v=onepage&q=tsplib%20%22oliver30%22%20best&f=false

