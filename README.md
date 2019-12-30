# aco-erlang-implementation
Implementation of Ant Colony Optimization (ACO) in Erlang language. Project realized for Computational Intelligence class.

# Notatki z konsultacji

- mrówka po przejściu całej ścieżki wysyła informacje o zwiększeniu feromonu na tej ścieżce
- ewaporacja:  
	ROZWAŻANE: każda mrówka po przejściu ścieżki do krawędzi wysyła info o zwiększeniu feromonu, a do pozostalych wysyła: evaporuj -> wtedy synchronizacja między chodzeniem mrówek a parowaniu, n więcej komunikatów w systemie -> kapkę ciężko  
	ROZWAŻANE: mrówka specjalistka -> jak doszłą do trasy, (jednej pojdzie lepiej, drugiej gorzej), ona zapinguje proces odparowywacz, który ...  
	NIE WOLNO: każdy z procesów krawędzi sam będzie siebie odparowywał -> najgorsze problemy synchronizacji w erlangu to pytanie zegara o coś  
	ROZWAŻANIE: ileś mrówek przeszło przez krawędź, krawędź sobie zlicza liczbę odwiedzin, i wtedy sobie odlicza feromony -> może być pechowa krawędź, przez którą żadna mrówka nie przejdzie :(
	
	SOLUTION vol. 1 -> jeden zegarowany proces i zobaczymy co się będzie działo, ale to jest wtedy zależne od prędkości wykonania  
	SOLUTION vol. 2 -> w jakiś sposób to uzależnić od przebiegu algorytmu  

Mrówka po aktualizacji feromonu w krawędziach wysyła raport o tym że skończyła, po tym zliczymy fitness, może całą trasę

* Można zaimplementować to z tym timerem, że to jest zjebane żeby pokazać
* Trzeba jakoś zaraportować mrówce fitness, żeby ktoś podjął decyzję, czy mrówka ma zaraportować info o trasie czy nie 
* Jest to algorytm który jest bardzo ściśle związany z technologią
* Warunkiem stopu będzie jakiś czas określony, bądź w iteracjach mrówki (pojedynczej?)

---

WSZYSTKO NA MAPACH TRZEBA IMPLEMENTOWAĆ, ROBIENIE NA LISTACH CZEGOŚ CO JEST WIĘKSZE NIŻ 10 SZTUK TO POMYŁKA

Na początku losujemy mrówce pierwszy węzeł.

Każdy węzeł ma dokładnie N-1 feromonów na krawędziach wychodzących z niego do innego węzła.  

WĘZEŁ JAKO PROCES:
* węzeł: N x N-1 krawędzi, info dokąd dojść, komplet informacji o feromonach
* jak mrówki dochodza do węzła z innych stron to mogą się informacje rozejść (trza to rozwiązać, jakoś średnią wyciągnąć czy coś -> chodzi o info o tablicy feromonu zawarte na węzłach połaczonych krawędzią, żeby te dane były TE SAME a nie rozbieżne)
* kierunkowość nam jest niepotrzebny

* mrówka może mieć w pamięci lub dostać mapę aktualnych wartości feromonów na krawędzi 
* wartość feromonu zależy od długości, dlatego dopiero po przebyciu całej ścieżki można odpowiednio updatować wartości feromonu
* odparowywanie: jeden dedykowany proces, który przez wszystkie węzły przechodzi i odparowuje, może niech to będzie mrówka która przechodzi przez wszystkie węzły. Niech mrówka chodzi sobie sekwencyjnie i odparowuje, i tu nie będzie problemu z kierunkowością. Może niech ona też godzi te rozjazdy miedzy kierunkami, żeby uśredniać? Dobry pomysł, parować i zmniejszać te rozjazdy. Fajnie.

https://books.google.pl/books?id=pg9f4rmV-akC&lpg=PA413&ots=il3o_t9JHf&dq=tsplib%20%22oliver30%22%20best&pg=PA413#v=onepage&q=tsplib%20%22oliver30%22%20best&f=false