# aco-erlang-implementation
Implementation of Ant Colony Optimization (ACO) in Erlang language. Solving travelling salesman problem (TSP).  
Project realized for Computational Intelligence class, AGH University of Science and Techonology

---
## About
Program finds near-optimal solution for travelling salesman problem (TSP) using Ant Colony Optimization algorithm. 

## Usage
To compile and run:
```sh
./run <path_to_tsplib> <ants> <iterations> <method>
```

If you wish to run directly with _Erlang_: 
```sh
erl -pz ebin/<method> -s main start problems/oliver30.tsp 30 100
```
or in <span style="color:purple">_Eshell_</span> -  go to <span style="color:teal">_src/\<method\>_</span> and execute:
```erlang
Eshell V10.6  (abort with ^G)
1> c(main), c(node), c(reader), c(ant), c(master).                % to compile
2> main:start().                % note to change initial parameters in start() method accordingly!
```
Where _method_ is a chosen method from folder _src_.  


---
## Methods
Below described are 3 methods available in this repository.
#### parallel
This is the best and default method in this project. It mitigates the bottleneck problem of the parallel_with_master method by completely getting rid of _master_ process. Here each node is responsible for evaporation of its pheromone table, which happenes each time a complete number of ants in the system passes through the node. The _technical ant_ process is here responsible for traversing the best path by the pheromone indication level, which happenes without any selecting method like roulette wheel selection, like with the rest of ants. Here _technical ant_ only looks at the best possible path based on pheromone level in pheromone table of the node. It is also responsible for killing nodes, itself and main program on the end of the execution. 


#### parallel_with_master
Structurally program consists of 4 types of processes (besides the main process). These are _node_, which quantity is equal to the number of cities defined in a [TSPLIB data file](http://elib.zib.de/pub/mp-testdata/tsp/tsplib/tsp/index.html), _ant_, which quantity is defined by the user, _technical ant_, which is an ant created solely to give evaporation and die orders to all nodes upon certain conditions met, and lastly _master_ which is a checking process for finding the best solution and other such utilities.  
While this method is conceptually simple, it proved to have a bottleneck in the _master_ and _technical ant_ processes, which are only singular. Thus, for large datasets this solution is not optimal.


```erlang
node(NodeNo, DistanceTo, Pheromones) 
```
* each node has knows about its own number and distance maps to each other node, along with the pheromone value map to the other nodes 
```erlang
ant(Master, NodesPids, Distance, Path) 
```
* the ant has the pid of the master process, the pid map of each node and its own distance traveled with a list of nodes traveled
* while on the way, the ant sends a query to the current node's process where to go next, to which he receives distance maps and pheromone values ​​in response; on their basis, the ant calculates the probabilities of each edge originating from this node
* the next node on the ant's path is selected (randomly) using the _Roulette Wheel_ method, thanks to which we avoid the local minima and gradually see how the results are improving
* each ant, after completing the journey, sends the appropriate information to the _mastera_ process, which will take this information into account accordingly the ant then updates the pheromone values ​​in each node according to the calculated value (** it should be noted here that the ant sends information to both nodes forming the edge, so that the information in the two processes does not diverge and are identical **), and then starts its the journey again, re-drawing the starting node.
* the _master_ process stores the currently best result and the number of results already reported; when this value exceeds the number of ants defined in the system, we assume that one iteration has passed and information about the need to evaporate the pheromone value is sent to the _technical ant_ process
* according to the evaporation rate, the pheromone portion is evaporated on all nodes
* after exceeding the predetermined number of iterations, the program is terminated, all processes except the main one are killed, and the best result is displayed together with the calculation time

#### sequential
This is the most basic implementation of ACO algorithm, which does not leverage any kind of data and computing concurrency.

## Slurm
Load **Erlang 21.3** using the following command:
```
module load plgrid/apps/erlang/21.3
```
To schedule sbatch job, edit the sbatch-job.sh file appropriately and run it with `sbatch sbatch-job.sh`.

## Troubleshooting

If you are getting this kind of error: 
```sh
{"init terminating in do_boot",{badarith,[{node,initDistanceTo,5,[{file,"src/parallel/node.erl"},{line,28}]},{node,initNodes,5,[{file,"src/parallel/node.erl"},{line,40}]},{main,start,1,[{file,"src/parallel/main.erl"},{line,17}]},{init,start_em,1,[]},{init,do_boot,3,[]}]}}
init terminating in do_boot ({badarith,[{node,initDistanceTo,5,[{_},{_}]},{node,initNodes,5,[{_},{_}]},{main,start,1,[{_},{_}]},{init,start_em,1,[]},{init,do_boot,3,[]}]})
```
it probably means that the tsplib data file is not being parsed correctly. Check if nodes location is in integers of in scientific notation. If the latter, please convert it using attached converter.
