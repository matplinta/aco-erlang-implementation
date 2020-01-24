#!/bin/bash -l
##Nazwa zlecenia
#SBATCH -J Basic
## Liczba alokowanych węzłów
#SBATCH -N 1
## Liczba zadań per węzeł (domyślnie jest to liczba alokowanych rdzeni na węźle)
#SBATCH --ntasks-per-node=24
## Ilość pamięci przypadającej na jeden rdzeń obliczeniowy (domyślnie 4GB na rdzeń)
#SBATCH --mem-per-cpu=4GB
## Maksymalny czas trwania zlecenia (format HH:MM:SS)
#SBATCH --time=23:0:0
## Specyfikacja partycji
#SBATCH -p plgrid
#SBATCH -A metah1
## Plik ze standardowym wyjściem
##SBATCH --output="logs/basic/%j.csv"
## Plik ze standardowym wyjściem błędów
##SBATCH --error="logs/basic/%j.err"
  
module load plgrid/apps/erlang/21.3
 
## go to execution dir
cd $SLURM_SUBMIT_DIR
 
echo -e "============================\n===att532 problem"
 

 
for ants in 30 60 90 
do
    for ITER in 100 150 200
    do
	echo -e "################   Ants: $ants, Iterations: $ITER \n"
	./run problems/att532.tsp $ants $ITER parallel
    done
done




echo -e "#################  pla85900.tsp: ants 30, iteration 50:"
./run problems/pla85900.tsp 30 50 parallel
