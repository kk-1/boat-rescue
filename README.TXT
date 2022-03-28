 
This repo contains the data files and results for our paper:

K. I. Kilic and L. Mostarda, "Heuristic Drone Pathfinding Over Optimized Charging Station Grid," in IEEE Access, vol. 9, pp. 164070-164089, 2021, doi: https://doi.org/10.1109/ACCESS.2021.3134459

Bibtex:
--------------------------------------------------------------------------------
@ARTICLE{9645571,
  author={Kilic, Kemal Ihsan and Mostarda, Leonardo},
  journal={IEEE Access}, 
  title={Heuristic Drone Pathfinding Over Optimized Charging Station Grid}, 
  year={2021},
  volume={9},
  number={},
  pages={164070-164089},
  doi={10.1109/ACCESS.2021.3134459}}
-------------------------------------------------------------------------------- 


In this repo, there are 3 dirs:
--------------------------------
data: 
-----------
Contains lattice point data sets in TSPLIB format.
For each dataset eps files show the vertex configuration.

result: 
-----------
For each data sets contains results of the benchmarks in csv format.
The text files are results of the t-test.
The results are in the binary matrix format.
Row and column headers are the names of the algorithms.
1 represents statistically significant difference
0 represents no statistically significant difference

src:
-----------
The simulator written in R (run it from RStudio) and
info for processing the simulation results.
There is reame file in this dir.



boat-rescue-GUI.png: The screen shot of the simulator GUI developed.

-------------------------------------------------------------------------------- 
If you want to check out more files about the TSP algorithm please go to:
https://github.com/kk-1/tsp
-------------------------------------------------------------------------------- 

