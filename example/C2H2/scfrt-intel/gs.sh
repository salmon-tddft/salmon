export OMP_NUM_THREADS=24
date > output_gs
mpiexec -np 1 ../../../salmon.cpu < input_gs >> output_gs
date >> output_gs
