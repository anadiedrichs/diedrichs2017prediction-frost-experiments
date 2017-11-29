#!/bin/bash
# extraigo cabecera
head -1 2017-11-24\ 14\:42\:32--experimento-dacc-local-normal-1-2.csv > dacc-123.csv
# reunir todos los resultados en un solo archivo
(tail -n +2 -q 2017-11-24*.csv >> dacc-123.csv) &
