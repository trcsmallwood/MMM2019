#!/bin/bash

echo -e "Create plot gifs\n"

sleep 1

echo -e "Cumulative match points gif...\n"
convert -delay 60 -loop 0 -quality 100 -fuzz 5% -layers OptimizePlus ../Plots/MMM2019_CumulativeMatchPoints_gifplots/*.jpeg ../Plots/MMM2019_CumulativeMatchPoints.gif
echo -e "Complete!\n"

sleep 1

echo -e "Cumulative round points gif...\n"
convert -delay 120 -loop 0 -quality 100 -fuzz 5% -layers OptimizePlus ../Plots/MMM2019_CumulativeRoundPoints_gifplots/*.jpeg ../Plots/MMM2019_CumulativeRoundPoints.gif
echo -e "Complete!\n"

sleep 1

echo "Finished"

exit
