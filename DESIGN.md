## Parsing
Parsing have been done with Parsec and provides simple checks on the file:
 * room size is not null
 * all detectors have valid coordinates
 * the specified number of detectors are filled, not more, not less

## Solving strategies (Assistannt.findLowestDetectionProbability)
It is a two step strategies:
 * Split the room in 100x100 squares, for each one, find the closest to its center, since they will have the biggest impact on the minimum probability
 * It will gives a lightest path regarding of the closest detector, finally, it remains to find the farther point of all closest detectors to find the lowest detection probability

## NoteBene
I realized after 5 hours that it could be solve with a Dijkstra, after having difficulties to find a good library, I have found a file which could do the trick, sadly, it would have take me more than the remaining time to refactor it.
The challenge is basically tested and the missing pieces are the following:
 * Convert the squares grid into a graph (with the edges' weight equals to the square's closeness)
 * use the Dijkstra's module to get the farther path
 * Compute the detection probability of each square of this path
 * Get the maximum which would be the lowest detection probability
