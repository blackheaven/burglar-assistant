## Solving strategies (Assistannt.findLowestDetectionProbability)
It is a two step strategies:
 * Split the room in 100x100 rectangles, for each one, find the minimum probability
 * It will gives a lightest path which, if taken the heaviest element, which is the lowest detection probability

## Essentials
Building the rectangles takes (for r rectangles, here 100², and d detectors):
 * Memory: O(r)
 * Time: O(r * d)

I have used a Dijkstra algorithm with a priority queue over rectangles.
The usual complexity is (for e edges and v vertices):
 * Time: O((v+e) * log v)
 * Memory: O(e * v)

Given than our vertices are our rectangles and each edges are their neighbours (usually 8, 5 for borders and 3 for border), and the complexity of the used:
 * Time: O(r * log(r)²)
 * Memory: O(r²)

Given that paths are generally short, the amortized complexity is:
 * Time: Θ(r * log(log(r))²)
 * Memory: Θ(r * log(r))
