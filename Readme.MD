# Toy Quad-Tree - 100% Julia

-----------------------
This package implements a quad-tree for geospatial/2D plane lookups. There are many like it, but this is mine.

## S.1. Environment + Setup

Pending

## Usage - Sample Inserts

Sample inserts into the tree. Tree size and query time highly dependent on points/box param (`QT_MAX_PTS`). More benchmarking examples at `./quadtree.ipynb`.

```Julia
using BenchmarkTools
using Quack; const QTrees = Quack;

const QT_TEST_SIZE, = 1_000_000

## Random Coords on (0,1)^2
ps = QTrees.genRandomCoords(0., 1., 0., 1., QT_TEST_SIZE);
sampleQTree = QTrees.qtBox(QTrees.Coord(.0, .0), 1.)

@time (for p in ps QTrees.insertIntoQuadTree!(sampleQTree, p) end)
println("Total Obj Size: $(Base.summarysize(sampleQTree) / 1000000 ) MB")

$ 2.340458 seconds (24.43 M allocations: 795.519 MiB, 27.85% gc time)
$ Total Obj Size: 44.367784 MB
```

```Julia
@benchmark QTrees.queryRange(sampleQTree, QTrees.Box(QTrees.Coord(rand() , rand()), rand()/2))

BenchmarkTools.Trial: 
  memory estimate:  10.16 KiB
  allocs estimate:  202
  --------------
  minimum time:     11.787 μs (0.00% GC)
  median time:      7.139 ms (0.00% GC)
  mean time:        13.481 ms (12.10% GC)
  maximum time:     76.144 ms (11.36% GC)
  --------------
  samples:          370
  evals/sample:     1
```

