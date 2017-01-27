# Scala-LMS

Lightweight Modular Staging (LMS) is a Scala library for multi-stage generative programming. It allows for writing high-level, generic programs that generate specialized and fast low-level code.

One particularly interesting use case is staging functions for which some arguments are fixed; this allows for an optimized function to be generated. In this project, we investigate the performance of several such optimized staged functions.

## How to build

1. Download and install the [Java Development Kit (JDK)](http://www.oracle.com/technetwork/java/javase/downloads/index.html).

2. Install the [sbt](http://www.scala-sbt.org/) build tool.

3. Run the `sbt run` command to run the project.

## Project layout

- `main.scala` is the file which is being run when you run this project. 
  Within this file, you can see the method `main()`, which contains serval `.run()`-calls. 
  Each call triggers an experiment of one of the three implemented algorithms. 
- Each algorithm is implemented in a different file:
    1. Binary Search:       `BinarySearch.scala`
    2. Binary Tree Lookup:  `treesearch.scala`
    3. Rabin-Karp:          `RabinKarp.scala`

For more information on the implementation of an algorithm, see its corresponding file.
