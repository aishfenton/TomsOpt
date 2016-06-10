# TomsOpt

Playing around with Gaussian Processes for Bayesian Optimization. This project was mostly to test to runtime performance
of different approaches, and isn't a complete implementation (although with some effort it could be turned into one).

## Setup

Pre-reqs are:

* Sbt
* Scala
* Java
* BLAS

Note: On a Mac Sbt and Scala can be brew installed, and Blas comes built in.

## Running

To run a very simple example app (see code here /core/src/scala/tomsopt/math/example/) that fits a simple function
and then predicts new points and plots them.

```
sbt core/run
```

To run the unit tests:

```
sbt test
```

To run the benchmark suite (which tests 10,000,000 points and the calculates the point with highest
expected improvement using both JNI and pure Java versions).

```
sbt bench
```

Last run of bench on my MacBook Pro (Retina, 15-inch, Mid 2015) had produced the following results (in seconds):

```
[info] Benchmark               Mode  Cnt  Score   Error  Units
[info] EndToEnd.predictJava    avgt    4  9.254 ± 2.149   s/op
[info] EndToEnd.predictNative  avgt    4  6.306 ± 3.577   s/op
```

## Todo

Bits still missing:

* Kernel parameters are currently hardcoded in C versions (obviously these need to be passed in).

* Kernel parameters aren't fit, would need to explore Breeze's optimization stuff (from quick look it seems straight forward)

* There's no definition of what the feature space is. Features are assumed to be -inf to +inf range when sampling.

* Doesn't support producing a batch of points to explore, would need to introduce Monte Carlo estimate of best

* Haven't really checked anything for correctness, probably lots of bugs ;)


