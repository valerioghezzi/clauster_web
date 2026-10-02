# CLAuster in the browser

Cluster Analysis for Applied Research, running entirely in your web browser with WebAssembly (webR and Shinylive).

Open the app: https://valerioghezzi.github.io/clauster_web/

There is no server doing the computations. Every visitor's browser runs the analysis on that visitor's own computer, so any number of people can use the app at the same time, and data you open are never sent anywhere.

The first visit downloads R and the required packages (about 70 MB); later visits load from the browser cache and are faster. All methods of the desktop version are available except Gaussian mixtures, which need the mclust package and are not yet supported in the browser. Analyses run more slowly than in the desktop version, and an analysis in progress can be stopped only by its time limit.

Author: Valerio Ghezzi.
