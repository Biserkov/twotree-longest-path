# O(n) algorithm for computing longest paths in 2-trees

In 2013 Markov, Vassivel and Manev published a novel linear time [algorithm](https://sites.google.com/site/minkommarkov/longest-2-tree--draft.pdf?attredirects=0&d=1) for computing longest paths in 2-trees.

This repository contains 3 implementations of said algorithm in Clojure:

The [first](https://github.com/Biserkov/twotree-longest-path/blob/master/test/longest_path/direct.clj) one is a direct implementation of the mathematical operations described in the paper. Due to the need to construct subgraphs to implement splitting, the time complexity is ```O(n√n)```.

The [second](https://github.com/Biserkov/twotree-longest-path/blob/master/test/longest_path/preprocessed.clj) one was suggested by Minko Markov and preprocesses the 2-tree, thus avoiding the need to construct subgraphs. The time complexity is ```O(n)```. Due to its recursive nature, this implementation will fail with a Stack Overflow Error if used on deep 2-trees with millions of vertices.

The [third](https://github.com/Biserkov/twotree-longest-path/blob/master/src/longest_path/iterative.clj) implementation is an iterative one. It uses the EdgeLabels map as a sort of explicit call stack. The time complexity is ```O(n)```.

Only the third one should be used for any practical purposes.

I used property-based (a.k.a. generative) testing during development and found it provided great value.
