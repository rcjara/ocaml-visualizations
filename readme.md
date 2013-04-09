# Ocaml Data Structure Visualizations

An attempt to visualize recursive data structures in ocaml.  The idea is that for any recursive data structure, if you simply define a children function and a label function, you will be able to create a drawing module for that datastructure.  The data structure will then be rendered as circular nodes, with the children recursively coming out of the parent nodes.

Currently binomial heaps and red black trees are the only datastructures implemented.  More to be added.
