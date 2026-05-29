# Forest 

Trees, and data structures based on trees

## Usage

Run `stack test`

## What's Included

These kinds of trees (and their derivative data structures) are in this repo:

- Binary search tree
- Heap-ordered tree
  - Leftist min heap
- Binomial tree
  - Binomial heap
- Red-black tree
  - Map
  - Set
- Splay tree
  - Splay heap
- Heap-ordered multitree
  - Pairing heap
- Rose tree (with Traversable instance)
- 2-3 B-tree
- Ternary tree
- Exponential tree (with dimensionality constraints enforced by types)
- Finger tree (with Monad instance)

## Anything else?

This work is inspired by Okasaki's _Purely Functional Data Structures_. I've cleaned up some of the implementations here based on that reference, but I tried to come up with my own implementations first before consulting it. Any implementation not found in that book is my own.

Finger tree implementation is based on [Hinze, Paterson (2006)](https://www.cs.ox.ac.uk/ralf.hinze/publications/FingerTrees.pdf) with help from [this talk](https://www.youtube.com/watch?v=ip92VMpf_-A)
