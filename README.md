# Haskell-Data-Structure
This repo serves as a practice of writing both functional and mutable strict data strucutures in Haskell.  

My aim is to make a wide range of data structures that ideally would satisfy not performance-sensitive purposes. However, since it is a practice code instead of a library, there is no guarantee of correctness and stability. Moreover, efficiency was not an design objective. Therefore, any performance-sensitive task should not be relied on this.  

Now working on: List concatenation; Unordered List.  
Next to come: Map; Red-Black Tree.  

# [MMZKDS/DS.hs](MMZKDS/DS.hs)
This file provides type classes for immutable data structures.  
It contains class `DS` providing methods for clearing data and size checking, and class `DSCons` providing ways of instantiating and transforming the data structure to other immutable types (*e.g.* from or to list).  
See the [Haddocks](MMZKDS/DS.hs) for full documentation (TODO).  

# [MMZKDS/MDS.hs](MMZKDS/MDS.hs)
MDS is short for Mutable Data Structure. This file provides type classes for mutable structures.  
It contains class `MDS` providing ways of copying and clearing, and class `MDSCons` providing ways of instantiating and transforming the data structure to other immutable types (*e.g.* from or to list).  
See the [Haddocks](MMZKDS/MDS.hs) for full documentation.  

# [MMZKDS/ArrayBased.hs](MMZKDS/ArrayBased.hs)
This file provides type classes for immutable and mutable data structures in which their implementations are based on arrays.  
The classes `ArrayBased` and `MArrayBased` provide methods for re-allocating arrays in the need of larger or smaller length.  
See the [Haddocks](MMZKDS/ArrayBased.hs) for full documentation.  

# [MMZKDS/List.hs](MMZKDS/List.hs)
This file provides type classes for both immutable and mutable list structures, including methods of random access, addition, deletion, finding indices *etc.*   
There are two classes, namely `List` and `MList`, for immutable and mutable lists, respectively.  
The methods are based on the [Java List Interface](https://docs.oracle.com/javase/8/docs/api/java/util/List.html).  
Instances of `List` is automatically instances of `Eq` if the elements are instances of `Eq`, where two such structures are equal if and only if they have the same size and equal elements at each entry.  
See the [Haddocks](MMZKDS/List.hs) for full documentation.  

# [MMZKDS/Queue.hs](MMZKDS/Queue.hs)
This file provides type classes for both immutable and mutable queue and deque structures.  
There are two classes for queue, namely `Queue` and `MQueue`, for immutable and mutable queues, respectively.  
The other two classes are for deque, namely `Deque` and `MDeque`.  
The queue adds elements to the rear and removes elements from the front.  
See the [Haddocks](MMZKDS/Queue.hs) for full documentation.  

# [MMZKDS/PriorityQueue.hs](MMZKDS/PriorityQueue.hs)
This file provides type classes for both immutable and mutable priority queues.  
There are two classes, namely `PriorityQueue` and `MPriorityQueue`, for immutable and mutable priority queues, respectively.  
See the [Haddocks](MMZKDS/PriorityQueue.hs) for full documentation.  

# [MMZKDS/Set.hs](MMZKDS/Set.hs)
This file provides type classes for both immutable and mutable sets with unique elements, including methods of addition, deletion, union, intersection *etc.*  
There are two classes, namely `Set` and `MSet`, for immutable and mutable sets, respectively.  
It is expected that the elements are instances of `Eq`.  
See the [Haddocks](MMZKDS/Set.hs) for full documentation.  

# [MMZKDS/UnionFind.hs](MMZKDS/UnionFind.hs)
This file provides type classes for both immutable and mutable union-finds, offering find representative and union equivalence classes operations.    
There are two classes, namely `UnionFind` and `MUnionFind`, for immutable and mutable structures, respectively.  
See the [Haddocks](MMZKDS/UnionFind.hs) for full documentation.  

# [MMZKDS/ArrayList.hs](MMZKDS/ArrayList.hs)
`ArrayList` is a data structure implementing the `List` class with an internal array.  
Most operations that requires mutation on the `ArrayList` requires generating a new `ArrayList`, which is very costly (always O(n)). Therefore it is recommended to use the mutable version [`MArrayList`](MMZKDS/MArrayList.hs) for frequent state updates.
See the [Haddocks](MMZKDS/ArrayList.hs) for full documentation.  

# [MMZKDS/AVLSet.hs](MMZKDS/AVLSet.hs)  
`AVLSet` is a purely functional set structure with O(n * log n) operations. It is based on an internal AVL Tree.  
It is expected that the type of its elements is an instance of 'Ord'.  
See the [Haddocks](MMZKDS/AVLSet.hs) for full documentation.  

# [MMZKDS/FDQ.hs](MMZKDS/FDQ.hs)  
`FDQ` is a purely functional deque structure with amortised O(1) operations. It is based on two internal lists storing the front and rear of the deque.  
Although it also implements the `List` type class, it is the most efficient when used as a deque.  
See the [Haddocks](MMZKDS/FDQ.hs) for full documentation.  

# [MMZKDS/MArrayList.hs](MMZKDS/MArrayList.hs)
`MArrayList` is a mutable data structure implementing the `MList` class with an internal mutable array.  
See the [Haddocks](MMZKDS/MArrayList.hs) for full documentation.  

# [MMZKDS/MLinkedList.hs](MMZKDS/MLinkedList.hs)
`MLinkedList` is a doubly-linked circular list implementing the `MList` class.  
It remembers the last element (node) been visited, and enjoys fast access to the elements in its vicinity.  
See the [Haddocks](MMZKDS/MLinkedList.hs) for full documentation.  

# [MMZKDS/MHeapPQ.hs](MMZKDS/MHeapPQ.hs)
`MHeapPQ` is a mutable priority queue implementing the `MPriorityQueue` class through a min-heap, which is implemented with an internal `STArray`.  
It pops its minimum element, thus the type of its elements must be an instance of 'Ord'.  
See the [Haddocks](MMZKDS/MHeapPQ.hs) for full documentation.  

# [MMZKDS/UnBoxed](MMZKDS/Unboxed/)
Contains the unboxed version of (some of) the data structures.  

# [MMZKDS/UnBoxed/STURef.hs](MMZKDS/Unboxed/STURef.hs)
The strict unboxed equivalence for `STRef`.  
See the [Haddocks](MMZKDS/Unboxed/STURef.hs) for full documentation.  
