# Haskell-Data-Structure
This repo serves as a practice of writing both functional and mutable data strucutures in Haskell.  

My aim is to make a wide range of data structures that ideally would satisfy not performance-sensitive purposes. However, since it is a practice code instead of a library, there is no guarantee of correctness and stability. Moreover, efficiency was not an design objective. For example, all usages of `ST` monad are lazy here. Therefore, any performance-sensitive task should not be relied on this.  

Now working on: A List type class and Immutable and Mutable ArrayLists; documentations.  
Next to come: Array-based Heap; Linked-List.  

# [MDT.hs](MDT.hs)
MDT is short for Mutable Data Structure. This file provides type classes for those mutable types based on the Lazy `ST` Monad.  
It contains `class MDT` providing a single method `copy`, and `class MDTCons` providing a single method `new`.  
See the [Haddocks](MDT.hs) for full documentation.  

# [List.hs](List.hs)
This file provides type classes for both immutable and mutable list structures, including random access, addition, deletion, find index *etc.*   
There are two classes, namely `List` and `MList`, for immutable and mutable lists, respectively.  
The methods are based on the [Java List Interface](https://docs.oracle.com/javase/8/docs/api/java/util/List.html).  
See the [Haddocks](List.hs) for full documentation.  

### DOCUMENTATION FOR OTHER FILES ON THE WAY...
