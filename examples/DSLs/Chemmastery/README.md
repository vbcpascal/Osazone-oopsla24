# Chemmastery

This language is an introductory sample from MPS. The language checks if a chemical equation is balanced.

We could have implemented this through laborious definitions of data types. Then, we would desugar chemical compounds into creations of the data types. Finally, an equation becomes a check of equality between the data types on two sides.

We here demonstrate a simple, tricky implementation, which requires a far less powerful host language. Each chemical element is assigned a prime number. Multiple occurences of an element is desugared as a power of the prime number. A compound is desugared the product of all its elements. When we multiply everything on each side of the equation, the equation is balanced if and only if the products on both sides are the same.
