# A decision procedure for Synchronous Kleene Algebras
In a brief way, this procedure can be explained in the following way:
- given two regular expressions, we use the partial derivatives method to build two NFAs (one for each regular expression)
that accept the languages generated by said regular expressions.
- Afterwards we use the bisimulation up-to congruence to compare both NFAs
