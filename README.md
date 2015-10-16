# G4ip

Implementation of a theorem prover for propositional logic using G4ip in Haskell.


## File Structure

* G4ip/Proposition.hs  -- Definition of propositions and some syntactic sugar
* G4ip/Decider.hs      -- The actual theorem prover (decider?)
* G4ip/Tester.hs       -- For defining and running tests
* G4ip/TestMain.hs     -- Actually runs the default test suite


## Testing Manually

* Startup `ghci`
* Load `Main.hs` by typing `:l Main`
* Use `decide prop` to see if `prop` is provable.

You can use `T`, `F`, `/\`, `\/`, `==>`, `<==`, `<=>`, `neg`, and `()` with their usual meanings to form propositions. To form an atom, either use `Atom "name"` or use one of the predefined atoms: `a`, `b`, `c`, `d`, `e`, or `f`. Here is an example:

```
decide $ (neg b ==> neg a) ==> (a ==> b) \/ (a \/ a ==> a)
```

which prints `True` as expected (`$` if for associativity, you can use parenthesis if you want).


## Contact

Email me at cacay@cmu.edu for any questions.
