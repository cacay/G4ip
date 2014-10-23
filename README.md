# G4ip

Implementation of a theorem prover for propositional logic using G4ip in Haskell.


## File Structure

* Main.hs         -- Where all the (IO) action happens
* Proposition.hs  -- Definition of propositions and some syntactic sugar
* Decider.hs      -- The actual theorem prover (decider?)
* Tester.hs       -- For defining and running tests


## Requirements

You will need the Haskell Platform and GHC (version 7.6.3 is what I have). This does not rely on any specific features of GHC, so you can modify the make file to make it work with another compiler if you need.


## Testing

Use `make` to build the testing infrastructure and `./g4ip` to run. This will run all the tests in `Tester.tests`, and print either `OK` or `WRONG` for each. You can add extra tests in `Tester.tests`.

To test manually:
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
