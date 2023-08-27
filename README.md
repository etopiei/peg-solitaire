# Peg Solitaire Solver

See the results of the solver on [YouTube](https://youtu.be/xN28KUunDXk).

Code to solve peg solitaire and make an animation of playing the game.

To solve:

```
ocamlc solve.ml
./a.out > yes.txt
```

To make animation:

```
mv yes.txt animation/
cd animation
python -m venv .venv
.venv/bin/activate
python -m pip install -r requirements.txt
python main.py
```