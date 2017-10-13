Calculator: The Game Solver (Haskell)
=============================
This is a tool to solve puzzles from [Calculator: The Game][game], ([Android][android] and [iOS][ios]).

The game consists of a calculator with only a few buttons for very specific operations, and a limited number of moves (button presses) to get to a specific goal number.

Instructions
-----------------------
**Build:**
```
ghc -o resevanje resevanje.hs
```
**Syntax:**
```
./resevanje [start] [goal] [max moves] [operation1 ... operationN]
```
**Operations:**
* '>n' means shift right in decimal by n places
* '-'  means change sign
* '+n' means add n
* '-n' means subtract n
* '*n' means multiply by n
* '/n' means divide by n
* 'reverse' reverse
* 'a=>b' replace a with b
* ':n' means add n to the end

**Example:**

*Level 57*
```
./resevanje 0 58 4 '+4' '*4' '-3' reverse
```
[game]: http://www.simplemachine.co/game/calculator-the-game/
[android]: https://play.google.com/store/apps/details?id=com.sm.calculateme
[ios]: https://itunes.apple.com/us/app/calculator-the-game/id1243055750
