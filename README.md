# HaskellPong
Small pong game written in Haskell.
<br/>
<img src="http://i.imgur.com/EaCtIOM.png" width="50%"/>

### Building
This should be straight forward with cabal:
<br/>
Once in the directory
```
cabal configure
cabal build
```

### Running the game
```
./dist/build/HaskellPong/HaskellPong
```
The game can be played with the following options (command line parameter):
- 0 - Computer against computer
- 1 - 1 player (bottom player) against computer
- 2 - 2 players (default)

### How to play
<p>To move the top player the keys are:
  <ul>
    <li>A - Move left</li>
    <li>D - Move right</li>
    <li>S - Release the ball</li>
  </ul>
</p>
<p>To move the bottom player the keys are:
  <ul>
    <li>Left arrow - Move left</li>
    <li>Right arrow - Move right</li>
    <li>Up arrow - Release the ball</li>
  </ul>
</p>
<p>Score:
  <ul>
    <li>R - reset the score</li>
  </ul>
</p>
