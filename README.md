squaddie
========

A little AI Game.

The idea behind squaddie is that player's write code that responds to a goal. In the actual game, when a squad member recieves this goal it will use the player written code.
The ultimate goal is to create a networked game in which players compete against each other by writing their own squad intelligence.


Current Iteration: 1
========
Two pieces of code are required to play Squaddie: a config and a squaddie.
The config file creates the world and the goal. Below is an example that provides a procedure to run the game:
```racket
(provide run-game)
(define (run-game %) (((create-game WORLD-SCENE) 50+50i (location 200+345i)) %))
```
This config creates a game with a starting point at the (50, 50) with a location based goal of (200, 345).
The function, run-game, is provided for the player to use.
The WORLD-SCENE is defined elsewhere as a background image and the location structure comes from the goals and directives module.

A player can require this module in order to run an AI against this configuration:
```racket
(require "config.rkt" "goals.rkt" "squad.rkt")

(define-squaddie my-squaddie% 
  ((location-goal goal) (move-toward goal)))

(run-game my-squaddie%)
```
This file creates a squaddie that will move itself toward the goal.
After the AI is defined, the run-game function that was defined in config.rkt is called to run the simulation.