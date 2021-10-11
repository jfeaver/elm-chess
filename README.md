# Elm Chess
A basic implementation of Chess written in the Elm programming language. This does not
implement a lot of quality of life features like a menu, checking for check and check
mate (or preventing you from moving into check). There's no import/export of game data
or tracking move history. There's no AI or cloud multiplayer.

## To Play Chess
I like to use Elm Live but I imagine you could get this working with `elm reactor`. With
Elm Live installed and with your terminal open to the project's root directory:

```
elm-live src/Main.elm
```