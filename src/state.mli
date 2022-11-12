(** Representation of states in a game.

    This module represents the states the game can take on*)

(*This is the state when the application first opens. It is a paused state,
  waiting for the player to start the game. It displays a simple message on the
  screen. The application remains in this state so long as the player never
  presses a key. In addition, this is the state the application returns to when
  the game is over.*)
val state_inactive : bool ref

(*This is the state when the game is running and the ghost is chasing the
  camel.*)
val state_active : bool ref

(*This is the state when the game is being temporarily paused and waiting for
  the user's input to continue playing the game.*)
val state_pause : bool ref

(*This state restores the current game details. The application switches to this
  state if state_paused is true in the previous frame, and the player pressed a
  key. When state_continue is true, state_active should be changed to true in
  the next animation frame.*)
val state_continue : bool ref

(*State_reverse is true when the ghost is trying to escape from the maze.*)
val state_escape : bool ref

(*State_complete is true when the game is over.*)
val state_complete : bool ref
