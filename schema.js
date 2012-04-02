// ===========================
// = Messages sent to server =
// ===========================

// Connect
{
  "messageType": "connect",
  "data": {
    gameID: number
  }
}

// Join
{
  "messageType": "join",
  "data": {
    "nickname": string,
    "playerType": "player" | "spectator"
  }
}

// Ready

{
  "messageType": "set_ready",
  data: {
    "ready": boolean
  }
}

// Card Move
{
  "messageType": "play_card",
  "data": {
    "target" : <target>,
  }
}

// Get game state
{
  "messageType": "get_game_state",
  "gameID": number
}

// Rematch

{
  "messageType": "rematch",
  "gameID": number
}


// =================================
// = Messages returned from server =
// =================================

// Error
{
  "messageType": "error",
  "data": {
    "errorMessage": string,
    "errorType": <short error message>
  }
}

// Error Types: join_failed | game_not_found

// Connected
{
  "messageType": "connected",
  "data": {
    "started": boolean,
    "players": [string]
  }
}

// Roster Update
{
  "messageType": "roster_update",
  "data": {
   "players": [string]
  }
}

// Started
{
  "messageType": "started",
  "data": {
   "gameState": gamestate
  }
}

// Game State
{
  "messageType": "game_state"
  "data": {
    "gameState": gamestate
  }
}

// Card Tracking


// Game over
{
  "messageType": "game_over"
  "data": {
    "winner": <string>,
  }
}

// ===================
// = Data Structures =
// ===================

target = {
  kind: "player" | "stack",
  id: number
}

gameState: {
  dealtCard: {
    card: <card>,
    target: <target>,
    eta: number
  }
  players: [
    {
      rider: {
        card: "hit" | "split" | "skip"
        extras: number
      } | null,
      stacks: [
        [<card>]
      ]
    }
  ]
}

card: "hit" | "split" | "skip" | "stop" | "push" | "pull" | "ground"