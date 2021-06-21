# Ocean of Code

## The Goal

This game is based on the board game Captain Sonar.

You pilot a submarine and you know that an enemy is present near you because you are listening to its radio frequency communication. You don't know exactly where it is but you can hear all its orders.

You and your opponent have 6 hit points. When a player's hit points reach 0, the player loses.

## Rules

### Definitions

- Submarines move on a map constituted of water and islands. They can only move on cells with water. They can share the same cell without colliding.
- The map is 15 cells in width and 15 cells in height. Coordinates start at (0,0) which is the top left cell of the map.
- The map is split in 9 sectors, which contain 25 cells each (5x5 blocks of cells). The top left sector is 1. The bottom right sector is 9.

### Beginning of the game

At the beginning of the game, you'll receive a map (15x15 cells) that indicates the position of islands. Islands are obstacles. You cannot move or fire through islands. Then, you will decide where you want to place your submarine by indicating a coordinate (x,y).

### Each turn

This is a turn based game which means that each player plays a turn one after the other. The player with the id 0 begins. During your turn, thankfully to your radio frequency analysis, you will receive an indication of what your opponent has done. For example, you can receive that it moved to the north. It's up to you to use this valuable information to detect where it is. Then, you must perform at least one action.

### Actions

Each turn you must perform at least one action. You can do several actions by chaining them using the pipe |. But you can use each type of action only once per turn (you can move one time per turn, no more). If you fail to output a valid action, you will SURFACE in that turn.

#### Move

A move action moves your submarine 1 cell in a given direction (north, east, south, west) and charges a power of your choice. When you move, you must respect the following rules:

- You cannot move through islands
- You cannot move on a cell you already visited before

You can decide, what to charge. Different devices require a different amount of charges to be ready. In this league you can charge the torpedo, the sonar and the silence mode.

#### Surface

By using surface you will reset your path of visited cells so that you can freely move to a cell that you have previously visited. But surfacing has a major impact: your opponent will know in which sector you are surfacing and you lose 1 hit point.

#### Torpedo

A torpedo requires 3 charge actions to be ready. When fully charged, the torpedo can be fired at an arbitrary water position within a range of 4 cells. This allows the torpedo's path to contain corners and go around islands, but not through them. The damage of the explosion is 2 on the cell itself and 1 on all neighbors (including diagonal ones). You can also damage yourself with a torpedo. The following image illustrates the range of a torpedo:

![torpedo-range](https://raw.githubusercontent.com/CodinGameCommunity/ocean-of-code/master/torpedoRange.png)

The following table shows, how different actions will be shown to your opponent:

| Your action      | Shown to opponent | comment                                                                                                                        |
| ---------------- | ----------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| `MOVE N TORPEDO` | `MOVE N`          | The opponent will see the movement direction (in this case: north), but not which power is charged                             |
| `SURFACE`        | `SURFACE 3`       | The opponent will see, that you surfaced in sector 3                                                                           |
| `TORPEDO 3 5`    | `TORPEDO 3 5`     | The opponent will see, that you fired a torpedo at the cell (3,5)                                                              |

### Victory Conditions

Have more lifes than your enemy at the end of the game. Each player has 300 turns including the initial placement.

### Lose Conditions

- Output an invalid command or don't respond in time.
- Reach 0 lives.
- Intersect your own path, move into an island or out of the map.

### Expert Rules

The source code can be found here: <https://github.com/CodinGameCommunity/ocean-of-code>.

---

## Game Input

### Inputs of the first turn

Line 1: a string with 3 space separated integers.

`width` `height` `myId`

- `width` and `height` that indicates the size of the grid in term of cells (15 15)
- `myId` that indicates your player ID (0 or 1). The player with ID 0 begins.

Next `height` lines: a string of `width` chars representing the content of each cells of this row. `x` for an island, `.` for an empty cell.

### Output for the first turn

2 space separated integers `x` `y` which represent the coordinates of your starting position. The cell at the specified coordinates should be empty which means no island, but the opponent can choose the same cell!

### Inputs for each turn

Line 1: a string with 8 space separated integers.

`x` `y` `myLife` `oppLife` `torpedoCooldown` `sonarCooldown` `silenceCooldown` `mineCooldown`

- `x` and `y` represent your current position.
- `myLife` and `oppLife` give the number of hit points remaining for, respectively, you and your opponent
- `torpedoCooldown` `sonarCooldown` `silenceCooldown` `mineCooldown` represent the cooldowns for each device. Devices unavailable in your league will have -1.

Line 2: a string `sonarResult` which gives you the result of the `SONAR` action: Y for yes, N for no. It is NA, if no sonar was used.

Line 3: `opponentOrders`, a summary of the actions (separated by |) that your opponent has made during its turn.

Example: `MOVE N |TORPEDO 3 5`

This example indicates that your opponent moved to the north and then fired a torpedo at the cell (3, 5).

NA is used the first turn of the starting player (this is the only case when your opponent hasn't done anything yet).

## Output for each next turns

One or multiple commands separated by |.

e.g.: `MOVE N TORPEDO | TORPEDO 3 5` These commands move your submarine to the north and then fire a torpedo at the cell 3,5. Here are the different available actions:

- `MOVE direction POWER`
- `SURFACE`
- `TORPEDO X Y`
- `MSG message`

### Constraints

Response time first turn ≤ 1000 ms

Response time per turn ≤ 50 ms
