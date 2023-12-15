## Brick Gomoku Game

**Group Member:**

Shaolong Li  
Anze Xie  
Shaochun Zheng  
Yanfan Chen  

### Overview:
This is the repo for the project of CSE 230. In this repo, we plan to implement a variant of the classic strategy board game Gomoku, also known as Five in a Row. The outcome of the project will be a line application based on the [brick](https://github.com/jtdaugherty/brick/) library.


### Setup:

#### Install GHC Environment
Recommend using Ubuntu 20 and [ghcup](https://www.haskell.org/ghcup/install/)
Tested on ghc 9.2.7, stack 2.11.1

#### Build
```
stack build
```

#### Run
```
stack exec boom
```

#### Test
```
stack test
```

#### Development Guide
```
Recommend to use VSCode with extension: Haskell and Simple GHC
```

### Description of Five in a Row:
The game is played on a grid board with a certain size. Players take turns to place their stones on the intersections of the grids. The goal of the game for each player is to create an unbroken row of fine stones horizontally, vertically, or diagonally. The first player to achieve this goal wins the game. 

### Objective of project:
A line application of our variant of Five in a Row based on the brick library. The application will be implemented with the following features:

### Features:
+ Undo Move: Players can retract their moves.
+ Online Support: Players can connect and compete with others over the internet or LAN.
+ "Throwing Knife": Each player is granted a single opportunity to remove any pieces within a specified 2x2 square area on the board, potentially disrupting the opponent's progress or clearing a path for their own victory.

### Update
Friday 12/15 Milestone 3: Demonstration

#### Game logics
The actions taken by players are recorded by a list of tuples. Each tuple represents the type (placing a stone or “booming”) and the coordinate of the action.
When a player undoes their step, the last element of the list is deleted.
The board is rendered by iterating the list, adding or removing stones according to the type and the coordinate of the actions. After the board has been rendered, every stone is checked whether there are four stones of the same color to its left, right, up, down, or in the diagonals.
When implementing the online mode, we will build our own server

#### UI Module
On the main page, each user can enter their username. A player can choose the grid size of the game as well as whether they want to play locally or online.
In a player’s turn, they can use arrow keys to move the cursor on the board, and press the enter key to place a stone on an empty intersection or press the delete key to remove all stones in the 2×2 matrix to the lower right of the cursor (called “boom”).

![image](https://github.com/lukechen2/cse-230-proj/assets/146805418/a90002c7-3d05-40b5-85ea-e5722b52ce29)



#### Challenges and Expectations:
The online version requires us to resolve some network issues. Functional language may not be a good fit for this. Therefore, we consider the network version of the project as an add-on. We do not expect to complete it before the deadline.


### Schedule of progress:
Friday 11/17 Milestone 1: Registration and Proposal  
Friday 12/1 Milestone 2: Updates  
Friday 12/15 Milestone 3: Demonstration  

