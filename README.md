# CSE230-project

## Summary
Our team is going to build a game called **Blendoku** with Haskell. 

**Blendoku is a puzzle game that involves arranging colors in a specific order to create a gradient or blend.** The game typically presents players with a grid of colors, and the objective is to arrange them in a way that smoothly transitions from one color to another. The difficulty increases as you progress through the levels, requiring more intricate arrangements and precise color blending.

The game often includes challenges related to color perception, pattern recognition, and logical thinking. Blendoku has been available on various platforms, including mobile devices, and it has gained popularity for its unique and engaging approach to color-based puzzles. Players find it both entertaining and a good exercise for improving their understanding of color relationships.

## Game rules
In Blendoku, players are given a grid of squares, some with colors provided, some not. A palette of additional color tiles is provided. To clear a level, a player must "fill in" the empty squares from the palette in such a way as to create smooth sequences of colors both horizontally and vertically, i.e. the depth of color increases monotonously from left to right and from top to bottom. 

## Related work

We found Blendoku on the App Store as a game based on iOS released by Lonely Few LLC in 2015. A lot of people liked playing it. Even though the company doesn't update it anymore, many fans made their own versions of Blendoku. 


Searching for the keyword "Blendoku" on GitHub reveals implementations in JavaScript, PHP, TypeScript, C++, Java, Ruby, and Swift. **As of our knowledge cutoff on the data of our proposal (11/17/2023), there is no implementation in Haskell yet.**

Below is a `GIF` demonstrating how to play Blendoku, along with some useful links for a better understanding of the game. The GIF was created by running [the website](https://karlbao.github.io/Blendoku/), a demo project that we adopt as a non-Haskell reference for completing ours.


![How to play blendoku](docs/demo-blendoku.gif)
- [A demo to blendoku](http://www.blendoku.com/)
- [Blendoku2 on AppStore](https://apptopia.com/ios/app/1017177662/about)
- [Github search result for "Blendoku"](https://github.com/search?q=Blendoku&type=repositories)


## To-do list
Here are some of the tasks that we need to complete are listed as follows: 

- [ ] Static UI interface: Displays color tiles (given tiles, empty tiles to fill, candidate region)
- [ ] Event handle: Allows users to interact with the program by either mouse or keyboard 
- [ ] Puzzle generation: Generates random graph of grids and candidate tiles
- [ ] Ground truth generation: Generates ground truth for puzzle
- [ ] (Optional) Hint: Allows users to seek hint
- [ ] (Optional) Metrics generation: Computes the time spent on each game and output score for users.

