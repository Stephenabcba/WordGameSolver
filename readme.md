# Word Game Solving Assistant
## About
- This is a text-based assistant to help you solve the recently popular word game.
- This version is written in JavaScript for easier webpage display
  - It is based on the python version, and may have some bugs still
- There are currently 2309 words that could be possible solutions as of 2/14/2022
  - The words are pulled from the word game website's javascript file
  - As such, the program is unable to process 
- With each "guess" you enter (along with the hit/miss response from the game), the program filters through the words list, providing you with a list of all possible words that could be the answer (target word)
- With two good initial guesses, there are at most 45 choices left in the word list after two guesses.
- Have fun!

## How to use it
- There are two choices in how you want to 
1. Download/Clone the project.
   - Open index.html in any browser that supports modern JavaScript
2. Open the GitHub page in any browser that supports modern JavaScript
3. Use your keyboard for input
   - alphabets (a-z) to enter letters
   - enter to submit (or click process!)
   - backspace to remove letters that you have entered