# Word Processor
An improved version of a word processor that uses a binary tree structure for indexing words to improve the time complexity from O(n) to O(log n). Counts the ”value” of each word using the scoring rules of the game Scrabble™ to find out how much the author should be paid for  their work.

To run this in ghci, use the command `:load JoinList` to import all the required modules. Then enter `main` to start the editor.

The editor interface is as follows:
  - v — view the current location in the document
  - n — move to the next line
  - p — move to the previous line
  - l — load a file into the editor
  - e — edit the current line
  - q — quit
  - ? — show this list of commands

To move to a specific line, enter the line number you wish to navigate to at the prompt. The display shows you up to two preceding and two following lines in the document surrounding the current line, which is indicated by an asterisk. The prompt itself indicates the current value of the entire document.
