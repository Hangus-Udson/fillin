/* COMP30020 Project Two
Author: Angus Hudson <ahudson1@student.unimelb.edu.au>
Student ID: 835808
Purpose: Implementing a fill-in puzzle solver using Prolog as according to
the Project 2 specifications.

This file contains all code necessary to solve a fill-in puzzle, provided
it is given as an NxN puzzle, fillable squares are marked as '_' and unfillable
squares are marked as '#'. The file operates using 'main', where one can
provide a filepath to a valid puzzle, and another filepath to a list of words
to fill in to generate an output file featuring a solution (if one exists).

The file is split into three distinct sections.

The first handles I/O, reading in files correctly and printing them out,
as well as checking whether puzzles given have valid dimensions.
All code in this section was provided by Peter Schachte as part of the
specification, and has been left untouched.

The second handles the process of converting a puzzle into a list of
Slots, where a Slot is defined as a list of free variables and pre-filled
values indicating an unfilled word. Unfilled squares in the puzzle are first
converted to unique free variables using Prolog's "logical variable"
functionality. Then the puzzle is scanned to generate the Slot list.

The third handles the process of solving a list of slots against the word
list. The process is thus: The slot/s with the fewest possible word matches
are found and identified as an index (N), then the slot is filled and
the slot/word combination is removed from the slot list and word list
respectively. The process repeats until failure (a slot has no possible words)
or a solution is found. */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SECTION ONE

:- ensure_loaded(library(clpfd)).

/** main/3
 * main(+PuzzleFile : string, +WordlistFile : string, -SolutionFile : string).
 *
 * Generates a solution for a puzzle/wordlist combination and puts it in a
 * new output file
 *
 * @param PuzzleFile The filepath for the puzzle.
 * @param WordlistFile The filepath for the wordlist.
 * @param SolutionFile The filepath for the solution.
 */
main(PuzzleFile, WordlistFile, SolutionFile) :-
  read_file(PuzzleFile, Puzzle),
  read_file(WordlistFile, Wordlist),
  valid_puzzle(Puzzle),
  solve_puzzle(Puzzle, Wordlist, Solved),
  print_puzzle(SolutionFile, Solved).

/** read_file/2
 * read_file(+FileName: string, -Content: list).
 *
 * Reads in a file, and converts it to a Prolog string
 *
 * @param FileName The filepath.
 * @param Content The contents of the file as a list of lines after reading.
 */
read_file(Filename, Content) :-
  open(Filename, read, Stream),
  read_lines(Stream, Content),
  close(Stream).

/** read_lines/2
 * read_lines(+Stream: stream, -Content: list).
 *
 * Reads in a set of lines of a file, and converts it to a Prolog string.
 * Helper for read_file/2
 *
 * @param Stream The I/O stream
 * @param Content The contents of the file as a list of lines after reading.
 */
read_lines(Stream, Content) :-
  read_line(Stream, Line, Last),
  (   Last = true
  ->  (   Line = []
      ->  Content = []
      ;   Content = [Line]
      )
  ;  Content = [Line|Content1],
      read_lines(Stream, Content1)
  ).

/** read_line/3
 * read_line(+Stream: stream, -Line: list, -Last: boolean).
 *
 * Reads in a single line of the stream.
 * Helper for read_lines/3
 *
 * @param Stream The I/O stream
 * @param Line The contents of the line being read
 * @param Last Whether or not the end of the file has been reached
 */
read_line(Stream, Line, Last) :-
  get_char(Stream, Char),
  (   Char = end_of_file
  ->  Line = [],
      Last = true
  ; Char = '\n'
  ->  Line = [],
      Last = false
  ;   Line = [Char|Line1],
      read_line(Stream, Line1, Last)
  ).

/** print_puzzle/2
 * print_puzzle(+SolutionFile: string, +Puzzle: list)

 * Prints the puzzle out
 *
 * @param SolutionFile The filepath of the solution file
 * @param Puzzle A list-of-rows of the read-in puzzle
 */
print_puzzle(SolutionFile, Puzzle) :-
  open(SolutionFile, write, Stream),
  maplist(print_row(Stream), Puzzle),
  close(Stream).

/** print_row/2
 * print_row(+Stream: stream, +Row: list).
 *
 * Prints a single row of the puzzle to stream
 *
 * @param Stream The I/O stream
 * @param Row A row of the puzzle
 */
print_row(Stream, Row) :-
  maplist(put_puzzle_char(Stream), Row),
  nl(Stream).

/** put_puzzle_char/2
 * put_puzzle_char(+Stream: stream, +Char: char).
 *
 * Prints a single character of the puzzle to stream
 *
 * @param Stream The I/O stream
 * @param Char A character of the puzzle
 */
put_puzzle_char(Stream, Char) :-
  (   var(Char)
  ->  put_char(Stream, '_')
  ;   put_char(Stream, Char)
  ).

/** valid_puzzle/1
 * valid_puzzle(+Puzzle: list).
 *
 * Checks if the puzzle is of NxN dimensions
 *
 * @param Puzzle A list-of-rows representing the puzzle
 */
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
  maplist(samelength(Row), Rows).

/** samelength/2
 * same_length(+L1: list, +L2: list).
 *
 * Checks if two lists are the same length
 *
 * @param L1 The first list
 * @param L2 The second list
 */
samelength([], []).
samelength([_|L1], [_|L2]) :-
  same_length(L1, L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SECTION TWO

/** generate_puzzle_vars/3
 * generate_puzzle_vars(+Puzzle: list, +Acc: list, -NewPuzzle: list).
 *
 * Replaces unfilled squares in the Puzzle with free variables in NewPuzzle
 *
 * @param Puzzle The original puzzle
 * @param Acc An accumulator
 * @param NewPuzzle The new puzzle featuring free variables
 */
generate_puzzle_vars([], Result, Result).
generate_puzzle_vars([Row|Rows], Acc, NewPuzzle) :-
  generate_row_vars(Row, [], NewRow),
  append(Acc, [NewRow], NewAcc),
  generate_puzzle_vars(Rows, NewAcc, NewPuzzle).

/** generate_row_vars/3
 * generate_row_vars(+Row: list, +Acc: list, -NewRow: list).
 *
 * Replaces unfilled squares in Row with free variables in NewRow
 * Helper for generate_puzzle_vars/3
 *
 * @param Row The original row
 * @param Acc An accumulator
 * @param NewRow The new row featuring free variables
 */
generate_row_vars([], Result, Result).
generate_row_vars([Head|Tail], Acc, NewRow) :-
  (  Head = '_' ->
     % Using Prolog's 'Logical Variable' to add unique free variables
       append(Acc, [NewVar], NewAcc)
     ;
       append(Acc, [Head], NewAcc)
  ),
  generate_row_vars(Tail, NewAcc, NewRow).

/** generate_all_slots/2
 * generate_all_slots(+Puzzle: list, -AllSlots: list).
 *
 * Generates a list of all valid slots for the puzzle
 *
 * @param Puzzle The puzzle
 * @param AllSlots A full list of valid slots for the puzzle
 */
generate_all_slots([], _).
generate_all_slots(Puzzle, AllSlots) :-
  generate_half_slots(Puzzle, [], HorizontalSlots),
  % We transpose to get column slots
  transpose(Puzzle, FlipPuzzle),
  generate_half_slots(FlipPuzzle, [], VerticalSlots),
  append(HorizontalSlots, VerticalSlots, AllSlots),
  generate_all_slots([], AllSlots).

/** generate_half_slots/3
 * generate_half_slots(+Puzzle: list, +Acc: list, -Slots: list).
 *
 * Generates a list of all valid slots for all rows or columns in puzzle
 * Helper for all_slots/2
 *
 * @param Puzzle The puzzle
 * @param Acc An accumulator for the list of slots
 * @param Slots A list of valid slots for all rows or columns in the puzzle
 */
generate_half_slots([], Result, Result).
generate_half_slots([Row|Rows], Acc, Slots) :-
  generate_row_slots(Row, [], [], RowSlots),
  append(Acc, RowSlots, NewAcc),
  generate_half_slots(Rows, NewAcc, Slots).

/** generate_row_slots/4
 * generate_row_slots(+Row: list, +SlotStack: list, +Acc: list, -RowSlots: list).
 *
 * Generates a list of all valid slots in a row or column of the puzzle
 * Helper for half_slots/3
 *
 * @param Row A row/column of the puzzle
 * @param SlotStack Used to accumulate the current slot
 * @param Acc An accumulator for the list of slots
 * @param RowSlots A list of valid slots within Row
 */
generate_row_slots([], [], Result, Result).
generate_row_slots([], SlotStack, Acc, RowSlots) :-
  length(SlotStack, Len),
  % Only accept slots that have length > 1
  (  Len > 1 ->
       append(Acc, [SlotStack], NewAcc),
       generate_row_slots([], [], NewAcc, RowSlots)
   ;
       generate_row_slots([], [], Acc, RowSlots)
  ).
generate_row_slots([Head|Tail], SlotStack, Acc, RowSlots) :-
  % If Head is a free variable or a prefilled value, add to SlotStack
  (  (var(Head) ; Head \= '#') ->
        append(SlotStack, [Head], NewSS),
        generate_row_slots(Tail, NewSS, Acc, RowSlots)
   ;
  % Have reached an unfillable square, so add the slot if its big enough
     length(SlotStack, Len),
     (  Len > 1 ->
          append(Acc, [SlotStack], NewAcc),
    	    generate_row_slots(Tail, [], NewAcc, RowSlots)
        ;
          generate_row_slots(Tail, [], Acc, RowSlots)
     )
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SECTION THREE

/** solve_puzzle/3
 * solve_puzzle(+Puzzle: list, +WordList: list, -Solved: list).
 *
 * Solves the puzzle in a more general sense, returning a new 'solved' puzzle
 *
 * @param Puzzle The unsolved puzzle
 * @param WordList A list of all words in the puzzle
 * @param SolvedPuzzle The solved puzzle
 */
solve_puzzle(Puzzle, WordList, SolvedPuzzle) :-
  % Convert unfilled squares into free variables
  generate_puzzle_vars(Puzzle,[], SolvedPuzzle),
  % Generate the set of slots for the puzzle
  generate_all_slots(SolvedPuzzle, AllSlots),
  % Find a single solution for the set of slots against the word list
  solve(AllSlots, WordList), !.

/** solve/2
 * solve(+AllSlots: list, +WordList: list).
 *
 * Solves the puzzle in terms of its slots and its wordlist, matching and
 * filling in all slots in AllSlots with words from WordList
 *
 * @param AllSlots A list of all slots in the puzzle
 * @param WordList A list of all words in the puzzle
 */
solve([],[]).
solve(AllSlots, WordList) :-
  % First create the list of possible words for each slot
  generate_slotwordlists(AllSlots, WordList, SlotWordLists),
  % Now find the slot with the fewest word matches
  find_best_slot(SlotWordLists, N),
  % Generate variables for the slot, as well as its corresponding word list
  nth1(N, AllSlots, Slot, RemSlots),
  nth1(N, SlotWordLists, SlotWords),
  % Unify the slot in with each of its possible words
  fill_slot(Slot, SlotWords),
  % Now remove the slot and its associated word list from consideration
  remove_all([Slot], AllSlots, RemSlots),
  remove_all([Slot], WordList, RemWordList),
  % And keep going until both of the lists are empty, indicating the puzzle
  % being successfully solved, with all free variables assigned
  solve(RemSlots, RemWordList).

/** generate_slotwordlists/3
 * generate_slotwordlists(+SlotList: list, +WordList: list,
 *   -SlotWordLists: list).
 *
 * Generates a list of lists of all possible words every slot could match with
 *
 * @param SlotList A list of slots
 * @param WordList A list of words to fill in slots
 * @param SlotWordList A list of possible valid words for each slot
 */
generate_slotwordlists([], _, []).
generate_slotwordlists([CurrSlot|RestSlots], WordList, SlotWordLists) :-
  generate_slotwords(CurrSlot, WordList, SlotWords),
  append([SlotWords], SlotWordListsTemp, SlotWordLists),
  generate_slotwordlists(RestSlots, WordList, SlotWordListsTemp).

/** generate_slotwords/3
 * generate_slotwords(+Slot: list, +WordList: list, -SlotWords: list).
 *
 * Generates a list of lall possible words a slot could match to
 * Helper for generate_slotwordlists/3
 *
 * @param Slot The element to look for
 * @param WordList The list to look through
 * @param SlotWords A list of possible words to fit in Slot
 */
generate_slotwords(_, [], []).
generate_slotwords(Slot, [Head|Tail], SlotWords) :-
  % If a slot and word have same length, and are unifiable
  % we have a possible combination
  (  can_unify(Slot, Head) ->
       append([Head], NewSlotWords, SlotWords),
       generate_slotwords(Slot, Tail, NewSlotWords)
     ;
       generate_slotwords(Slot, Tail, SlotWords)
  ).

/** can_unify/2
 * can_unify(+Slot: list, +Word: list).
 *
 * Checks to see if a slot can unify with a word
 * Helper for generate_slotwords/3
 *
 * @param Slot The slot to inspect
 * @param Word The word we want to put in slot
 */
can_unify([], []).
can_unify([HeadX|TailXs], [HeadY|TailYs]) :-
  % HeadX is a free variable, so it can be any value
  (  var(HeadX) ->
       can_unify(TailXs, TailYs)
     ;
  % Two prefilled squares with same value, so good to go
     nonvar(HeadX), nonvar(HeadY), HeadX = HeadY ->
       can_unify(TailXs, TailYs)
     ;
  % Can't unify, so return failure immediately
       can_unify([HeadX], [])
  ).

/** find_best_slot/2
 * find_best_slot(+SlotWordLists: list, -N: int).
 *
 * Finds the slot/s with the fewest word matches
 *
 * @param SlotWordLists A list of possible words for every slot
 * @param N The index of the slot with fewest word matches
 */
find_best_slot(SlotWordLists, N) :-
  len_list(SlotWordLists, LenList),
  min_list(LenList, Mth),
  nth1(N, LenList, Mth).

/** len_list/2
 * len_list(+SlotWordLists: list, -LenList: list).
 *
 * Generates a list of the lengths of each wordlist for each slot
 * Helper for find_best_slot/2
 *
 * @param SlotWordLists A list of possible words for every slot
 * @param LenList A list of lengths of each list in SlotWordLists
 */
len_list([],[]).
len_list([Head|Tail], LenList) :-
  length(Head, Len),
  append([Len], LenListTemp, LenList),
  len_list(Tail, LenListTemp).

/** fill_slot/2
 * fill_slot(+Slot: list, +SlotWords: list).
 *
 * Unify a slot with all possible words it can unify with
 *
 * @param Slot The slot to fill in
 * @param SlotWords A list of all words the slot can unify with
 */
fill_slot(Slot, [Slot]).
fill_slot(Slot, [Word|Words]) :-
  % Use 'or' operation to account for ALL possible slot/word combinations
  Slot = Word ; fill_slot(Slot, Words).

/** remove_all/3
 * remove_all(+Lst: list, +Elem: list, -NewLst: list).
 *
 * Removes all instances of particular elements from a list
 * Helper for solve/2
 *
 * @param Lst The original list
 * @param Elem A list of elements to remove
 * @param NewLst The new reduced list
 */
remove_all(_, [], []).
remove_all(Elem, [Head|Tail], NewList) :-
  in_list(Head, Elem),
  remove_all(Elem, Tail, NewList).
remove_all(Elem, [Head|Tail], [Head|NewListTail]) :-
  remove_all(Elem, Tail, NewListTail).

/** in_list/2
 * in_list(+Elem: list, +Lst: list).
 *
 * Checks if an element exists in a list
 * Helper for remove_elems/3
 *
 * @param Elem The element to look for
 * @param Lst The list to look through
 */
in_list(Elem, [Head|_]) :- Elem == Head.
in_list(Elem, [_|Tail]) :- in_list(Elem, Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
