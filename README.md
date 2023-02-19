# Countdown-Game-Haskell
Countdown is a British1 TV game show that is still airing nowadays. In one of the games, the<br>
contestants are given a list of numbers, as well as a target number, and attempt to reach the target number<br>
by applying the basic arithmetic operations +, *, - and / using the numbers they are given. There are some<br>
natural restrictions in how they can use those numbers:<br>
• Negative or fractional numbers are not allowed at any stage of a computation. So for instance, (3−5)+10<br>
and 5/2 + 3/2 are never valid answers.<br>
• All numbers given to reach the target must be used at most once in the computation. For instance, if<br>
the list 5, 5, 4, 3, 2 is given, 5 + 5 − 4 and 3 ∗ 2 are valid answers to reach 6, but 5 − 4 + 3 + 4 − 2 is not.<br>
To limit the search space, let us only consider expressions including + and * for this question.<br>
Write a function<br>
<b>countdownAllSolutions :: [Int] -> Int -> [Expr]</b><br>
that takes as input a list of numbers, a target number, and lists all of the arithmetical expression that a<br>
contestant could use to compute the target from the given number.
