(********************************* Problem 1 *********************************)

(* mynull takes in a list and returns a bool indicating whether the list is 
empty or not*)

fun mynull [] = true
|   mynull _ = false

    (* UNIT TESTS *)
    val () = Unit.checkAssert "mynull empty list"
            (fn () => mynull [])

    val () = Unit.checkAssert "mynull non- empty list"
            (fn () => not (mynull [1, 2, 3]))

(*****************************************************************************)
(********************************* Problem 2 *********************************)
(************************************* 2a ************************************)

(* reverse takes in a list and returns the reverse of that list *)

fun reverse [] = []
|   reverse (x :: xs) = List.foldl (op ::) [x] xs


    (* UNIT TESTS *)
    val () = Unit.checkExpectWith (Unit.listString Unit.stringString) "r empty"
            (fn () => reverse [] )  
            []

    val () = Unit.checkExpectWith (Unit.listString Unit.stringString) "strings"
            (fn () => reverse ["hi", "hello"] )  
            ["hello", "hi"]

    val () = Unit.checkExpectWith (Unit.listString Unit.intString) "r ints"
            (fn () => reverse [1, 2, 3, 4] )  
            [4, 3, 2, 1]

    val () = Unit.checkExpectWith (Unit.listString Unit.intString) "r singelton"
            (fn () => reverse [1] )  
            [1]

(************************************* 2b ************************************)

(* minlist takes in a nonempty list of integers and  returns the smallest 
element of the list *)

fun minlist [] = raise Match
|   minlist (x :: xs) = List.foldl Int.min x xs


        (* UNIT TESTS *)
    val () = Unit.checkExpectWith Unit.intString "minlist singleton"
            (fn () => minlist [1])
            1

    val () = Unit.checkExpectWith  Unit.intString "minlist int"
            (fn () => minlist [1, 2, 3, 4] )  
            1 

    val () = Unit.checkExpectWith  Unit.intString "minlist with negs"
            (fn () => minlist [1, ~2, 3, ~4] )  
            ~4 
    
        val () =
        Unit.checkExnWith Unit.intString "minlist empty list"
        (fn () => minlist [])


(*****************************************************************************)
(********************************* Problem 3 *********************************)

(* defining exception Mismatch*)
exception Mismatch

(* zip takes a pair of lists of equal length and returns the equivalent list of
pairs*)

fun zip ([], []) = []
|   zip ((x :: xs), (y :: ys)) = (x, y) :: zip (xs, ys)
|   zip (_, _) = raise Mismatch


    (* string builders *)
    val int_pair_toString = Unit.pairString Unit.intString Unit.intString
    val list_pair_toString = Unit.listString int_pair_toString

    (* UNIT TESTS *)
    val () =
        Unit.checkExpectWith list_pair_toString "zip on integer lists"
        (fn () => zip ([1, ~2, 4], [9, 12, 0]))
        [(1, 9), (~2, 12), (4, 0)]

    val () =
        Unit.checkExpectWith list_pair_toString "zip on empty lists"
        (fn () => zip ([], []))
        []
        
        val () =
        Unit.checkExnWith list_pair_toString "zip on lists of unequal length"
        (fn () => zip ([1, ~2, 4], [9, 12]))

    
(*****************************************************************************)
(********************************* Problem 4 *********************************)

(* pairfoldrEq takes in a function f, an accumulator init and a pair of lists of
equal length and returns the equivalent list of pairs*)

fun pairfoldrEq f init ((x :: xs), (y :: ys)) =  f x y (pairfoldrEq f init 
                                                                    (xs, ys))
|   pairfoldrEq f init ([], []) = init
|   pairfoldrEq f init (_, _) = raise Mismatch

    (* UNIT TESTS *)
    fun mSum x y z = if z >= x + y then z else x + y
    val () = Unit.checkExpectWith Unit.intString "maxProd of int lists"
                    (fn () => pairfoldrEq mSum 0 ([1, 2, 3], [2, 3, 4]))
                    7

(* addPair is a helper function takes in three arguments, makes a pair of the 
first two, i.e., a and b and adds it to list c *)

fun addPair a b c = (a, b) :: c

(* ziptoo takes a pair of lists of equal length and returns the equivalent list
of pairs. It does so by making use of the pairfoldrEq function*)

fun ziptoo (xs, ys)  = pairfoldrEq addPair [] (xs, ys)

    (* UNIT TESTS *)
    val () = Unit.checkExpectWith list_pair_toString "zip on integer lists"
            (fn () => ziptoo ([1, ~2, 4], [9, 12, 0]))
            [(1, 9), (~2, 12), (4, 0)]


(*****************************************************************************)
(********************************* Problem 5 *********************************)

(* concat takes a list of lists of 'a and produces a single list of 'a 
containing all the elements in the correct order *)

fun concat [] = []
|   concat (x::xs) = x @ concat xs

    (* UNIT TESTS *)
    val () = Unit.checkExpectWith  (Unit.listString Unit.intString) "ints"
            (fn () => concat [[1], [~2], [3, ~4]] )  
            [1, ~2, 3, ~4]
    
    val () = Unit.checkExpectWith  (Unit.listString Unit.intString) "empty"
            (fn () => concat [[], []] )  
            []

(*****************************************************************************)
(********************************* Problem 6 *********************************)

(* representation of S-expressions *)
datatype ordsx 
  = BOOL of bool
  | NUM  of int
  | SYM  of string
  | SXS  of ordsx list

(* string builder for S-expressions *)
fun sxString (SYM s)   = s
  | sxString (NUM n)   = Unit.intString n
  | sxString (BOOL b)  = if b then "true" else "false"
  | sxString (SXS sxs) = "(" ^ String.concatWith " " (map sxString sxs) ^ ")"

(* numbersSx takes in a list of numbers and converts it into an ordinary 
S-expression *)

fun numbersSx xs = SXS (map (fn (x) => NUM x) xs)

    (* UNIT TESTS *)
    val () = Unit.checkExpectWith  sxString "list"
        (fn () => numbersSx [1, ~2, 3] )  
        (SXS [NUM 1, NUM ~2, NUM 3])

    val () = Unit.checkExpectWith  sxString "singelton"
        (fn () => numbersSx [1] )  
        (SXS [NUM 1])

    val () = Unit.checkExpectWith  sxString "empty"
                    (fn () => numbersSx [] )  
                    (SXS [])

(* flattenSyms takes in an ordinary S-expression and extracts just the symbols 
from the S-expression, returning a list of the extracted symbols *)

fun flattenSyms (SYM s) = [s]
|   flattenSyms (SXS xs) = List.foldr (fn (a, b) => (flattenSyms a) @ b ) [] xs
|   flattenSyms _ = []


    (* UNIT TESTS *)
    val () = Unit.checkExpectWith  (Unit.listString Unit.stringString) "1"
            (fn () => flattenSyms (SXS [SYM "s", NUM ~2, NUM 3]))
            ["s"]

    val () = Unit.checkExpectWith  (Unit.listString Unit.stringString) "2"
            (fn () => flattenSyms (SYM "s") )  
            ["s"]

    val () = Unit.checkExpectWith  (Unit.listString Unit.stringString) "3"
            (fn () => flattenSyms (SXS [SYM "s", BOOL true, SYM "cs105"]))  
            ["s", "cs105"]

    val () = Unit.checkExpectWith  (Unit.listString Unit.stringString) "4"
            (fn () => flattenSyms (SXS [NUM ~2, BOOL true]) )  
            []

(*****************************************************************************)
(*****************************************************************************)

val () = Unit.reportWhenFailures ()
