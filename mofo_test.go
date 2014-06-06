package mofo

import (
  "io/ioutil"
  "os"
  "os/exec"
  "strings"
  "testing"
)

func oneliner(t *testing.T, in, out string) {
  c := exec.Command("./mofo")
  c.Stdin = strings.NewReader(in)
  b, err := c.CombinedOutput()
  if err != nil {
    t.Fatal("input:", in, "runtime error:", err)
  }
  s := string(b)
  want := out + " ok\n";
  if s != want {
    t.Error("input:", in, "\nwant:", want, "got:", s);
  }
}

// Test examples from "Starting Forth" by Leo Brodie.
func TestStartingForthExamples(t *testing.T) {
  for _, v := range []struct { in, out string }{
    // Chapter 1.
    { ": star 42 emit ; star", "*" },
    { ": star 42 emit ; : stars 0 do star loop ; 35 stars",
      "***********************************" },
    { "3 4 + .", "7 " },

    // Chapter 2.
    { ": 5#SUM + + + + ; 17 20 132 3 9 5#SUM .", "181 " },
    { "2 10 4 - SWAP / .", "3 " },
    { "1 2 3 ROT .S", "<3> 2 3 1 " },

    // Chapter 4.
    { ": ?FULL  12 = IF  .\" It's full\"  THEN ; 11 ?FULL", "" },
    { ": ?FULL  12 = IF  .\" It's full\"  THEN ; 12 ?FULL", "It's full" },
    { "FALSE INVERT .", "-1 " },
    { "TRUE INVERT .", "0 " },
    { ": BOXTEST ( length width height -- ) 6 >  ROT 22 >  ROT 19 >  AND AND IF .\" Big enough\" THEN ; 23 20 7 BOXTEST", "Big enough" },

    // Chapter 5.
    { ": R%  10 */  5 +  10 / ; 227 32 R% .", "73 " },
    { ": DIFFERENCE - ABS ; 52 37 DIFFERENCE .", "15 " },
    { ": DIFFERENCE - ABS ; 37 52 DIFFERENCE .", "15 " },
    { ": COMMISSION   10 /  50 MIN ; 600 COMMISSION .", "50 " },
    { ": COMMISSION   10 /  50 MIN ; 450 COMMISSION .", "45 " },
    { ": noname >R SWAP R> ; 2 3 1 noname .S", "<3> 3 2 1 " },
    { ": QUADRATIC  ( a b c x -- n ) >R SWAP ROT R@ *  + R> *  + ; 2 7 9 3 QUADRATIC .", "48 " },

    // Chapter 6.
    { ": DECADE  10 0 DO  I .  LOOP ; DECADE", "0 1 2 3 4 5 6 7 8 9 " },
    { ": MULTIPLICATIONS CR 11 1 DO DUP I * .  LOOP DROP ; 7 MULTIPLICATIONS",
      "\n7 14 21 28 35 42 49 56 63 70 " },
    { ": PENTAJUMPS  50 0 DO  I .  5 +LOOP ; PENTAJUMPS",
      "0 5 10 15 20 25 30 35 40 45 " },
    { ": FALLING  -10 0 DO  I .  -1 +LOOP ; FALLING",
      "0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 " },
    { ": DOUBLING   32767 1 DO  I . I +LOOP ; DOUBLING",
      "1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 " },

    // Chapter 8.
    { "VARIABLE DATE 12 DATE ! DATE @ . 13 DATE ! DATE ?", "12 13 " },
    { "CREATE LIMITS  220 , 340 , 170 , 100 , 190 ,  LIMITS CELL+ @ .",
      "340 " },
  } {
    oneliner(t, v.in, v.out)
  }
}

func TestBeginAgain(t *testing.T) {
  // Implicitly tests Chapter 11 of Brodie, because we define:
  //   : begin here ; immediate
  oneliner(t, ": foo begin 1+ dup . dup 50 = if exit then again ; 47 foo", "48 49 50 ")
}

func TestUTF8(t *testing.T) {
  oneliner(t, "hex 5e8a emit 524d emit 660e emit 6708 emit 5149 emit", "床前明月光")
  oneliner(t, "hex char 床 . char 前 . char 明 . char 月 . char 光 .", "5e8a 524d 660e 6708 5149 ")
}

func filer(t *testing.T, in, want string) {
  f, err := ioutil.TempFile("", "test")
  if err != nil {
    t.Fatal("TempFile:", err);
  }
  f.Close()
  err = ioutil.WriteFile(f.Name(), []byte(in), 0777)
  if err != nil {
    t.Fatal("WriteFile:", err);
  }
  defer os.Remove(f.Name())

  c := exec.Command("./mofo", f.Name())
  c.Stdin = strings.NewReader("bye\n")
  b, err := c.CombinedOutput()
  if err != nil {
    t.Fatal("input:", in, "runtime error:", err)
  }
  s := string(b)
  if s != want {
    t.Error("input:", in, "\nwant:", want, "got:", s);
  }
}

func TestRecursion(t *testing.T) {
  filer(t, `
: factorial dup 1 > if dup 1 - factorial * then ;
10 factorial .
: *factorial dup 0> if dup 1- -rot * swap *factorial else drop then ;
: factorial 1 swap *factorial ;
20 factorial .
: choose dup if over 1- over 1- choose rot * swap / else drop drop 1 then ;
100 50 choose .
`, "3628800 2432902008176640000 100891344545564193334812497256 ")
}

// Also from "Starting Forth" by Leo Brodie.
func TestChapter5LongerExample(t *testing.T) {
  filer(t,
`  : R%  10 */  5 +  10 / ;
   : DOUBLED
     6 1000 21 1 DO  CR ." YEAR " I 2 U.R
           2DUP R% +  DUP ."    BALANCE " .
           DUP 2000 > IF  CR CR ." more than doubled in "
                             I . ." years " LEAVE
                    THEN
      LOOP 2DROP ;
   DOUBLED
`, `
YEAR  1   BALANCE 1060 
YEAR  2   BALANCE 1124 
YEAR  3   BALANCE 1191 
YEAR  4   BALANCE 1262 
YEAR  5   BALANCE 1338 
YEAR  6   BALANCE 1418 
YEAR  7   BALANCE 1503 
YEAR  8   BALANCE 1593 
YEAR  9   BALANCE 1689 
YEAR 10   BALANCE 1790 
YEAR 11   BALANCE 1897 
YEAR 12   BALANCE 2011 

more than doubled in 12 years `)
}

func TestChapter9LongerExample(t *testing.T) {
  filer(t, `
    ( 1 ) : HELLO    ." Hello " ;
    ( 2 ) : GOODBYE  ." Goodbye " ;
    ( 3 ) VARIABLE 'aloha  ' HELLO 'aloha !
    ( 4 ) : ALOHA    'aloha @ EXECUTE ;
    ALOHA
    ' GOODBYE 'aloha !
    ALOHA
    : SAY ' 'aloha ! ;
    SAY HELLO
    ALOHA
    SAY GOODBYE
    ALOHA
    : COMING ['] HELLO   'aloha ! ;
    : GOING  ['] GOODBYE 'aloha ! ;
    COMING
    ALOHA
    GOING
    ALOHA
`, `Hello Goodbye Hello Goodbye Hello Goodbye `)
}

func TestChapter11LongerExample(t *testing.T) {
  filer(t, `\ Shapes, using a defining word.

DECIMAL

: star  [CHAR] * EMIT ;

: .row  CR 8 0 DO
   DUP 128 AND IF  star
     ELSE  SPACE
     THEN
   1 LSHIFT
     LOOP  DROP ;

: SHAPE CREATE  8 0 DO  C,  LOOP
  DOES> DUP 7 + DO  I C@ .row  -1 +LOOP  CR ;

  HEX  18 18 3C 5A 99 24 24 24  SHAPE man
  81 42 24 18 18 24 24 81  SHAPE equis
  AA AA FE FE 38 38 38 FE  SHAPE castle
  DECIMAL
  man
`, `
   **   
   **   
  ****  
 * ** * 
*  **  *
  *  *  
  *  *  
  *  *  
`)
}


// Partial explanation:
// http://en.literateprograms.org/Eight_queens_puzzle_(Forth)
// Uses cool bit-twiddling tricks.
func TestEightQueens(t *testing.T) {
  filer(t,
`variable solutions
variable nodes

: bits ( n -- mask ) 1 swap lshift 1- ;
: lowBit  ( mask -- bit ) dup negate and ;
: lowBit- ( mask -- bits ) dup 1- and ;

: next3 ( dl dr f files -- dl dr f dl' dr' f' )
  invert >r
  2 pick r@ and 2* 1+
  2 pick r@ and 2/
  2 pick r> and ;

: try ( dl dr f -- )
  dup if
    1 nodes +!
    dup 2over and and
    begin ?dup while
      dup >r lowBit next3 recurse r> lowBit-
    repeat
  else 1 solutions +! then
  drop 2drop ;

: queens ( n -- )
  0 solutions ! 0 nodes !
  -1 -1 rot bits try
  solutions @ . ." solutions, " nodes @ . ." nodes" ;

8 queens
`, `92 solutions, 1965 nodes`)
}

// http://en.literateprograms.org/Fixed-point_arithmetic_(Forth)
func TestMandelbrot(t *testing.T) {
  filer(t,
`hex
 4000 constant 1fixed
 4666 constant 1.1fixed
10000 constant 4fixed
decimal
1fixed  3 * 80 / constant xinc
1.1fixed 2* 24 / constant yinc

: *f ( f g -- f*g ) 1fixed */ ;
: sq ( f -- f f*f ) over dup *f ;
: mandel
  1.1fixed dup negate do
    1fixed dup 2* negate do
      i j 30                 ( initial point x,y and max iteration count )
      begin  1- ?dup
      while  -rot sq sq
             2dup + 4fixed <
      while  - i +
             -rot *f 2* j + rot
      repeat 2drop drop          \ exit from second while
             space
      else   ." *"               \ exit from first while
      then 2drop
    xinc +loop
    cr
  yinc +loop ;
mandel
`,
`                                                                                 
                                                                                 
                                                                                 
                                               * ****                            
                                                ******                           
                                           **  ******* * *     *                 
                                       ************************                  
                                     *************************                   
                                    ***************************                  
                       *   * *    ******************************                 
                      *********** ******************************                 
                     *******************************************                 
      *   *   ***********************************************                    
                    ********************************************                 
                      *********** ******************************                 
                       *   * *    ******************************                 
                                    ***************************                  
                                     *************************                   
                                       ************************                  
                                            *  ******* ***     *                 
                                                *****                            
                                                *****                            
                                                                                 
                                                                                 
                                                                                 
`)
}

// http://rosettacode.org/wiki/Conway's_Game_of_Life#Forth
// Commented out ": life" definition because of "at-xy" and "key?".
func TestGameOfLife(t *testing.T) {
  filer(t, `
 \ The fast wrapping requires dimensions that are powers of 2.
 1 6 lshift constant w \ 64
 1 4 lshift constant h \ 16
 
 : rows    w * 2* ;
 1 rows constant row
 h rows constant size
 
 create world size allot
 world   value old
 old w + value new
 
 variable gens
 : clear  world size erase     0 gens ! ;
 : age  new old to new to old  1 gens +! ;
 
 : col+  1+ ;
 : col-  1- dup w and + ; \ avoid borrow into row
 : row+  row + ;
 : row-  row - ;
 : wrap ( i -- i ) [ size w - 1- ] literal and ;
 : w@ ( i -- 0/1 ) wrap old + c@ ;
 : w! ( 0/1 i -- ) wrap old + c! ;
 
 : foreachrow ( xt -- )
   size 0 do  I over execute  row +loop drop ;
 
 : showrow ( i -- ) cr
   old + w over + swap do I c@ if [char] * else bl then emit loop ;
 : show  ['] showrow foreachrow  cr ." Generation " gens @ . ;
 
 : sum-neighbors ( i -- i n )
   dup  col- row- w@
   over      row- w@ +
   over col+ row- w@ +
   over col-      w@ +
   over col+      w@ +
   over col- row+ w@ +
   over      row+ w@ +
   over col+ row+ w@ + ;
 : gencell ( i -- )
   sum-neighbors  over old + c@
   or 3 = 1 and   swap new + c! ;
 : genrow ( i -- )
   w over + swap do I gencell loop ;
 : gen  ['] genrow foreachrow  age ;
 
 \ : life  begin gen 0 0 at-xy show key? until ;
 
 \ patterns
 char | constant '|'
 : pat ( i addr len -- )
   rot dup 2swap  over + swap do
     I c@ '|' = if drop row+ dup else
     I c@ bl  = 1+ over w!  col+ then
   loop 2drop ;
 
 : blinker s" ***" pat ;
 : toad s" ***| ***" pat ;
 : pentomino s" **| **| *" pat ;
 : pi s" **| **|**" pat ;
 : glider s"  *|  *|***" pat ;
 : pulsar s" *****|*   *" pat ;
 : ship s"  ****|*   *|    *|   *" pat ;
 : pentadecathalon s" **********" pat ;
 : clock s"  *|  **|**|  *" pat ;

clear 0 glider show gen show
`, `
 *                                                              
  *                                                             
***                                                             
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
Generation 0 
                                                                
* *                                                             
 **                                                             
 *                                                              
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
Generation 1 `)
}
