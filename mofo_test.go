package mofo

import (
  "os/exec"
  "strings"
  "testing"
)

func oneliner(t *testing.T, in, out string) {
  c := exec.Command("./mofo")
  c.Stdin = strings.NewReader(in)
  b, err := c.CombinedOutput()
  if err != nil {
    t.Fatal("runtime error:", err)
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
    { ": DIFFERENCE - ABS ; 52 37 DIFFERENCE .", "15 " },
    { ": DIFFERENCE - ABS ; 37 52 DIFFERENCE .", "15 " },
    { ": COMMISSION   10 /  50 MIN ; 600 COMMISSION .", "50 " },
    { ": COMMISSION   10 /  50 MIN ; 450 COMMISSION .", "45 " },

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

func TestRecursion(t *testing.T) {
  for _, v := range []struct { in, out string }{
    { ": fact dup 1 > if dup 1 - fact * then ; 10 fact .", "3628800 " },
    { ": choose dup if over 1- over 1- choose rot * swap / else drop drop 1 then ; 100 50 choose .",
       "100891344545564193334812497256 " },
  } {
    oneliner(t, v.in, v.out)
  }
}
