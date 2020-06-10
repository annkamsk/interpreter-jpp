#!/bin/bash

TEST_FILES_GOOD="mygood/*.latb"
TEST_FILES_BAD="mybad/*.latb"

echo "Testing good"
for f in $TEST_FILES_GOOD
do
  echo "Testing $f"
  expected="${f%.latb}.out"
  result="${f%.latb}_result.out"
  /usr/bin/stack build --exec "Interpret-exe $f" > "$result"
  diff "$expected" "$result"
done

echo "Testing bad"
for f in $TEST_FILES_BAD
do
  echo "Testing $f"
  expected="${f%.latb}.out"
  result="${f%.latb}_result.out"
  /usr/bin/stack build --exec "Interpret-exe $f" > "$result"
  diff "$expected" "$result"
done



