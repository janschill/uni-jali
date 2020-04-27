#!/usr/bin/env bash

base_dir="$(dirname "$0")"
jali="$base_dir/../bin/jali "
tests_dir="$base_dir/../tests"
tests_reduction_dir="$tests_dir/reduction"
tests_evaluate_dir="$tests_dir/evaluate"
DIR=''
failed=0
passed=0
flagarg=''

fix_whitespace() {
    cat "$1" | tr -d '\000' | tr -d ' \t\n\r\f' 1>&1
}

check_equal() {
    EXPECTED=$(fix_whitespace "$PROGOUT")
    ACTUAL=$(fix_whitespace "$TESTOUT")
    if [ "$EXPECTED" = "$ACTUAL" ]; then
        echo -e "\033[92mTest $FILENAME passed\033[0m"
        rm -f $TESTOUT
        passed=$((passed+1))
    else
        echo -e "\033[91mTest $FILENAME failed\033[0m"
        echo -e "\033[0;34mActual:\033[0m"
        echo $ACTUAL
        echo ""
        echo -e "\033[0;34mExpected:\033[0m"
        echo $EXPECTED
        echo ""
        failed=$((failed+1))
    fi
}

while getopts 'ir' flag; do
  case "${flag}" in
    i) flagarg='-i'
       DIR=$tests_evaluate_dir
    ;;
    r) flagarg='-r'
       DIR=$tests_reduction_dir
    ;;
    *) print_usage
       exit 1 ;;
  esac
done

echo "---------------------------------"
echo "Running tests from: $tests_dir"
for JALI in $DIR/*jali; do
    JALI=$(basename "$JALI")
    PROG=$(echo $JALI)
    FILENAME=$(echo $PROG | sed 's/.jali$//')
    TESTOUT=$DIR/$PROG-testresult
    PROGOUT=$DIR/out/$FILENAME.out

    $jali $flagarg $DIR/$PROG | sed 's/Result://' > $TESTOUT 2>&1

    check_equal
done
echo "================================="
echo -e "Total: $((passed+failed)) – \033[92m$passed passed\033[0m – \033[91m$failed failed\033[0m"
