#!/usr/bin/env bash

base_dir="$(dirname "$0")"
jali="$base_dir/../bin/jali "
tests_dir="$base_dir/../tests"
failed=0
passed=0

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

echo "---------------------------------"
echo "Running tests from: $tests_dir"
for JALI in $tests_dir/*jali; do
    JALI=$(basename "$JALI")
    PROG=$(echo $JALI)
    FILENAME=$(echo $PROG | sed 's/.jali$//')
    TESTOUT=$tests_dir/$PROG-testresult
    PROGOUT=$tests_dir/$FILENAME.out

    $jali -i $tests_dir/$PROG | sed 's/Result://' > $TESTOUT 2>&1

    check_equal
done
echo "================================="
echo -e "Total: $((passed+failed)) – \033[92m$passed passed\033[0m – \033[91m$failed failed\033[0m"
