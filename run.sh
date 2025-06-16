#!/bin/bash
# run.sh - One-click runner for OOLang interpreter

cd "$(dirname "$0")"

if ! command -v racket &> /dev/null; then
    echo "Error: Racket is not installed. Please install Racket v8.0+ and try again."
    exit 1
fi

run_file() {
    echo "===== Running $1 ====="
    if racket -t "main.rkt" -m "$1"; then
        echo "===== Finished $1 (success) ====="
        return 0
    else
        echo "===== Finished $1 (failed) ====="
        return 1
    fi
    echo ""
}

if [ "$1" = "test" ]; then
    echo ">>> Running all test cases"
    for test_file in tests/*.oo; do
        run_file "$test_file" || true
    done
    echo ">>> All tests completed"
    exit 0
fi

if [ -n "$1" ]; then
    run_file "$1"
    exit $?
fi

echo "===== OOLang Interactive Console ====="
echo "Enter OOLang expressions. Type 'exit' to quit."
echo ""

while true; do
    read -p "OOLang> " input
    
    if [ "$input" = "exit" ]; then
        break
    fi
    
    tmpfile=$(mktemp /tmp/oolang.XXXXXX)
    echo "$input" > "$tmpfile"
    
    if ! racket -t "main.rkt" -m "$tmpfile"; then
        echo "Error evaluating expression"
    fi
    
    rm "$tmpfile"
done