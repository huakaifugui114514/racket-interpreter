#!/bin/bash
# run.sh - One-click test runner for OOLang tests

cd "$(dirname "$0")"

if ! command -v racket &> /dev/null; then
    echo "Error: Racket is not installed. Please install Racket v8.0+ and try again."
    exit 1
fi

run_test_file() {
    local test_file="$1"
    echo "===== Running $test_file ====="
    if racket main.rkt "$test_file"; then
        echo "===== Finished $test_file (success) ====="
        return 0
    else
        echo "===== Finished $test_file (failed) ====="
        return 1
    fi
    echo ""
}

if [ "$1" = "all" ]; then
    echo ">>> Running all test cases"
    for test_file in tests/*.oo; do
        run_test_file "$test_file" || true
    done
    echo ">>> All tests completed"
    exit 0
fi

if [ -n "$1" ]; then
    run_test_file "$1"
    exit $?
fi

echo "Usage:"
echo "  ./run.sh all               Run all test cases"
echo "  ./run.sh <test-file.oo>    Run specific test file"
echo ""
echo "Available test files:"
ls tests/*.oo | sed 's/^/  /'