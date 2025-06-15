#!/bin/bash
# run.sh - One-click runner for OOLang interpreter

# 进入脚本所在目录
cd "$(dirname "$0")"

# 检查Racket是否安装
if ! command -v racket &> /dev/null; then
    echo "Error: Racket is not installed. Please install Racket v8.0+ and try again."
    exit 1
fi

# 运行所有测试
if [ "$1" = "test" ]; then
    echo ">>> Running all test cases"
    for test_file in tests/*.oo; do
        echo ""
        echo "===== Running $test_file ====="
        racket main.rkt "$test_file"
        echo "===== Finished $test_file ====="
    done
    echo ">>> All tests completed"
    exit 0
fi

# 运行单个文件
if [ -n "$1" ]; then
    echo ">>> Running $1"
    racket main.rkt "$1"
    exit 0
fi

# 交互式控制台
echo "===== OOLang Interactive Console ====="
echo "Enter OOLang expressions. Type 'exit' to quit."
echo ""

while true; do
    read -p "OOLang> " input
    
    if [ "$input" = "exit" ]; then
        break
    fi
    
    # 创建一个临时文件
    tmpfile=$(mktemp /tmp/oolang.XXXXXX)
    echo "$input" > "$tmpfile"
    
    # 运行表达式
    racket main.rkt "$tmpfile"
    
    # 清理临时文件
    rm "$tmpfile"
done