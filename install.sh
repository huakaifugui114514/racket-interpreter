#!/bin/bash
# install.sh - Install OOLang interpreter as a Racket package

# 进入脚本所在目录
cd "$(dirname "$0")"

# 检查Racket是否安装
if ! command -v racket &> /dev/null; then
    echo "Error: Racket is not installed. Please install Racket v8.0+ and try again."
    exit 1
fi

# 检查info.rkt文件是否存在
if [ ! -f "info.rkt" ]; then
    echo "Error: Missing info.rkt file. Cannot install package."
    exit 1
fi

# 安装包
echo ">>> Installing OOLang package..."
raco pkg install --auto --skip-installed

# 创建运行脚本链接
echo ">>> Creating runner script..."
cat > /usr/local/bin/oolang <<EOF
#!/bin/bash
racket -l oolang "\$@"
EOF

chmod +x /usr/local/bin/oolang

echo ">>> Installation complete!"
echo "You can now run OOLang programs with:"
echo "  oolang <filename.oo>"