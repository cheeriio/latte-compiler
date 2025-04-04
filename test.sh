#!/bin/bash

for file in tests/good/*.lat; do
    X=$(basename "$file" .lat)
    
    ./latc_x86_64 "tests/good/${X}.lat" &>/dev/null
    
    if [ ! -f "tests/good/${X}" ]; then
        echo "FAIL - $X (compilation failed)"
        continue
    fi
    
    if [ -f "tests/good/${X}.input" ]; then
        "./tests/good/${X}" < "tests/good/${X}.input" > "temp_output"
    else
        "./tests/good/${X}" > "temp_output"
    fi
    
    if diff -q "temp_output" "tests/good/${X}.output" > /dev/null; then
        echo "OK - $X"
    else
        echo "FAIL - $X - Incorrect output"
    fi
    
    rm -f "temp_output" "$X"
    rm -f "tests/good/${X}"
done

for file in tests/bad/*.lat; do
    ./latc_x86_64 "$file" &>/dev/null
    
    if [ $? -eq 0 ]; then
        echo "FAIL $file"
    fi
done

