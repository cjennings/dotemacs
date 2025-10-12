#!/usr/bin/env bash

# Create base directories
mkdir -p test-dir1/level1/level2/level3
mkdir -p test-dir2/level1/level2/level3

# Create identical files in both directories
echo "same content" > test-dir1/level1/same1.txt
echo "same content" > test-dir2/level1/same1.txt

echo "identical file" > test-dir1/level1/level2/same2.txt
echo "identical file" > test-dir2/level1/level2/same2.txt

echo "matching" > test-dir1/level1/level2/level3/same3.txt
echo "matching" > test-dir2/level1/level2/level3/same3.txt

# Create unique files in test-dir1
echo "unique to test-dir1" > test-dir1/unique1.txt
echo "only in test-dir1" > test-dir1/level1/unique2.txt
echo "test-dir1 only" > test-dir1/level1/level2/unique3.txt
echo "test-dir1 specific" > test-dir1/level1/level2/level3/unique4.txt

# Create unique files in test-dir2
echo "unique to test-dir2" > test-dir2/unique5.txt
echo "only in test-dir2" > test-dir2/level1/unique6.txt
echo "test-dir2 only" > test-dir2/level1/level2/unique7.txt
echo "test-dir2 specific" > test-dir2/level1/level2/level3/unique8.txt
