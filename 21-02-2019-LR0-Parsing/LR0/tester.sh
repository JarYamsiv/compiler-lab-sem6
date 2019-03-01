#!/bin/bash
echo ""

TESTS_FILES=(test1)

for p in ${TESTS_FILES[*]}
do
	printf "\n===========================\n"
	printf "doing test : %s \n" ${p}
	./ec ${p}.txt
	printf "\n===========================\n"

done