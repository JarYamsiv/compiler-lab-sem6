#!/bin/bash
echo ""

TESTS_FILES=(test2)

for p in ${TESTS_FILES[*]}
do
	printf "\n===========================\n"
	printf "doing test : %s \n" ${p}
	./ec ${p}.txt
	printf "\n===========================\n"

done