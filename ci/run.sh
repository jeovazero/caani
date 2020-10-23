#!/bin/sh

testFailure() {
  [ $? -ne 0 ] && exit 1
}

echo -e "----Running the Hlint----\n"

hlint .

testFailure

echo -e "\n\n----Building the artifact----\n"

nix-build artifact.nix --arg isDynamic true

testFailure

echo -e "\n\n----Running the CLI----\n"

A=`echo "main = pure ()" | ./result/caani/caani -o foo.png`

echo -e "\n\n----Testing the output----\n"

echo "$A" | grep "The image was saved in 'foo.png'"

if [ $? -eq 0 ];
then
    echo -e "\nSUCCESS!!!";
    exit 0;
else
    echo -e "\nFAILURE!!!";
		exit 1;
fi