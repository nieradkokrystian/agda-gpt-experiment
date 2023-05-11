
#!/bin/sh

# ######################## VARIABLES #############################
#  Note that there should be no spaces around the "=" sign,   ####
#  and the variable name should not have any spaces either.   ####

FILE_NAME=W1.agda

FUNCTION_TYPE="length : {A : Set} → List A → Nat"

OPERATION_MODE="Debug"  # eg Pretty or Debug

MAX_TURNS=3

#CONFIG_PATH="/home/kryn/tt/agda-gpt-experiment/agda-gpt-experiment/data/config/config1.json"

CONFIG_PATH="./config1.json"

# ##################################################################


if [ $1 == 1  ]; then
    stack build --copy-bins
else
    echo "Haskell binary file without changes"
fi

cd /home/kryn/.local/bin

./agda-gpt-experiment-exe $FILE_NAME  "$FUNCTION_TYPE"  $OPERATION_MODE  $MAX_TURNS $CONFIG_PATH
