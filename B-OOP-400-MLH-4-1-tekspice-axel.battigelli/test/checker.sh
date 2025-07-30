#!/bin/bash

# set -euxo pipefail

if [[ $# -ge 1 && $1 == "--bless" ]]; then
    is_blessing=true
else
    is_blessing=false
fi

if $is_blessing; then
    echo "WARN: Blessing files"
fi

total_tests=0
successful_tests=0

for script in `ls script`; do
    total_tests=$((total_tests + 1))
    testname="${script%.*}"
    echo Testing \'$testname\'...

    if ! [[ -f nts/$testname.nts ]]; then
        echo "ERROR: nts file nts/$testname.nts does not exist!"
        exit 1
    fi

    if $is_blessing; then
        ../nanotekspice nts/$testname.nts <script/$script >expected_output/$testname.log
    else
        if ! [[ -f expected_output/$testname.log ]]; then
            echo "ERROR: log file expected_output/$testname.log does not exist!"
            exit 1
        fi
        if [[ -f script_aux/${testname}_rom.bin ]]; then
            cp -f script_aux/${testname}_rom.bin ./rom.bin
        fi
        rm ./log.bin || true
        if git diff --exit-code --no-index expected_output/$testname.log <(../nanotekspice nts/$testname.nts <script/$script); then
            if [[ -f script_aux/${testname}_log.bin ]]; then
                if git diff --exit-code --no-index script_aux/${testname}_log.bin ./log.bin; then
                    successful_tests=$((successful_tests + 1))
                else
                    echo "ERROR: Test $testname failed!"
                fi
            else
                successful_tests=$((successful_tests + 1))
            fi
        else
            echo "ERROR: Test $testname failed!"
        fi
    fi
done

echo
echo "Advanced tests :"

list_command=(
    "../nanotekspice"
    "../nanotekspice --help"
    "../nanotekspice -h"
    "../nanotekspice nts/not_exist.nts"
    "../nanotekspice nts/missing_chipset1.nts"
    "../nanotekspice nts/missing_chipset2.nts"
    "../nanotekspice nts/missing_link1.nts"
    "../nanotekspice nts/missing_link2.nts"
    "../nanotekspice nts/infinity_loop.nts"
    "../nanotekspice nts/null_connection.nts"
    "../nanotekspice nts/undecided.nts"
    "../nanotekspice nts/unliked_pin.nts"
    "../nanotekspice nts/and.nts"
    "../nanotekspice nts/name_fail1.nts"
    "../nanotekspice nts/name_fail2.nts"
)

list_input=(
    "/dev/null"
    "/dev/null"
    "/dev/null"
    "/dev/null"
    "/dev/null"
    "/dev/null"
    "/dev/null"
    "/dev/null"
    "script_aux/scripts/infinity_loop_tests.txt"
    "script_aux/scripts/exception_tests.txt"
    "script_aux/scripts/exception_tests.txt"
    "script_aux/scripts/exception_tests.txt"
    "script_aux/scripts/exit_tests.txt"
    "/dev/null"
    "/dev/null"
)

list_status=(
    84
    0
    0
    84
    84
    84
    84
    84
    0
    84
    0
    84
    0
    84
    84
)

list_output=(
    "script_aux/empty_out.log"
    "script_aux/help_out.log"
    "script_aux/help_out.log"
    "script_aux/empty_out.log"
    "script_aux/empty_out.log"
    "script_aux/empty_out.log"
    "script_aux/empty_out.log"
    "script_aux/empty_out.log"
    "script_aux/infinity_loop_out.log"
    "script_aux/display1_out.log"
    "script_aux/display2_out.log"
    "script_aux/empty_out.log"
    "script_aux/exit_test_out.log"
    "script_aux/empty_out.log"
    "script_aux/empty_out.log"
)

list_err=(
    "script_aux/bad_usage_err.log"
    "script_aux/empty_err.log"
    "script_aux/empty_err.log"
    "script_aux/reading_stream_err.log"
    "script_aux/missing_chipsets_label_err.log"
    "script_aux/no_chipsets_err.log"
    "script_aux/missing_link_label_err.log"
    "script_aux/no_link_err.log"
    "script_aux/infinity_loop_err.log"
    "script_aux/unlincked_pin_err.log"
    "script_aux/empty_err.log"
    "script_aux/reading_stream_err.log"
    "script_aux/exit_test_err.log"
    "script_aux/no_name_err.log"
    "script_aux/bad_syntax_err.log"
)

for index in "${!list_command[@]}"; do
    command="${list_command[$index]}"
    input="${list_input[$index]}"
    expected_status="${list_status[$index]}"
    output="${list_output[$index]}"
    stderr="${list_err[$index]}"

    echo Testing \'$command \< $input\'...
    total_tests=$((total_tests + 1))

    set +e
    ${command} < ${input} >out.log 2>err.log
    status=$?
    set -e

    if $is_blessing; then
        cp out.log $command + "_out.log"
        cp err.log $command + "_err.log"
    else
        if ! [ $status -eq $expected_status ]; then
            echo "Failure: Command '$command' with input '$input' returned status: $status"
            continue
        fi
        if ! git diff --no-index --exit-code "${output}" out.log; then
            echo "Failure: Output does not match for command '$command' with input '$input'"
            continue
        fi
        if ! git diff --no-index --exit-code "${stderr}" err.log; then
            echo "Failure: Stderr does not match for command '$command' with input '$input'"
            continue
        fi

        successful_tests=$((successful_tests + 1))
    fi
done

echo
echo "$successful_tests out of $total_tests tests passed."
