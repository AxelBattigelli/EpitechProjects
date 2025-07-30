#!/usr/bin/env python3
##
## EPITECH PROJECT, 2025
## zappy
## File description:
## main
##

import sys
import os
from typing import List, Dict, Any
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from AI.include.Player import Player
import traceback

def print_usage() -> None:
    print("Usage: zappy_ai -p <port> -n <name> -h <host>")
    print("Options:")
    print("  -p <port>   Specify the port number")
    print("  -n <name>   Specify the player name")
    print("  -h <host>   Specify the host address")

def retrieve_args(args: List[str]) -> Dict[str, Any]:
    options: Dict[str, Any] = {"host": "localhost", "debug": False}

    try:
        if "--help" in args:
            print_usage()
            exit(0)
        if "-d" in args:
            options['debug'] = True
            args.remove("-d")
        for i in range(1, len(args), 2):
            if args[i] == "-p":
                if i + 1 < len(args):
                    options['port'] = int(args[i + 1])
                else:
                    raise ValueError("Missing port argument")
            elif args[i] == "-n":
                if i + 1 < len(args):
                    options['name'] = args[i + 1]
                else:
                    raise ValueError("Missing name argument")
            elif args[i] == "-h":
                if i + 1 < len(args):
                    options['host'] = args[i + 1]
                else:
                    raise ValueError("Missing host argument")
            else:
                raise ValueError(f"Unknown argument {args[i]}")
        if 'port' not in options or 'name' not in options:
            raise ValueError("Both port (-p) and name (-n) must be specified")
    except ValueError as e:
        print(f"zappy_ai: {e}")
        print("Try using ./zappy_ai --help.")
        exit(84)
    return options

def main() -> None:
    try:
        options: Dict[str, Any] = retrieve_args(sys.argv)
        if options.get("debug", False):
            print(f"Debug mode is ON. Options: {options}")
        player: Player = Player(options["name"], 10, options["host"], options["port"], options["debug"])

        player.start()

    except Exception as e:
        print(f"zappy_ai: something went wrong during process: {e}")
        print(traceback.format_exc())
        exit(84)

if __name__ == "__main__":
    main()
