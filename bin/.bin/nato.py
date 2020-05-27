#!/usr/bin/env python3

# Print the first argument (or stdin) using the NATO phonetic alphabet

import sys
import re

words = {
    "A": "Alpha",
    "B": "Bravo",
    "C": "Charlie",
    "D": "Delta",
    "E": "Echo",
    "F": "Foxtrot",
    "G": "Golf",
    "H": "Hotel",
    "I": "India",
    "J": "Juliet",
    "K": "Kilo",
    "L": "Lima",
    "M": "Mike",
    "N": "November",
    "O": "Oscar",
    "P": "Papa",
    "Q": "Quebec",
    "R": "Romeo",
    "S": "Sierra",
    "T": "Tango",
    "U": "Uniform",
    "V": "Victor",
    "W": "Whiskey",
    "X": "X-Ray",
    "Y": "Yankee",
    "Z": "Zulu",
    "0": "Zero",
    "1": "One",
    "2": "Two",
    "3": "Three",
    "4": "Four",
    "5": "Five",
    "6": "Six",
    "7": "Seven",
    "8": "Eight",
    "9": "Nine",
}


def nato_char(char):
    try:
        return words[char.upper()]
    except KeyError:
        return char


def nato(string):
    return re.sub(" +", " ",
                  " ".join([nato_char(char) for char in string]))


if __name__ == "__main__":
    if len(sys.argv) > 1:
        print(nato("".join(sys.argv[1:])))
        sys.exit(0)

    buffer = ''
    while True:
        buffer_len = len(buffer)
        buffer += sys.stdin.read(1)

        if len(buffer) == buffer_len:
            print(nato(buffer))
            sys.exit(0)

        if buffer.endswith('\n') or len(buffer) > 79:
            print(nato(buffer))
            buffer = ''
