CHAR_FOR_NEXT_SYMBOL = '^n'
NUMBERS = '1234567890'


def get_language():
    result = ''
    input_for_language = input()
    for char in range(0, len(input_for_language)):
        if input_for_language[char] == '^':
            result += input_for_language[char - 1]
    return result


def check_for_word():
    language = get_language()
    input_for_word = input()
    flag = True
    for elem in language:
        if elem not in input_for_word and elem not in NUMBERS:
            flag = False

    for elem in input_for_word and elem not in NUMBERS:
        if elem not in language:
            flag = False

    if flag:
        return 'yes'
    else:
        return 'no'


def main():
    print(check_for_word())


if __name__ == '__main__':
    main()
