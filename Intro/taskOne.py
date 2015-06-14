SYMBOL_FOR_NEXT_ROW = '/'


def convert_input(input_str):
    result = []
    temp = []
    for elem in input_str:
        if elem != ' ' and elem != SYMBOL_FOR_NEXT_ROW:
            temp.append(elem)
            if len(temp) == 2:
                result.append((temp[0], temp[1]))
                temp = []
    return result


def create_str_from_list(list_with_tuples):
    moment_char = list_with_tuples[0][1]
    flag = True
    result = list_with_tuples[0][0] + list_with_tuples[0][1]
    while flag:
        flag = False
        for elem in list_with_tuples:
            if elem[0] == moment_char:
                result += elem[1]
                moment_char = elem[1]
                flag = True
                break
    return result


def main():
    my_input = input()
    print(create_str_from_list(convert_input(my_input)))

if __name__ == '__main__':
    main()
